package softphone4s.ua

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.{Queue, Random, Supervisor}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.{Datagram, Network}
import fs2.Chunk
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.DigestVerifier
import softphone4s.SipLiterals.*
import softphone4s.codec.RtpCodec
import softphone4s.config.{RtpLatching, SoftphoneConfig}
import softphone4s.fsm.CallTimer
import softphone4s.media.{AudioCodecs, JitterConfig}
import softphone4s.model.*
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.sdp.{
  CryptoAttribute,
  LocalCrypto,
  MediaProtocol,
  SessionDescription
}
import softphone4s.srtp.{MasterKey, SrtpInbound, SrtpPolicy, SrtpSuite}
import softphone4s.transaction.*
import softphone4s.transport.{
  FlakyTransport,
  InMemoryTransport,
  Inbound,
  Transport
}

import scala.concurrent.duration.*

/** The call against a real transaction layer: the test is the remote party,
  * reading what the call sends and answering through the peer's transport.
  */
class OutboundCallSuite extends CatsEffectSuite {

  private val localIp    = ip"127.0.0.1"
  private val sipPort    = port"19060"
  private val us         = InMemoryTransport.AddressA
  private val them       = InMemoryTransport.AddressB
  private val testConfig =
    SoftphoneConfig(user = "test", password = "test", realm = "test.local")

  private def endpointOn(rtp: Port) = LocalEndpoint
    .from(testConfig, localIp, sipPort, rtp)
    .fold(err => fail(err.getMessage), identity)

  private final case class Fixture(
      call: OutboundCall[IO],
      peer: Transport[IO],
      resolved: Ref[IO, List[SipUri]]
  )

  /** A call, its transaction layer, and the peer's end of the wire. Requests
    * and strays the layer reports are handed to the call, as
    * `SoftphoneStreaming` does.
    */
  private def fixture(
      rtp: Port,
      inviteTimeout: FiniteDuration = 32.seconds,
      resolveWith: SipUri => IO[SocketAddress[IpAddress]] = _ => IO.pure(them),
      alternatives: List[Uplink] = Nil,
      streamPeer: SocketAddress[IpAddress] = them,
      oversize: Option[Uplink] = None,
      udpLimit: Int = 1300,
      extraHeaders: List[ExtensionHeader] = Nil,
      first: Option[Uplink] = None,
      reach: Transport[IO] => Transport[IO] = identity,
      transactions: TransactionConfig = TransactionConfig()
  ): Resource[IO, Fixture] =
    for {
      resolved <- Resource.eval(Ref.of[IO, List[SipUri]](Nil))
      pair     <- Resource.eval(InMemoryTransport.pair[IO])
      layer    <- TransactionManager.resource(
        reach(pair._1),
        transactions,
        NoOpLogger.impl[IO]
      )
      supervisor <- Supervisor[IO]
      random     <- Resource.eval(Random.scalaUtilRandom[IO])
      timers     <- Timers.resource[IO, CallTimer]
      media      <- Resource.eval(
        MediaSession.create[IO](
          localIp,
          rtp,
          AudioCodecs.Default,
          RtpLatching.Follow,
          JitterConfig.Default
        )
      )
      input <- Resource.eval(Queue.unbounded[IO, DialogInput[IO]])
      call  <- Resource.eval(
        OutboundCall.create(
          callId("test@call"),
          uri("sip:bob@test.local"),
          layer,
          first.getOrElse(Uplink(endpointOn(rtp), them)),
          alternatives,
          oversize,
          udpLimit,
          (transport, uri) =>
            resolved.update(_ :+ uri) >>
              (if transport == TransportProtocol.Tcp then IO.pure(streamPeer)
               else resolveWith(uri)),
          supervisor,
          Entropy.fromRandom(random),
          media,
          timers,
          inviteTimeout,
          NoOpLogger.impl[IO],
          extraHeaders
        )
      )
      _ <- Resource.make(
        call.runEventLoop(Stream.fromQueueUnterminated(input)).start
      )(_.cancel)
      _ <- Resource.make(
        layer.incoming
          .evalMap(t => input.offer(DialogInput.Request(t)))
          .compile
          .drain
          .start
      )(_.cancel)
      _ <- Resource.make(
        layer.unmatched
          .evalMap(i => input.offer(DialogInput.Unmatched(i)))
          .compile
          .drain
          .start
      )(_.cancel)
    } yield Fixture(call, pair._2, resolved)

  // ---- The peer's side ---------------------------------------------------------

  private def received(peer: Transport[IO], wanted: SipRequest => Boolean) =
    peer.messages
      .collectFirst { case Inbound(_, r: SipRequest) if wanted(r) => r }
      .compile
      .lastOrError
      .timeout(5.seconds)

  private def nextOf(peer: Transport[IO], method: SipMethod) =
    received(peer, _.method == method)

  private def sdp =
    "v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\nm=audio 41000 RTP/AVP 0\r\n"

  private def answerTo(
      request: SipRequest,
      code: Int,
      reason: String,
      withContact: Boolean = false,
      recordRoute: List[String] = Nil,
      toTag: String = "rtag"
  ): SipResponse = {
    val base =
      TransactionMessages.responseTo(request, code, reason, Some(tag(toTag)))
    if withContact then
      base.copy(
        headers = base.headers.copy(
          contacts = List(nameAddr("<sip:bob@127.0.0.1:5061>")),
          recordRoutes = recordRoute.map(nameAddr),
          contentType = Some(MediaType.Sdp)
        ),
        body = sdp
      )
    else base
  }

  private def reply(peer: Transport[IO], response: SipResponse) =
    peer.send(us, response)

  private def byeFromPeer(invite: SipRequest, ok: SipResponse): SipRequest =
    SipRequest(
      SipMethod.Bye,
      uri("sip:test@127.0.0.1:19060"),
      SipHeaders(
        callId = invite.headers.callId,
        from = ok.headers.to,
        to = invite.headers.from,
        vias = NonEmptyList.one(
          via("SIP/2.0/UDP 127.0.0.1:5061;branch=z9hG4bKpeerbye")
        ),
        cseq = cseq(1, SipMethod.Bye)
      )
    )

  /** Receives the INVITE, answers 200, and waits for the call to notice. */
  private def answered(f: Fixture): IO[(SipRequest, SipResponse)] =
    for {
      invite <- nextOf(f.peer, SipMethod.Invite)
      ok = answerTo(invite, 200, "OK", withContact = true)
      _ <- reply(f.peer, ok)
      _ <- f.call.awaitPickup
    } yield (invite, ok)

  // ---- An answered call ----------------------------------------------------------

  test("an answered call is ACKed, outside any transaction") {
    fixture(port"19000").use { f =>
      for {
        _   <- answered(f)
        ack <- nextOf(f.peer, SipMethod.Ack)
      } yield assertEquals(ack.headers.cseq.method, SipMethod.Ack)
    }
  }

  test("a retransmitted 200 is ACKed again") {
    fixture(port"19000").use { f =>
      for {
        answeredWith <- answered(f)
        (invite, ok) = answeredWith
        _     <- nextOf(f.peer, SipMethod.Ack)
        _     <- reply(f.peer, ok) // the ACK was "lost": the 200 comes again
        again <- nextOf(f.peer, SipMethod.Ack)
      } yield assertEquals(again.headers.callId, invite.headers.callId)
    }
  }

  // ---- A BYE from the remote party ---------------------------------------------

  private def remoteBye(f: Fixture) =
    for {
      answeredWith <- answered(f)
      (invite, ok) = answeredWith
      _ <- f.peer.send(us, byeFromPeer(invite, ok))
    } yield ()

  private def byeAnswer(peer: Transport[IO]) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipResponse)
            if r.headers.cseq.method == SipMethod.Bye =>
          r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  test("awaitHangup completes after remote BYE, which is answered 200") {
    fixture(port"19000").use { f =>
      for {
        _      <- remoteBye(f)
        _      <- f.call.awaitHangup
        answer <- byeAnswer(f.peer)
      } yield assertEquals(answer.statusCode, 200)
    }
  }

  test("source raises RemoteHangup after remote BYE") {
    fixture(port"19000").use { f =>
      for {
        _      <- remoteBye(f)
        _      <- f.call.awaitHangup
        result <- f.call.source.compile.drain.attempt
      } yield assertEquals(result, Left(SoftphoneError.RemoteHangup))
    }
  }

  test("dtmfEvents raises RemoteHangup after remote BYE") {
    fixture(port"19000").use { f =>
      for {
        _      <- remoteBye(f)
        _      <- f.call.awaitHangup
        result <- f.call.dtmfEvents.compile.drain.attempt
      } yield assertEquals(result, Left(SoftphoneError.RemoteHangup))
    }
  }

  // ---- Hanging up ------------------------------------------------------------------

  test(
    "hanging up an answered call sends a BYE and does not signal a remote hangup"
  ) {
    fixture(port"19000").use { f =>
      for {
        answeredWith <- answered(f)
        (invite, _) = answeredWith
        _       <- f.call.hangup
        bye     <- nextOf(f.peer, SipMethod.Bye)
        outcome <- f.call.awaitHangup.timeout(200.millis).attempt
      } yield {
        assertEquals(bye.headers.callId, invite.headers.callId)
        assert(
          outcome.isLeft,
          s"awaitHangup completed on a local hangup: $outcome"
        )
      }
    }
  }

  // ---- Before the answer ---------------------------------------------------------

  test("an unanswered call times out through the invite timer") {
    fixture(port"19002", inviteTimeout = 150.millis).use { f =>
      f.call.awaitPickup.attempt.map(result =>
        assertEquals(result, Left(SoftphoneError.CallTimedOut))
      )
    }
  }

  test(
    "a failure response rejects the call, and the transaction layer ACKs it"
  ) {
    fixture(port"19004").use { f =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(f.peer, answerTo(invite, 486, "Busy Here"))
        result <- f.call.awaitPickup.attempt
        ack    <- nextOf(f.peer, SipMethod.Ack)
      } yield {
        assertEquals(
          result,
          Left(SoftphoneError.CallRejected(486, "Busy Here"))
        )
        assertEquals(ack.headers.to.tag.map(_.value), Some("rtag"))
        assertEquals(ack.headers.topVia.branch, invite.headers.topVia.branch)
      }
    }
  }

  test("wrong credentials: one retry, then the call is rejected, not looped") {
    val challenge =
      AuthChallenge.parse("""Digest realm="test.local", nonce="n1"""")
    def unauthorized(invite: SipRequest): SipResponse = {
      val base = answerTo(invite, 401, "Unauthorized")
      base.copy(headers = base.headers.copy(wwwAuthenticate = List(challenge)))
    }
    fixture(port"19003").use { f =>
      for {
        first  <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(f.peer, unauthorized(first))
        _      <- nextOf(f.peer, SipMethod.Ack) // for the first 401
        retry  <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(f.peer, unauthorized(retry))
        result <- f.call.awaitPickup.attempt
      } yield {
        assertEquals(
          result,
          Left(SoftphoneError.CallRejected(401, "Unauthorized"))
        )
        assert(
          first.headers.others.isEmpty,
          "the first INVITE has no credentials"
        )
        assert(
          retry.headers.others.exists(_.name.matches("Authorization")),
          "the retry carries credentials"
        )
        assertNotEquals(
          retry.headers.topVia.branch,
          first.headers.topVia.branch
        )
        assertEquals(retry.headers.cseq.seq, first.headers.cseq.seq.next)
      }
    }
  }

  // ---- Cancelling ------------------------------------------------------------------

  test("hanging up before the answer sends a CANCEL on the INVITE's branch") {
    fixture(port"19005").use { f =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- f.call.hangup
        cancel <- nextOf(f.peer, SipMethod.Cancel)
        _      <- reply(f.peer, answerTo(cancel, 200, "OK"))
        _      <- reply(f.peer, answerTo(invite, 487, "Request Terminated"))
        ack    <- nextOf(f.peer, SipMethod.Ack)
      } yield {
        assertEquals(cancel.headers.topVia.branch, invite.headers.topVia.branch)
        assertEquals(cancel.headers.cseq.seq, invite.headers.cseq.seq)
        assertEquals(ack.headers.cseq.method, SipMethod.Ack)
      }
    }
  }

  test("a 200 that crosses the CANCEL is ACKed and ended with a BYE") {
    fixture(port"19006").use { f =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- f.call.hangup
        cancel <- nextOf(f.peer, SipMethod.Cancel)
        _      <- reply(f.peer, answerTo(invite, 200, "OK", withContact = true))
        _      <- reply(f.peer, answerTo(cancel, 200, "OK"))
        ack    <- nextOf(f.peer, SipMethod.Ack)
        bye    <- nextOf(f.peer, SipMethod.Bye)
      } yield {
        assertEquals(ack.headers.callId, invite.headers.callId)
        assertEquals(bye.headers.callId, invite.headers.callId)
        assertEquals(bye.headers.to.tag.map(_.value), Some("rtag"))
      }
    }
  }

  test("a 200 to the CANCEL alone does not make the call ACK or BYE anything") {
    fixture(port"19007").use { f =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- f.call.hangup
        cancel <- nextOf(f.peer, SipMethod.Cancel)
        _      <- reply(f.peer, answerTo(cancel, 200, "OK"))
        _      <- reply(f.peer, answerTo(invite, 487, "Request Terminated"))
        _      <- nextOf(
          f.peer,
          SipMethod.Ack
        ) // the transaction layer's, for the 487
        extra <- f.peer.messages
          .collectFirst {
            case Inbound(_, r: SipRequest) if r.method == SipMethod.Bye => r
          }
          .compile
          .last
          .timeoutTo(300.millis, IO.pure(None))
      } yield assertEquals(extra, None)
    }
  }

  private def unauthorizedWith(
      invite: SipRequest,
      challenge: String
  ): SipResponse = {
    val base = answerTo(invite, 401, "Unauthorized")
    base.copy(headers =
      base.headers.copy(wwwAuthenticate = List(AuthChallenge.parse(challenge)))
    )
  }

  test(
    "an INVITE challenged with SHA-256 is retried with a response the server can verify"
  ) {
    fixture(port"19008").use { f =>
      for {
        first <- nextOf(f.peer, SipMethod.Invite)
        _     <- reply(
          f.peer,
          unauthorizedWith(
            first,
            """Digest realm="test.local", nonce="n1", algorithm=SHA-256, qop="auth""""
          )
        )
        retry <- nextOf(f.peer, SipMethod.Invite)
      } yield assertEquals(
        DigestVerifier.verify(retry, "test").map(v => (v.valid, v.algorithm)),
        Some((true, "SHA-256"))
      )
    }
  }

  test("auth-int hashes the SDP offer the retried INVITE carries") {
    fixture(port"19009").use { f =>
      for {
        first <- nextOf(f.peer, SipMethod.Invite)
        _     <- reply(
          f.peer,
          unauthorizedWith(
            first,
            """Digest realm="test.local", nonce="n1", qop="auth-int""""
          )
        )
        retry <- nextOf(f.peer, SipMethod.Invite)
      } yield {
        assert(retry.body.nonEmpty)
        assertEquals(
          DigestVerifier.verify(retry, "test").map(_.valid),
          Some(true)
        )
      }
    }
  }

  // ---- Routing within the dialog ---------------------------------------------

  /** The call answered with a 200 that carries `recordRoute`. */
  private def answeredVia(f: Fixture, recordRoute: List[String]) =
    for {
      invite <- nextOf(f.peer, SipMethod.Invite)
      _      <- reply(
        f.peer,
        answerTo(
          invite,
          200,
          "OK",
          withContact = true,
          recordRoute = recordRoute
        )
      )
      _ <- f.call.awaitPickup
    } yield invite

  private def hosts(f: Fixture): IO[List[String]] =
    f.resolved.get.map(_.map(_.host.toString))

  test(
    "a loose route set is followed: Route headers, the Contact as request-URI, the first route as next hop"
  ) {
    fixture(port"19010").use { f =>
      for {
        _ <- answeredVia(
          f,
          List("<sip:p2.example.com;lr>", "<sip:p1.example.com;lr>")
        )
        ack   <- nextOf(f.peer, SipMethod.Ack)
        _     <- f.call.hangup
        bye   <- nextOf(f.peer, SipMethod.Bye)
        asked <- hosts(f)
      } yield {
        val routes = List("<sip:p1.example.com;lr>", "<sip:p2.example.com;lr>")
        List(ack, bye).foreach { request =>
          assertEquals(request.requestUri.render, "sip:bob@127.0.0.1:5061")
          assertEquals(request.headers.routes.map(_.render), routes)
        }
        assertEquals(asked, List("p1.example.com", "p1.example.com"))
      }
    }
  }

  test(
    "a strict router first rewrites the request-URI and appends the Contact to the routes"
  ) {
    fixture(port"19011").use { f =>
      for {
        _     <- answeredVia(f, List("<sip:strict.example.com>"))
        ack   <- nextOf(f.peer, SipMethod.Ack)
        asked <- hosts(f)
      } yield {
        assertEquals(ack.requestUri.render, "sip:strict.example.com")
        assertEquals(
          ack.headers.routes.map(_.render),
          List("<sip:bob@127.0.0.1:5061>")
        )
        assertEquals(asked, List("strict.example.com"))
      }
    }
  }

  test("without a route set the requests go to the Contact") {
    fixture(port"19012").use { f =>
      for {
        _     <- answeredVia(f, Nil)
        ack   <- nextOf(f.peer, SipMethod.Ack)
        asked <- hosts(f)
      } yield {
        assertEquals(ack.requestUri.render, "sip:bob@127.0.0.1:5061")
        assertEquals(ack.headers.routes, Nil)
        assertEquals(asked, List("127.0.0.1"))
      }
    }
  }

  test(
    "a next hop that cannot be resolved falls back to the server that was dialled"
  ) {
    fixture(
      port"19013",
      resolveWith = _ => IO.raiseError(new RuntimeException("no such host"))
    ).use { f =>
      for {
        _   <- answeredVia(f, List("<sip:p1.example.com;lr>"))
        ack <- nextOf(f.peer, SipMethod.Ack) // still arrives, at the fallback
      } yield assertEquals(
        ack.headers.routes.map(_.render),
        List("<sip:p1.example.com;lr>")
      )
    }
  }

  // ---- Requests from the remote party ----------------------------------------

  private def established(f: Fixture) =
    answered(f).flatMap { case (invite, ok) =>
      nextOf(f.peer, SipMethod.Ack).as((invite, ok))
    }

  private def statusOf(
      peer: Transport[IO],
      method: SipMethod,
      wanted: Int => Boolean = _ => true
  ) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipResponse)
            if r.headers.cseq.method == method && wanted(r.statusCode) =>
          r.statusCode
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  test("a BYE for another dialog is answered 481 and the call goes on") {
    fixture(port"19014").use { f =>
      for {
        established <- established(f)
        (invite, ok) = established
        stray        = byeFromPeer(invite, ok).copy(headers =
          byeFromPeer(invite, ok).headers
            .copy(from = nameAddr("<sip:bob@test.local>;tag=wrong"))
        )
        _      <- f.peer.send(us, stray)
        status <- statusOf(f.peer, SipMethod.Bye)
        ended  <- f.call.awaitHangup.timeout(300.millis).attempt
      } yield {
        assertEquals(status, 481)
        assert(ended.isLeft, "the call must not have ended")
      }
    }
  }

  test(
    "a request that is not newer than the last is answered 500; a newer one ends the call"
  ) {
    fixture(port"19015").use { f =>
      def info(n: Int, ok: SipResponse, invite: SipRequest) =
        byeFromPeer(invite, ok).copy(
          method = SipMethod.Info,
          headers = byeFromPeer(invite, ok).headers.copy(
            cseq = cseq(n, SipMethod.Info),
            vias = NonEmptyList.one(
              via(s"SIP/2.0/UDP 127.0.0.1:5061;branch=z9hG4bKinfo$n")
            )
          ),
          body = "Signal=5\r\nDuration=160\r\n"
        )
      for {
        established <- established(f)
        (invite, ok) = established
        _       <- f.peer.send(us, info(5, ok, invite))
        first   <- statusOf(f.peer, SipMethod.Info)
        _       <- f.peer.send(us, info(4, ok, invite)) // older than 5
        refused <- statusOf(f.peer, SipMethod.Info)
      } yield {
        assertEquals(first, 200)
        assertEquals(refused, 500)
      }
    }
  }

  // ---- Our own hold and resume -----------------------------------------------

  private def sdpAnswering(direction: String) =
    s"v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\nm=audio 41000 RTP/AVP 0\r\na=$direction\r\n"

  private def okTo(reinvite: SipRequest, direction: String): SipResponse = {
    val base = TransactionMessages.responseTo(reinvite, 200, "OK")
    base.copy(
      headers = base.headers.copy(
        contacts = List(nameAddr("<sip:bob@127.0.0.1:5061>")),
        contentType = Some(MediaType.Sdp)
      ),
      body = sdpAnswering(direction)
    )
  }

  test(
    "a hold sends a sendonly re-INVITE in the dialog, and the 200 is ACKed"
  ) {
    fixture(port"19016").use { f =>
      for {
        established <- established(f)
        (invite, ok) = established
        _        <- f.call.hold
        reinvite <- nextOf(f.peer, SipMethod.Invite)
        _        <- reply(f.peer, okTo(reinvite, "recvonly"))
        ack      <- nextOf(f.peer, SipMethod.Ack)
        _        <- f.call.resume
        resume   <- nextOf(f.peer, SipMethod.Invite)
      } yield {
        assertEquals(reinvite.headers.callId, invite.headers.callId)
        assertEquals(
          reinvite.headers.cseq.seq.value,
          invite.headers.cseq.seq.value + 1
        )
        assertEquals(reinvite.headers.to.tag, ok.headers.to.tag)
        assertEquals(reinvite.requestUri.render, "sip:bob@127.0.0.1:5061")
        assert(reinvite.body.contains("a=sendonly"), reinvite.body)
        assertEquals(ack.headers.cseq.seq, reinvite.headers.cseq.seq)
        assertEquals(
          resume.headers.cseq.seq.value,
          reinvite.headers.cseq.seq.value + 1
        )
        assert(resume.body.contains("a=sendrecv"), resume.body)
      }
    }
  }

  test("a 491 to the re-INVITE is retried after a delay, with a new CSeq") {
    fixture(port"19017").use { f =>
      for {
        _     <- established(f)
        _     <- f.call.hold
        first <- nextOf(f.peer, SipMethod.Invite)
        _     <- reply(
          f.peer,
          TransactionMessages.responseTo(first, 491, "Request Pending")
        )
        second <- nextOf(f.peer, SipMethod.Invite).timeout(6.seconds)
      } yield {
        assertEquals(
          second.headers.cseq.seq.value,
          first.headers.cseq.seq.value + 1
        )
        assertEquals(second.headers.callId, first.headers.callId)
        assert(second.body.contains("a=sendonly"), second.body)
      }
    }
  }

  test("the remote's re-INVITE while ours is pending is answered 491") {
    fixture(port"19018").use { f =>
      for {
        established <- established(f)
        (invite, ok) = established
        _ <- f.call.hold
        _ <- nextOf(f.peer, SipMethod.Invite)
        base   = byeFromPeer(invite, ok)
        theirs = base.copy(
          method = SipMethod.Invite,
          headers = base.headers.copy(
            cseq = cseq(7, SipMethod.Invite),
            vias = NonEmptyList.one(
              via("SIP/2.0/UDP 127.0.0.1:5061;branch=z9hG4bKglare")
            )
          ),
          body = sdpAnswering("sendrecv")
        )
        _       <- f.peer.send(us, theirs)
        refused <- statusOf(f.peer, SipMethod.Invite, _ >= 200)
      } yield assertEquals(refused, 491)
    }
  }

  // ---- A forked call -----------------------------------------------------------

  test(
    "the first branch to answer is the call; a later branch's 200 is ACKed and ended"
  ) {
    fixture(port"19019").use { f =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(f.peer, answerTo(invite, 180, "Ringing", toTag = "a"))
        _      <- reply(f.peer, answerTo(invite, 180, "Ringing", toTag = "b"))
        _      <- reply(
          f.peer,
          answerTo(invite, 200, "OK", withContact = true, toTag = "b")
        )
        _    <- f.call.awaitPickup
        ackB <- nextOf(f.peer, SipMethod.Ack)
        // Another branch answers too, after the call was taken.
        _ <- reply(
          f.peer,
          answerTo(invite, 200, "OK", withContact = true, toTag = "c")
        )
        ackC  <- nextOf(f.peer, SipMethod.Ack)
        byeC  <- nextOf(f.peer, SipMethod.Bye)
        _     <- reply(f.peer, TransactionMessages.responseTo(byeC, 200, "OK"))
        ended <- f.call.awaitHangup.timeout(300.millis).attempt
        // Hanging up ends the call with branch b, not c.
        _    <- f.call.hangup
        byeB <- nextOf(f.peer, SipMethod.Bye)
      } yield {
        assertEquals(ackB.headers.to.tag, Some(tag("b")))
        assertEquals(ackC.headers.to.tag, Some(tag("c")))
        assertEquals(byeC.headers.to.tag, Some(tag("c")))
        assert(ended.isLeft, "the call must not have ended")
        assertEquals(byeB.headers.to.tag, Some(tag("b")))
      }
    }
  }

  // ---- Session requests from the remote, and UPDATE from us -------------------------

  private def peerInvite(
      invite: SipRequest,
      ok: SipResponse,
      body: String,
      n: Int = 7
  ) = {
    val base = byeFromPeer(invite, ok)
    base.copy(
      method = SipMethod.Invite,
      headers = base.headers.copy(
        cseq = cseq(n, SipMethod.Invite),
        vias = NonEmptyList.one(
          via(s"SIP/2.0/UDP 127.0.0.1:5061;branch=z9hG4bKre$n")
        ),
        contacts = List(nameAddr("<sip:bob@127.0.0.1:5061>")),
        contentType = Option.when(body.nonEmpty)(MediaType.Sdp)
      ),
      body = body
    )
  }

  private def finalResponses(peer: Transport[IO], method: SipMethod) =
    peer.messages.collect {
      case Inbound(_, r: SipResponse)
          if r.statusCode >= 200 && r.headers.cseq.method == method =>
        r
    }

  test(
    "a re-INVITE without SDP is answered with our offer, and the answer in the ACK moves the media"
  ) {
    fixture(port"19020").use { f =>
      for {
        established <- established(f)
        (invite, ok) = established
        _      <- f.peer.send(us, peerInvite(invite, ok, ""))
        answer <- finalResponses(
          f.peer,
          SipMethod.Invite
        ).head.compile.lastOrError.timeout(5.seconds)
        ack = SipRequest(
          SipMethod.Ack,
          uri("sip:test@127.0.0.1:19060"),
          peerInvite(invite, ok, sdpAnswering("sendonly")).headers.copy(
            cseq = cseq(7, SipMethod.Ack),
            contentType = Some(MediaType.Sdp)
          ),
          sdpAnswering("sendonly")
        )
        _ <- f.peer.send(us, ack)
        // The ACK ended the repeats: nothing more arrives.
        more <- finalResponses(f.peer, SipMethod.Invite).head.compile.last
          .timeout(1500.millis)
          .attempt
      } yield {
        assertEquals(answer.statusCode, 200)
        assertEquals(answer.headers.contentType, Some(MediaType.Sdp))
        assert(answer.body.contains("m=audio"), answer.body)
        assert(more.isLeft, "the 200 must stop being repeated once ACKed")
      }
    }
  }

  test("the 200 to a re-INVITE is repeated until the ACK arrives") {
    fixture(port"19021").use { f =>
      for {
        established <- established(f)
        (invite, ok) = established
        _ <- f.peer.send(us, peerInvite(invite, ok, sdpAnswering("sendrecv")))
        seen <- finalResponses(f.peer, SipMethod.Invite)
          .take(3)
          .compile
          .toList
          .timeout(20.seconds)
      } yield {
        assertEquals(seen.size, 3) // the 200, then two repeats
        assertEquals(seen.map(_.statusCode).distinct, List(200))
      }
    }
  }

  test("a hold by UPDATE sends an UPDATE that is answered without an ACK") {
    fixture(port"19022").use { f =>
      for {
        established <- established(f)
        (invite, _) = established
        _ <- f.call.changeSession(softphone4s.fsm.SessionChange.HoldByUpdate)
        update <- nextOf(f.peer, SipMethod.Update)
        _      <- reply(f.peer, okTo(update, "recvonly"))
        ack    <- nextOf(f.peer, SipMethod.Ack).timeout(700.millis).attempt
      } yield {
        assertEquals(update.headers.callId, invite.headers.callId)
        assertEquals(
          update.headers.cseq.seq.value,
          invite.headers.cseq.seq.value + 1
        )
        assert(update.body.contains("a=sendonly"), update.body)
        assert(ack.isLeft, "an UPDATE's 200 is not ACKed")
      }
    }
  }

  // ---- Trying the next server --------------------------------------------------------

  private val unreachable = SocketAddress(ip"192.0.2.99", port"5060")

  /** `endpointOn(rtp)` over TCP, reaching `peer`. */
  private def tcpUplink(rtp: Port, peer: SocketAddress[IpAddress]): Uplink =
    Uplink(
      endpointOn(rtp)
        .via(localIp, TransportProtocol.Tcp)
        .fold(err => fail(err.getMessage), identity),
      peer
    )

  private def udpUplink(rtp: Port, peer: SocketAddress[IpAddress]) =
    Uplink(endpointOn(rtp), peer)

  test(
    "when the first server cannot be reached the INVITE goes to the next, over its transport"
  ) {
    val rtp = port"19023"
    fixture(
      rtp,
      first = Some(udpUplink(rtp, unreachable)),
      alternatives = List(tcpUplink(rtp, them)),
      reach = FlakyTransport(_, failing = Set(unreachable))
    ).use { f =>
      for {
        (invite, _) <- answered(f)
        ack         <- nextOf(f.peer, SipMethod.Ack)
        _           <- f.call.hangup
        bye         <- nextOf(f.peer, SipMethod.Bye)
        again <- nextOf(f.peer, SipMethod.Invite).timeout(300.millis).attempt
      } yield {
        assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
        assertEquals(
          invite.headers.contacts.flatMap(_.uri.transport),
          List(TransportProtocol.Tcp)
        )
        // The dialog is on the transport that worked, not the one first tried.
        assertEquals(ack.headers.topVia.transport, TransportProtocol.Tcp)
        assertEquals(bye.headers.topVia.transport, TransportProtocol.Tcp)
        assert(again.isLeft, "the INVITE must reach the peer once")
      }
    }
  }

  test(
    "a server that does not answer is given up on after the transaction's timeout"
  ) {
    val rtp = port"19024"
    fixture(
      rtp,
      inviteTimeout = 30.seconds,
      first = Some(udpUplink(rtp, unreachable)),
      alternatives = List(tcpUplink(rtp, them)),
      reach = FlakyTransport(_, silent = Set(unreachable)),
      transactions = TransactionConfig(t1 = 20.millis)
    ).use { f =>
      for {
        (invite, _) <- answered(f)
      } yield assertEquals(
        invite.headers.topVia.transport,
        TransportProtocol.Tcp
      )
    }
  }

  test("each alternative is tried in turn") {
    val rtp  = port"19025"
    val also = SocketAddress(ip"192.0.2.98", port"5060")
    fixture(
      rtp,
      first = Some(udpUplink(rtp, unreachable)),
      alternatives = List(udpUplink(rtp, also), tcpUplink(rtp, them)),
      reach = FlakyTransport(_, failing = Set(unreachable, also))
    ).use { f =>
      answered(f).map((invite, _) =>
        assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
      )
    }
  }

  test("with no server left the call fails as a transport failure") {
    val rtp = port"19026"
    fixture(
      rtp,
      first = Some(udpUplink(rtp, unreachable)),
      reach = FlakyTransport(_, failing = Set(unreachable))
    ).use(f => f.call.awaitPickup.attempt)
      .map(r => assertEquals(r, Left(SoftphoneError.TransportFailed)))
  }

  test("a server that answers, even to refuse, is not failed over from") {
    val rtp = port"19027"
    fixture(rtp, alternatives = List(tcpUplink(rtp, them))).use { f =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(f.peer, answerTo(invite, 486, "Busy Here"))
        result <- f.call.awaitPickup.attempt
        again  <- nextOf(f.peer, SipMethod.Invite).timeout(500.millis).attempt
      } yield {
        assertEquals(
          result,
          Left(SoftphoneError.CallRejected(486, "Busy Here"))
        )
        assert(
          again.isLeft,
          "a refusal is an answer; the next server is not asked"
        )
      }
    }
  }

  // ---- A request too big for UDP ------------------------------------------------------

  /** A header long enough to push the INVITE past a datagram. */
  private val bulky =
    List(
      ExtensionHeader(
        FieldName("X-Context").fold(fail(_), identity),
        "x" * 1500
      )
    )

  test(
    "an INVITE too big for UDP goes over TCP, with the Via and Contact of TCP"
  ) {
    val rtp = port"19028"
    fixture(rtp, oversize = Some(tcpUplink(rtp, them)), extraHeaders = bulky)
      .use { f =>
        for {
          (invite, _) <- answered(f)
          ack         <- nextOf(f.peer, SipMethod.Ack)
          _           <- f.call.hangup
          bye         <- nextOf(f.peer, SipMethod.Bye)
        } yield {
          assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
          assertEquals(
            invite.headers.contacts.flatMap(_.uri.transport),
            List(TransportProtocol.Tcp)
          )
          // The dialog runs on the transport the INVITE went over.
          assertEquals(ack.headers.topVia.transport, TransportProtocol.Tcp)
          assertEquals(bye.headers.topVia.transport, TransportProtocol.Tcp)
        }
      }
  }

  test("an INVITE that fits in a datagram stays on UDP") {
    val rtp = port"19029"
    fixture(rtp, oversize = Some(tcpUplink(rtp, them))).use { f =>
      answered(f).map((invite, _) =>
        assertEquals(invite.headers.topVia.transport, TransportProtocol.Udp)
      )
    }
  }

  test("the limit is the one given") {
    val rtp = port"19030"
    fixture(rtp, oversize = Some(tcpUplink(rtp, them)), udpLimit = 200).use {
      f =>
        answered(f).map((invite, _) =>
          assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
        )
    }
  }

  test("with no TCP way to the server a big INVITE still goes over UDP") {
    val rtp = port"19031"
    fixture(rtp, extraHeaders = bulky).use { f =>
      answered(f).map((invite, _) =>
        assertEquals(invite.headers.topVia.transport, TransportProtocol.Udp)
      )
    }
  }

  test("when TCP cannot be reached a big INVITE is sent again over UDP") {
    val rtp    = port"19032"
    val stream = tcpUplink(rtp, unreachable)
    fixture(
      rtp,
      oversize = Some(stream),
      extraHeaders = bulky,
      reach = FlakyTransport(_, failing = Set(unreachable))
    ).use { f =>
      answered(f).map((invite, _) =>
        assertEquals(invite.headers.topVia.transport, TransportProtocol.Udp)
      )
    }
  }

  test("a call to a name that is already over TCP is not switched") {
    val rtp = port"19033"
    fixture(
      rtp,
      first = Some(tcpUplink(rtp, them)),
      oversize = Some(udpUplink(rtp, unreachable)),
      extraHeaders = bulky,
      reach = FlakyTransport(_, failing = Set(unreachable))
    ).use { f =>
      answered(f).map((invite, _) =>
        assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
      )
    }
  }

  // ---- Requests within a dialog that are too big for UDP -------------------------------

  /** A route set long enough that every request in the dialog is a big one. */
  private val longRoute =
    (1 to 60).map(n => s"<sip:proxy$n.example.com;lr>").toList

  private def answeredWithRoute(f: Fixture) =
    for {
      invite <- nextOf(f.peer, SipMethod.Invite)
      _      <- reply(
        f.peer,
        answerTo(invite, 200, "OK", withContact = true, recordRoute = longRoute)
      )
      _ <- f.call.awaitPickup
    } yield invite

  test("the ACK and BYE of a dialog with a long route set go over TCP") {
    fixture(port"19034").use { f =>
      for {
        _   <- answeredWithRoute(f)
        ack <- nextOf(f.peer, SipMethod.Ack)
        _   <- f.call.hangup
        bye <- nextOf(f.peer, SipMethod.Bye)
      } yield {
        List(ack, bye).foreach { request =>
          assertEquals(request.headers.topVia.transport, TransportProtocol.Tcp)
          assertEquals(
            request.headers.contacts.flatMap(_.uri.transport),
            List(TransportProtocol.Tcp)
          )
          assertEquals(request.headers.routes.size, 60)
        }
      }
    }
  }

  test(
    "a big BYE whose TCP connection cannot be made is sent again over UDP, once"
  ) {
    val rtp = port"19035"
    fixture(
      rtp,
      streamPeer = unreachable,
      reach = FlakyTransport(_, failing = Set(unreachable))
    ).use { f =>
      for {
        _     <- answeredWithRoute(f)
        _     <- f.call.hangup
        bye   <- nextOf(f.peer, SipMethod.Bye)
        again <- nextOf(f.peer, SipMethod.Bye).timeout(400.millis).attempt
      } yield {
        assertEquals(bye.headers.topVia.transport, TransportProtocol.Udp)
        assert(again.isLeft, "the BYE must arrive once")
      }
    }
  }

  test("a request within a dialog that fits in a datagram stays on UDP") {
    fixture(port"19036").use { f =>
      for {
        _   <- answered(f)
        ack <- nextOf(f.peer, SipMethod.Ack)
        _   <- f.call.hangup
        bye <- nextOf(f.peer, SipMethod.Bye)
      } yield {
        assertEquals(ack.headers.topVia.transport, TransportProtocol.Udp)
        assertEquals(bye.headers.topVia.transport, TransportProtocol.Udp)
      }
    }
  }

  // ---- Digits --------------------------------------------------------------------------

  private val sdpWithEvents =
    "v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\n" +
      "m=audio 41000 RTP/AVP 0 101\r\na=rtpmap:101 telephone-event/8000\r\n"

  private def infoOk(info: SipRequest) =
    TransactionMessages.responseTo(info, 200, "OK")

  test(
    "a digit goes in a SIP INFO when the other side did not agree to telephone events"
  ) {
    fixture(port"19037").use { f =>
      for {
        established <- established(f)
        (invite, _) = established
        method <- f.call.sendDtmf(DtmfDigit.D5, 200.millis)
        info   <- nextOf(f.peer, SipMethod.Info)
        _      <- reply(f.peer, infoOk(info))
        _      <- f.call.sendDtmf(DtmfDigit.Hash, 160.millis)
        second <- nextOf(f.peer, SipMethod.Info)
      } yield {
        assertEquals(method, DtmfMethod.SipInfo)
        assertEquals(info.headers.callId, invite.headers.callId)
        assertEquals(
          info.headers.cseq.seq.value,
          invite.headers.cseq.seq.value + 1
        )
        assertEquals(info.headers.contentType, Some(MediaType.DtmfRelay))
        assertEquals(info.body, "Signal=5\r\nDuration=200\r\n")
        assertEquals(second.body, "Signal=#\r\nDuration=160\r\n")
        assertEquals(
          second.headers.cseq.seq.value,
          invite.headers.cseq.seq.value + 2
        )
      }
    }
  }

  test(
    "a digit goes as telephone events, and in no INFO, when the other side agreed to them"
  ) {
    val rtpPeer = Network[IO].bindDatagramSocket(
      address = SocketAddress(ip"127.0.0.1", port"41000")
    )
    (fixture(port"19038"), rtpPeer).tupled.use { (f, media) =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(
          f.peer,
          answerTo(invite, 200, "OK", withContact = true)
            .copy(body = sdpWithEvents)
        )
        _ <- f.call.awaitPickup
        _ <- IO.sleep(500.millis) // the media socket is bound in the background
        method  <- f.call.sendDtmf(DtmfDigit.D9, 100.millis)
        packets <- media.reads.take(7).compile.toList.timeout(5.seconds)
        info    <- nextOf(f.peer, SipMethod.Info).timeout(300.millis).attempt
      } yield {
        assertEquals(method, DtmfMethod.Rfc4733)
        val events = packets.map(d => RtpCodec.decode(d.bytes).get)
        assertEquals(events.map(_._1.payloadType).distinct, List(pt(101)))
        assertEquals(
          events
            .flatMap(e => SoftphoneEvent.decode(e._2))
            .map(_.event)
            .distinct,
          List(9)
        )
        assert(info.isLeft, "no INFO for a digit sent in the media")
      }
    }
  }

  test("a digit cannot be sent before the call is answered") {
    fixture(port"19039").use { f =>
      f.call
        .sendDtmf(DtmfDigit.D1, 100.millis)
        .attempt
        .map(r => assertEquals(r, Left(SoftphoneError.CallNotEstablished)))
    }
  }

  test("a digit cannot last less than 20 ms or more than 8 s") {
    fixture(port"19040").use { f =>
      for {
        _     <- established(f)
        short <- f.call.sendDtmf(DtmfDigit.D1, 10.millis).attempt
        long  <- f.call.sendDtmf(DtmfDigit.D1, 9.seconds).attempt
      } yield {
        assertEquals(short, Left(SoftphoneError.InvalidDtmfDuration(10)))
        assertEquals(long, Left(SoftphoneError.InvalidDtmfDuration(9000)))
      }
    }
  }

  // ---- Early media -----------------------------------------------------------------------

  private def mediaPeer(port: Port) =
    Network[IO].bindDatagramSocket(address = SocketAddress(ip"127.0.0.1", port))

  private def sdpAt(port: Int) = sdp.replace("41000", port.toString)

  private val silenceOf0x80 = Chunk.array(Array.fill[Byte](160)(0x80.toByte))

  private def rtpTo(call: Port, sequence: Int) =
    Datagram(
      SocketAddress(ip"127.0.0.1", call),
      RtpCodec.encode(
        pt(0),
        sequence,
        sequence * 160L,
        Ssrc.fromBits(5),
        silenceOf0x80,
        marker = false
      )
    )

  private def progress(invite: SipRequest, body: String) =
    answerTo(invite, 183, "Session Progress", withContact = true)
      .copy(body = body)

  test(
    "what the other side plays before it answers is heard, and what we say is heard by it"
  ) {
    (fixture(port"19041"), mediaPeer(port"41000")).tupled.use { (f, media) =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(f.peer, progress(invite, sdp))
        _ <- IO.sleep(500.millis) // the media socket is bound in the background
        _ <- media.write(rtpTo(port"19041", 1))
        heard <- f.call.source.take(320).compile.to(Chunk).timeout(5.seconds)
        _     <- Stream
          .chunk(Chunk.array(new Array[Byte](320)))
          .through(f.call.sink)
          .compile
          .drain
        sent     <- media.reads.head.compile.lastOrError.timeout(5.seconds)
        pickedUp <- f.call.awaitPickup.timeout(200.millis).attempt
      } yield {
        // μ-law 0x80 is the loudest positive sample.
        assertEquals(
          heard.toArray.grouped(2).map(_.toList).toList.distinct.size,
          1
        )
        assertEquals(heard.size, 320)
        assertEquals(
          RtpCodec.decode(sent.bytes).map(_._1.payloadType),
          Some(pt(0))
        )
        assert(pickedUp.isLeft, "it was not answered yet")
      }
    }
  }

  test(
    "the 200 after early media carries on with the same media, pointed where it says"
  ) {
    (
      fixture(port"19042"),
      mediaPeer(port"41000"),
      mediaPeer(port"40002")
    ).tupled.use { (f, early, answering) =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(f.peer, progress(invite, sdp))
        _      <- IO.sleep(500.millis)
        _      <- reply(
          f.peer,
          answerTo(invite, 200, "OK", withContact = true)
            .copy(body = sdpAt(40002))
        )
        _ <- f.call.awaitPickup
        _ <- Stream
          .chunk(Chunk.array(new Array[Byte](320)))
          .through(f.call.sink)
          .compile
          .drain
        atAnswer <- answering.reads.head.compile.lastOrError.timeout(5.seconds)
        atEarly  <- early.reads.interruptAfter(300.millis).compile.toList
      } yield {
        assert(RtpCodec.decode(atAnswer.bytes).isDefined)
        assertEquals(atEarly, Nil)
      }
    }
  }

  test("a call refused after early media lets go of its media") {
    (fixture(port"19043"), mediaPeer(port"41000")).tupled.use { (f, _) =>
      def bindable(attempts: Int): IO[Boolean] =
        Network[IO]
          .bindDatagramSocket(address =
            SocketAddress(ip"127.0.0.1", port"19043")
          )
          .use_
          .as(true)
          .handleErrorWith(_ =>
            if attempts <= 0 then IO.pure(false)
            else IO.sleep(100.millis) >> bindable(attempts - 1)
          )
      for {
        invite  <- nextOf(f.peer, SipMethod.Invite)
        _       <- reply(f.peer, progress(invite, sdp))
        _       <- IO.sleep(500.millis)
        busy    <- bindable(0) // the media is holding the port
        _       <- reply(f.peer, answerTo(invite, 486, "Busy Here"))
        refused <- f.call.awaitPickup.attempt
        freed   <- bindable(30)
      } yield {
        assert(!busy, "the early media held its port")
        assert(refused.isLeft)
        assert(freed, "and let it go when the call was refused")
      }
    }
  }

  // ---- Encrypted media ---------------------------------------------------------------

  private def masterKey(n: Int): MasterKey =
    MasterKey(
      Chunk.array(Array.fill[Byte](16)(n.toByte)),
      Chunk.array(Array.fill[Byte](14)((n + 1).toByte))
    ).fold(e => fail(e.message), identity)

  private def secureUplink(rtp: Port, policy: SrtpPolicy) =
    Uplink(
      endpointOn(rtp).withCapabilities(
        AudioCodecs.Default.capabilities.copy(crypto =
          LocalCrypto(
            policy,
            List(
              CryptoAttribute(1, SrtpSuite.AesCm128HmacSha1_80, masterKey(10)),
              CryptoAttribute(2, SrtpSuite.AesCm128HmacSha1_32, masterKey(20))
            )
          )
        )
      ),
      them
    )

  private def answerSdp(protocol: String, lines: String*) =
    ("v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\n" +
      s"m=audio 41000 $protocol 0\r\n") + lines.map(_ + "\r\n").mkString

  test(
    "with encryption required the INVITE offers secure media with our keys, and what follows the answer is encrypted"
  ) {
    val rtpPeer = Network[IO].bindDatagramSocket(
      address = SocketAddress(ip"127.0.0.1", port"41000")
    )
    val theirs =
      CryptoAttribute(1, SrtpSuite.AesCm128HmacSha1_80, masterKey(30))
    (
      fixture(
        port"19039",
        first = Some(secureUplink(port"19039", SrtpPolicy.Required))
      ),
      rtpPeer
    ).tupled.use { (f, media) =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(
          f.peer,
          answerTo(invite, 200, "OK", withContact = true)
            .copy(body = answerSdp("RTP/SAVP", theirs.attribute.render))
        )
        _ <- f.call.awaitPickup
        _ <- IO.sleep(500.millis) // the media socket is bound in the background
        _ <- Stream
          .chunk(Chunk.array(Array.fill[Byte](320)(1)))
          .through(f.call.sink)
          .compile
          .drain
        datagram <- media.read.timeout(5.seconds)
      } yield {
        val offered = SessionDescription
          .parse(invite.body)
          .fold(e => fail(e.message), identity)
        assertEquals(offered.media.head.protocol, MediaProtocol.RtpSavp)
        assertEquals(
          offered.media.head.cryptos.map(c => (c.tag, c.suite)),
          List(
            (1, SrtpSuite.AesCm128HmacSha1_80),
            (2, SrtpSuite.AesCm128HmacSha1_32)
          )
        )
        // The packet is sealed with our key for the suite they chose, and
        // opens with it.
        assertEquals(datagram.bytes.size, 12 + 160 + 10)
        val opened = SrtpInbound(SrtpSuite.AesCm128HmacSha1_80, masterKey(10))
          .unprotect(datagram.bytes)
        assert(opened.isRight, opened.left.toOption.map(_.message))
      }
    }
  }

  test(
    "with encryption required, an answer in plain RTP ends the call, and nothing goes out in the clear"
  ) {
    val rtpPeer = Network[IO].bindDatagramSocket(
      address = SocketAddress(ip"127.0.0.1", port"41000")
    )
    (
      fixture(
        port"19041",
        first = Some(secureUplink(port"19041", SrtpPolicy.Required))
      ),
      rtpPeer
    ).tupled.use { (f, media) =>
      for {
        invite <- nextOf(f.peer, SipMethod.Invite)
        _      <- reply(
          f.peer,
          answerTo(invite, 200, "OK", withContact = true)
            .copy(body = answerSdp("RTP/AVP"))
        )
        // An answer we cannot use is a call we hang up.
        bye  <- nextOf(f.peer, SipMethod.Bye).timeout(5.seconds)
        sent <- media.read.timeout(500.millis).attempt
      } yield {
        assertEquals(bye.method, SipMethod.Bye)
        assert(sent.isLeft, "media went out in the clear")
      }
    }
  }
}
