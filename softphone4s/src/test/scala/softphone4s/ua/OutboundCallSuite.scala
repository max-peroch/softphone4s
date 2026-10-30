package softphone4s.ua

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.{Queue, Random, Supervisor}
import com.comcast.ip4s.*
import fs2.Stream
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.DigestVerifier
import softphone4s.SipLiterals.*
import softphone4s.config.SoftphoneConfig
import softphone4s.fsm.CallTimer
import softphone4s.model.*
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transaction.*
import softphone4s.transport.{InMemoryTransport, Inbound, Transport}

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
      resolveWith: SipUri => IO[SocketAddress[IpAddress]] = _ => IO.pure(them)
  ): Resource[IO, Fixture] =
    for {
      resolved <- Resource.eval(Ref.of[IO, List[SipUri]](Nil))
      pair     <- Resource.eval(InMemoryTransport.pair[IO])
      layer    <- TransactionManager.resource(
        pair._1,
        Reliability.Unreliable,
        TransactionConfig(),
        NoOpLogger.impl[IO]
      )
      supervisor <- Supervisor[IO]
      random     <- Resource.eval(Random.scalaUtilRandom[IO])
      timers     <- Timers.resource[IO, CallTimer]
      media      <- Resource.eval(MediaSession.create[IO](160, localIp, rtp))
      input      <- Resource.eval(Queue.unbounded[IO, DialogInput[IO]])
      call       <- Resource.eval(
        OutboundCall.create(
          callId("test@call"),
          uri("sip:bob@test.local"),
          layer,
          them,
          uri => resolved.update(_ :+ uri) >> resolveWith(uri),
          endpointOn(rtp),
          supervisor,
          Entropy.fromRandom(random),
          media,
          timers,
          inviteTimeout,
          NoOpLogger.impl[IO]
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

  private def sdp = "v=0\r\nc=IN IP4 127.0.0.1\r\nm=audio 40000 RTP/AVP 0\r\n"

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
    s"v=0\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\nm=audio 40000 RTP/AVP 0\r\na=$direction\r\n"

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
}
