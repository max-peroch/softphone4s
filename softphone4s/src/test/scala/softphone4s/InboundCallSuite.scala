package softphone4s

import cats.data.NonEmptyList
import cats.effect.{Deferred, IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.config.{CodecChoice, SoftphoneConfig}
import softphone4s.media.{AudioCodec, HalfByteCodec}
import softphone4s.model.*
import fs2.io.net.Network
import softphone4s.codec.SipCodec
import softphone4s.transport.{Inbound, SipTransport, StreamFraming, Transport}

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** Incoming calls over real loopback UDP: a bare transport plays the caller
  * against a `SoftphoneStreaming`.
  */
class InboundCallSuite extends CatsEffectSuite {

  private val ours   = port"19080"
  private val theirs = port"19081"
  private val target = SocketAddress(ip"127.0.0.1", ours)

  private def config(answerTimeout: FiniteDuration) =
    SoftphoneConfig(
      user = "test",
      password = "test",
      realm = "127.0.0.1",
      localSipPort = ours,
      serverPort = theirs,
      baseRtpPort = port"19200",
      bindAddress = ip"127.0.0.1",
      hangupTimeout = 2.seconds,
      answerTimeout = answerTimeout
    )

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  /** The softphone under test, and the caller's end of the wire. */
  private def scenario(
      answerTimeout: FiniteDuration = 60.seconds,
      codecs: List[CodecChoice] = List(CodecChoice.Pcmu, CodecChoice.Pcma),
      extraCodecs: List[AudioCodec] = Nil
  ): Resource[IO, (SoftphoneStreaming[IO], Transport[IO])] =
    (
      SoftphoneStreaming.resource[IO](
        config(answerTimeout).copy(codecs = codecs),
        audio,
        NoOpLogger.impl[IO],
        extraCodecs
      ),
      SipTransport.udp[IO](
        SocketAddress(ip"127.0.0.1", theirs),
        NoOpLogger.impl[IO]
      )
    ).tupled

  private val offer =
    "v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\n" +
      "m=audio 40000 RTP/AVP 0 101\r\na=rtpmap:0 PCMU/8000\r\n" +
      "a=rtpmap:101 telephone-event/8000\r\n"

  private def invite(body: String = offer): SipRequest =
    SipRequest(
      SipMethod.Invite,
      uri("sip:test@127.0.0.1:19080"),
      SipHeaders(
        callId = callId("caller@host"),
        from = nameAddr("\"Bob\" <sip:bob@127.0.0.1>;tag=ft"),
        to = nameAddr("<sip:test@127.0.0.1>"),
        vias = NonEmptyList.one(
          via("SIP/2.0/UDP 127.0.0.1:19081;branch=z9hG4bKinvite1")
        ),
        cseq = cseq(1, SipMethod.Invite),
        contacts = List(nameAddr("<sip:bob@127.0.0.1:19081>")),
        contentType = Some(MediaType.Sdp)
      ),
      body
    )

  private def inDialog(
      method: SipMethod,
      n: Int,
      ok: SipResponse,
      body: String = ""
  ): SipRequest =
    SipRequest(
      method,
      uri("sip:test@127.0.0.1:19080"),
      SipHeaders(
        callId = callId("caller@host"),
        from = nameAddr("<sip:bob@127.0.0.1>;tag=ft"),
        to = ok.headers.to,
        vias = NonEmptyList.one(
          via(s"SIP/2.0/UDP 127.0.0.1:19081;branch=z9hG4bK${method}$n")
        ),
        cseq = cseq(n, method)
      ),
      body
    )

  private def send(peer: Transport[IO], message: SipMessage) =
    peer.send(target, message)

  private def response(peer: Transport[IO], wanted: SipResponse => Boolean) =
    peer.messages
      .collectFirst { case Inbound(_, r: SipResponse) if wanted(r) => r }
      .compile
      .lastOrError
      .timeout(5.seconds)

  private def request(peer: Transport[IO], method: SipMethod) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest) if r.method == method => r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  private def status(code: Int)(r: SipResponse) = r.statusCode == code

  private def firstCall(phone: SoftphoneStreaming[IO]) =
    phone.incoming.head.compile.lastOrError.timeout(5.seconds)

  /** The INVITE is sent and the call taken from the softphone. */
  private def ring(
      phone: SoftphoneStreaming[IO],
      peer: Transport[IO],
      i: SipRequest = invite()
  ) =
    send(peer, i) >> firstCall(phone)

  // ---- Answering ------------------------------------------------------------------

  test(
    "an incoming call is announced with its caller, rings, and is accepted"
  ) {
    scenario().use { (phone, peer) =>
      for {
        call    <- ring(phone, peer)
        _       <- call.ringing
        ringing <- response(peer, status(180))
        ok      <- call.accept.allocated.flatMap { case (_, release) =>
          response(peer, status(200)).flatTap(_ => release.start.void)
        }
      } yield {
        assertEquals(call.caller.displayName, Some("Bob"))
        assertEquals(call.caller.uri.render, "sip:bob@127.0.0.1")
        assertEquals(call.callee.uri.render, "sip:test@127.0.0.1")
        assertEquals(ringing.headers.to.tag, ok.headers.to.tag)
        assert(ok.headers.to.tag.isDefined)
        assert(ok.body.contains("m=audio"), ok.body)
        assert(ok.body.contains("RTP/AVP 0 101"), ok.body)
        assertEquals(ok.headers.contentType, Some(MediaType.Sdp))
        assertEquals(ok.headers.contacts.size, 1)
      }
    }
  }

  test(
    "the call is established by the caller's ACK, and ends with the caller's BYE"
  ) {
    scenario().use { (phone, peer) =>
      for {
        call     <- ring(phone, peer)
        accepted <- call.accept.allocated
        (established, _) = accepted
        ok    <- response(peer, status(200))
        _     <- send(peer, inDialog(SipMethod.Ack, 1, ok))
        _     <- established.awaitPickup.timeout(5.seconds)
        _     <- send(peer, inDialog(SipMethod.Bye, 2, ok))
        _     <- established.awaitHangup.timeout(5.seconds)
        byeOk <- response(peer, r => r.headers.cseq.method == SipMethod.Bye)
        ended <- established.source.compile.drain.attempt
      } yield {
        assertEquals(byeOk.statusCode, 200)
        assertEquals(ended, Left(softphone4s.model.SoftphoneError.RemoteHangup))
      }
    }
  }

  test("the 200 OK is repeated until the ACK arrives, then no more") {
    scenario().use { (phone, peer) =>
      for {
        call     <- ring(phone, peer)
        accepted <- call.accept.allocated
        first    <- response(peer, status(200))
        again    <- response(peer, status(200)) // after about 0.5 s, unACKed
        _        <- send(peer, inDialog(SipMethod.Ack, 1, first))
        _        <- accepted._1.awaitPickup.timeout(5.seconds)
        quiet    <- peer.messages
          .collectFirst {
            case Inbound(_, r: SipResponse) if r.statusCode == 200 => r
          }
          .compile
          .last
          .timeoutTo(1500.millis, IO.pure(None))
      } yield {
        assertEquals(again.headers.cseq, first.headers.cseq)
        assertEquals(quiet, None)
      }
    }
  }

  test("an INFO with a digit is answered and reaches dtmfEvents") {
    scenario().use { (phone, peer) =>
      for {
        call     <- ring(phone, peer)
        accepted <- call.accept.allocated
        ok       <- response(peer, status(200))
        _        <- send(peer, inDialog(SipMethod.Ack, 1, ok))
        _        <- accepted._1.awaitPickup.timeout(5.seconds)
        _        <- send(
          peer,
          inDialog(SipMethod.Info, 2, ok, "Signal=5\r\nDuration=160\r\n")
        )
        digit <- accepted._1.dtmfEvents.head.compile.lastOrError
          .timeout(5.seconds)
        infoOk <- response(peer, r => r.headers.cseq.method == SipMethod.Info)
      } yield {
        assertEquals(digit, DtmfDigit.D5)
        assertEquals(infoOk.statusCode, 200)
      }
    }
  }

  test("hanging up an answered call sends a BYE to the caller") {
    scenario().use { (phone, peer) =>
      for {
        call     <- ring(phone, peer)
        accepted <- call.accept.allocated
        ok       <- response(peer, status(200))
        _        <- send(peer, inDialog(SipMethod.Ack, 1, ok))
        _        <- accepted._1.awaitPickup.timeout(5.seconds)
        release  <- accepted._2.start
        bye      <- request(peer, SipMethod.Bye)
        _        <- send(peer, SipResponse(200, "OK", bye.headers))
        _        <- release.join
      } yield {
        assertEquals(bye.headers.callId, invite().headers.callId)
        assertEquals(bye.headers.from.tag, ok.headers.to.tag)
        assertEquals(bye.headers.to.tag.map(_.value), Some("ft"))
      }
    }
  }

  // ---- Turning calls away ---------------------------------------------------------------

  test("a rejected call gets the status") {
    scenario().use { (phone, peer) =>
      for {
        call <- ring(phone, peer)
        _    <- call.reject(FailureStatus.BusyHere)
        busy <- response(peer, status(486))
      } yield assertEquals(busy.headers.to.tag.isDefined, true)
    }
  }

  test("a call nobody answers is turned away with 480") {
    scenario(answerTimeout = 400.millis).use { (phone, peer) =>
      for {
        _ <- ring(phone, peer)
        r <- response(peer, status(480))
      } yield assertEquals(r.reason, "Temporarily Unavailable")
    }
  }

  test(
    "a CANCEL ends the call: 200 for the CANCEL, 487 for the INVITE, and cancelled completes"
  ) {
    scenario().use { (phone, peer) =>
      val cancel = invite().copy(
        method = SipMethod.Cancel,
        headers = invite().headers
          .copy(cseq = cseq(1, SipMethod.Cancel), contentType = None),
        body = ""
      )
      for {
        call     <- ring(phone, peer)
        _        <- send(peer, cancel)
        cancelOk <- response(
          peer,
          r => r.headers.cseq.method == SipMethod.Cancel && r.statusCode == 200
        )
        terminated <- response(peer, status(487))
        _          <- call.cancelled.timeout(5.seconds)
      } yield {
        assertEquals(cancelOk.statusCode, 200)
        assertEquals(terminated.headers.cseq.method, SipMethod.Invite)
      }
    }
  }

  test(
    "a codec the program added is answered with the payload type the caller gave it"
  ) {
    val dynamic = offer
      .replace("RTP/AVP 0 101", "RTP/AVP 111 0 101")
      .replace(
        "a=rtpmap:0 PCMU/8000\r\n",
        "a=rtpmap:0 PCMU/8000\r\na=rtpmap:111 x-half/8000\r\na=fmtp:111 mode=1\r\n"
      )
    scenario(
      codecs = List(CodecChoice.Named("x-half"), CodecChoice.Pcmu),
      extraCodecs = List(HalfByteCodec)
    ).use { (phone, peer) =>
      for {
        _    <- send(peer, invite(dynamic))
        call <- phone.incoming.head.compile.lastOrError.timeout(5.seconds)
        ok   <- call.accept.allocated.flatMap { case (_, release) =>
          response(peer, status(200)).flatTap(_ => release.start.void)
        }
      } yield {
        assert(ok.body.contains("RTP/AVP 111 101"), ok.body)
        assert(ok.body.contains("a=rtpmap:111 x-half/8000"), ok.body)
        assert(ok.body.contains("a=fmtp:111 mode=1"), ok.body)
        assert(!ok.body.contains("PCMU"), ok.body)
      }
    }
  }

  test("an offer of PCMA alone is answered with PCMA") {
    scenario().use { (phone, peer) =>
      for {
        _ <- send(peer, invite(offer.replace("RTP/AVP 0 101", "RTP/AVP 8 101")))
        call <- phone.incoming.head.compile.lastOrError.timeout(5.seconds)
        ok   <- call.accept.allocated.flatMap { case (_, release) =>
          response(peer, status(200)).flatTap(_ => release.start.void)
        }
      } yield {
        assert(ok.body.contains("RTP/AVP 8 101"), ok.body)
        assert(ok.body.contains("a=rtpmap:8 PCMA/8000"), ok.body)
        assert(!ok.body.contains("PCMU"), ok.body)
      }
    }
  }

  test(
    "an INVITE we cannot answer is refused with 488 and never reaches the application"
  ) {
    scenario().use { (phone, peer) =>
      for {
        _ <- send(peer, invite(offer.replace("RTP/AVP 0 101", "RTP/AVP 18")))
        refused   <- response(peer, status(488))
        announced <- phone.incoming.head.compile.last
          .timeoutTo(500.millis, IO.pure(None))
      } yield {
        assertEquals(refused.statusCode, 488)
        assertEquals(announced, None)
      }
    }
  }

  test("a request for a call that does not exist is answered 481") {
    scenario().use { (_, peer) =>
      val stray = inDialog(
        SipMethod.Bye,
        5,
        SipResponse(
          200,
          "OK",
          invite().headers.copy(to = nameAddr("<sip:test@127.0.0.1>;tag=zz"))
        )
      )
      for {
        _ <- send(peer, stray)
        r <- response(peer, status(481))
      } yield assertEquals(r.headers.cseq.method, SipMethod.Bye)
    }
  }

  test(
    "our BYE goes to the first Record-Route hop with the route set as received, not to the Contact"
  ) {
    scenario().use { (phone, peer) =>
      // The Contact points nowhere; only the route leads to the peer.
      val routed = invite().copy(headers =
        invite().headers.copy(
          contacts = List(nameAddr("<sip:bob@127.0.0.1:19999>")),
          recordRoutes = List(
            nameAddr("<sip:127.0.0.1:19081;lr>"),
            nameAddr("<sip:p2.example.com;lr>")
          )
        )
      )
      for {
        call     <- ring(phone, peer, routed)
        accepted <- call.accept.allocated
        ok       <- response(peer, status(200))
        _        <- send(peer, inDialog(SipMethod.Ack, 1, ok))
        _        <- accepted._1.awaitPickup.timeout(5.seconds)
        release  <- accepted._2.start
        bye      <- request(peer, SipMethod.Bye)
        _        <- send(peer, SipResponse(200, "OK", bye.headers))
        _        <- release.join
      } yield {
        assertEquals(bye.requestUri.render, "sip:bob@127.0.0.1:19999")
        assertEquals(
          bye.headers.routes.map(_.render),
          List("<sip:127.0.0.1:19081;lr>", "<sip:p2.example.com;lr>")
        )
        assertEquals(
          ok.headers.recordRoutes.map(_.render),
          routed.headers.recordRoutes.map(_.render)
        )
      }
    }
  }

  test(
    "a BYE with our Call-ID but the wrong To tag is answered 481 and the call goes on"
  ) {
    scenario().use { (phone, peer) =>
      for {
        call     <- ring(phone, peer)
        accepted <- call.accept.allocated
        ok       <- response(peer, status(200))
        _        <- send(peer, inDialog(SipMethod.Ack, 1, ok))
        _        <- accepted._1.awaitPickup.timeout(5.seconds)
        wrongTag = inDialog(SipMethod.Bye, 2, ok)
        _ <- send(
          peer,
          wrongTag.copy(headers =
            wrongTag.headers.copy(to = nameAddr("<sip:test@127.0.0.1>;tag=zz"))
          )
        )
        refused <- response(peer, r => r.headers.cseq.method == SipMethod.Bye)
        ended   <- accepted._1.awaitHangup.timeout(300.millis).attempt
      } yield {
        assertEquals(refused.statusCode, 481)
        assert(ended.isLeft, "the call must not have ended")
      }
    }
  }

  test(
    "a re-INVITE is answered with our SDP, a hold is mirrored, and OPTIONS lists our methods"
  ) {
    scenario().use { (phone, peer) =>
      def sdp(direction: String) =
        s"v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\nm=audio 19300 RTP/AVP 0\r\na=$direction\r\n"
      for {
        call     <- ring(phone, peer)
        accepted <- call.accept.allocated
        ok       <- response(peer, status(200))
        _        <- send(peer, inDialog(SipMethod.Ack, 1, ok))
        _        <- accepted._1.awaitPickup.timeout(5.seconds)
        _    <- send(peer, inDialog(SipMethod.Invite, 2, ok, sdp("sendonly")))
        held <- response(
          peer,
          r =>
            r.statusCode >= 200 && r.headers.cseq.method == SipMethod.Invite && r.headers.cseq.seq.value == 2
        )
        _       <- send(peer, inDialog(SipMethod.Options, 3, ok))
        options <- response(
          peer,
          r => r.headers.cseq.method == SipMethod.Options
        )
        _ <- accepted._2
      } yield {
        assertEquals(held.statusCode, 200)
        assertEquals(held.headers.contentType, Some(MediaType.Sdp))
        assert(held.body.contains("a=recvonly"), held.body)
        assertEquals(options.statusCode, 200)
        assert(
          options.headers.others.exists(h =>
            h.name.matches("Allow") && h.value.contains("UPDATE")
          )
        )
      }
    }
  }

  test("holding an incoming call sends a sendonly re-INVITE to the caller") {
    scenario().use { (phone, peer) =>
      for {
        call     <- ring(phone, peer)
        accepted <- call.accept.allocated
        ok       <- response(peer, status(200))
        _        <- send(peer, inDialog(SipMethod.Ack, 1, ok))
        _        <- accepted._1.awaitPickup.timeout(5.seconds)
        _        <- accepted._1.hold
        reinvite <- request(peer, SipMethod.Invite)
        answer = SipResponse(
          200,
          "OK",
          reinvite.headers.copy(contentType = Some(MediaType.Sdp)),
          "v=0\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\nm=audio 19300 RTP/AVP 0\r\na=recvonly\r\n"
        )
        _   <- send(peer, answer)
        ack <- request(peer, SipMethod.Ack)
        _   <- accepted._2
      } yield {
        assert(reinvite.body.contains("a=sendonly"), reinvite.body)
        assertEquals(reinvite.headers.cseq.seq.value, 2)
        assertEquals(ack.headers.cseq.seq, reinvite.headers.cseq.seq)
      }
    }
  }

  // ---- Requests within a dialog that are too big for UDP -------------------------------

  /** Record-Route entries enough to make every request in the dialog big; the
    * first is the caller's own address, which is where the first hop is.
    */
  private def bigRoute: List[NameAddr] =
    nameAddr("<sip:127.0.0.1:19081;lr>") ::
      (1 to 59).toList.map(n => nameAddr(s"<sip:proxy$n.example.com;lr>"))

  /** A TCP server where the caller is, handing over the first request it gets.
    */
  private def tcpServer: Resource[IO, Deferred[IO, SipRequest]] =
    for {
      got   <- Resource.eval(Deferred[IO, SipRequest])
      bound <- Network[IO].bind(SocketAddress(ip"127.0.0.1", theirs))
      _     <- bound.accept
        .map(
          _.reads
            .through(StreamFraming.frames[IO]())
            .evalMap {
              case Right(bytes) =>
                SipCodec
                  .decode(bytes)
                  .toOption
                  .collect { case r: SipRequest => r }
                  .traverse_(got.complete)
              case Left(_) => IO.unit
            }
            .handleErrorWith(_ => fs2.Stream.empty)
        )
        .parJoinUnbounded
        .compile
        .drain
        .background
    } yield got

  private def routed: SipRequest =
    invite().copy(headers = invite().headers.copy(recordRoutes = bigRoute))

  /** An answered call that is then hung up. */
  private def hangUpAnswered(
      phone: SoftphoneStreaming[IO],
      peer: Transport[IO]
  ) =
    for {
      call     <- ring(phone, peer, routed)
      accepted <- call.accept.allocated
      ok       <- response(peer, status(200))
      _        <- send(peer, inDialog(SipMethod.Ack, 1, ok))
      _        <- accepted._1.awaitPickup.timeout(5.seconds)
      _        <- accepted._2.start
    } yield ()

  test("our BYE, too big for UDP, goes over TCP to the first hop") {
    tcpServer.flatMap(got => scenario().map(got -> _)).use { (got, scene) =>
      val (phone, peer) = scene
      hangUpAnswered(phone, peer) >> got.get.timeout(5.seconds).map { bye =>
        assertEquals(bye.method, SipMethod.Bye)
        assertEquals(bye.headers.topVia.transport, TransportProtocol.Tcp)
        assertEquals(bye.headers.routes.size, 60)
      }
    }
  }

  test(
    "when that TCP connection cannot be made the BYE comes over UDP instead"
  ) {
    scenario().use { (phone, peer) =>
      hangUpAnswered(phone, peer) >> request(peer, SipMethod.Bye).map { bye =>
        assertEquals(bye.headers.topVia.transport, TransportProtocol.Udp)
        assertEquals(bye.headers.routes.size, 60)
      }
    }
  }

  test(
    "a sips: INVITE over UDP is refused 403 and never offered to the application"
  ) {
    scenario().use { (phone, peer) =>
      val insecure =
        invite().copy(requestUri = uri("sips:test@127.0.0.1:19080"))
      for {
        _       <- send(peer, insecure)
        refusal <- response(peer, r => r.statusCode >= 200)
        offered <- phone.incoming.head.compile.last.timeout(400.millis).attempt
      } yield {
        assertEquals(refusal.statusCode, 403)
        assert(offered.isLeft, "the application must not see the call")
      }
    }
  }
}
