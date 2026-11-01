package softphone4s

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.config.{CodecChoice, SoftphoneConfig}
import softphone4s.media.HalfByteCodec
import softphone4s.messaging.{MessageContent, MessageFailure}
import softphone4s.model.*
import softphone4s.transaction.TransactionMessages
import softphone4s.transport.{Inbound, SipTransport, Transport}

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** The whole stack over real loopback UDP: a `SoftphoneStreaming` places a call
  * to a bare transport that plays the remote party.
  */
class SoftphoneStreamingSuite extends CatsEffectSuite {

  private val ours   = port"19070"
  private val theirs = port"19071"

  private val config = SoftphoneConfig(
    user = "test",
    password = "test",
    realm = "127.0.0.1",
    localSipPort = ours,
    serverPort = theirs,
    baseRtpPort = port"19100",
    bindAddress = ip"127.0.0.1",
    hangupTimeout = 2.seconds
  )

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  private val phone =
    SoftphoneStreaming.resource[IO](config, audio, NoOpLogger.impl[IO])

  private val remote: cats.effect.Resource[IO, Transport[IO]] =
    SipTransport
      .udp[IO](SocketAddress(ip"127.0.0.1", theirs), NoOpLogger.impl[IO])

  private val callee = Callee("bob@127.0.0.1")
    .fold(err => fail(err), identity)

  private val us = SocketAddress(ip"127.0.0.1", ours)

  private def requestOf(peer: Transport[IO], method: SipMethod) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest) if r.method == method => r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  private def okFor(invite: SipRequest): SipResponse = {
    val base =
      TransactionMessages.responseTo(invite, 200, "OK", Some(tag("rtag")))
    base.copy(
      headers = base.headers.copy(
        contacts = List(nameAddr("<sip:bob@127.0.0.1:19071>")),
        contentType = Some(MediaType.Sdp)
      ),
      body = "v=0\r\nc=IN IP4 127.0.0.1\r\nm=audio 40000 RTP/AVP 0\r\n"
    )
  }

  private def byeFor(
      invite: SipRequest,
      ok: SipResponse,
      callId: CallId
  ): SipRequest =
    SipRequest(
      SipMethod.Bye,
      uri("sip:test@127.0.0.1:19070"),
      SipHeaders(
        callId = callId,
        from = ok.headers.to,
        to = invite.headers.from,
        vias = NonEmptyList.one(
          via("SIP/2.0/UDP 127.0.0.1:19071;branch=z9hG4bKbye1")
        ),
        cseq = cseq(1, SipMethod.Bye)
      )
    )

  test("a call over UDP is answered, ACKed, and ended by the remote BYE") {
    (phone, remote).tupled.use { (softphone, peer) =>
      softphone.call(callee).use { call =>
        for {
          invite <- requestOf(peer, SipMethod.Invite)
          ok = okFor(invite)
          _      <- peer.send(us, ok)
          _      <- call.awaitPickup.timeout(5.seconds)
          ack    <- requestOf(peer, SipMethod.Ack)
          _      <- peer.send(us, byeFor(invite, ok, invite.headers.callId))
          _      <- call.awaitHangup.timeout(5.seconds)
          answer <- peer.messages
            .collectFirst {
              case Inbound(_, r: SipResponse)
                  if r.headers.cseq.method == SipMethod.Bye =>
                r
            }
            .compile
            .lastOrError
            .timeout(5.seconds)
        } yield {
          assertEquals(ack.headers.callId, invite.headers.callId)
          assertEquals(answer.statusCode, 200)
        }
      }
    }
  }

  /** `io`, again after a moment while it fails for want of a pair of ports. */
  private def retrying[A](io: IO[A], times: Int): IO[A] =
    io.handleErrorWith {
      case _: SoftphoneError.RtpPortsExhausted if times > 0 =>
        IO.sleep(100.millis) >> retrying(io, times - 1)
      case other => IO.raiseError(other)
    }

  test("the RTP ports of a call go back to the pool when it ends") {
    // One pair of ports for the whole phone.
    val small = SoftphoneStreaming.resource[IO](
      config.copy(
        baseRtpPort = port"19120",
        maxRtpPort = Some(port"19121")
      ),
      audio,
      NoOpLogger.impl[IO]
    )
    (small, remote).tupled.use { (softphone, peer) =>
      for {
        first <- softphone.call(callee).use { call =>
          for {
            invite <- requestOf(peer, SipMethod.Invite)
            _      <- peer.send(us, okFor(invite))
            _      <- call.awaitPickup.timeout(5.seconds)
            // The pair is held, so another call finds none.
            blocked <- softphone.call(callee).use_.attempt
          } yield (invite, blocked)
        }
        // The first call is over, and its pair is free again.
        // The system may take a moment to let go of the sockets just closed.
        second <- retrying(
          softphone
            .call(callee)
            .use(_ => requestOf(peer, SipMethod.Invite)),
          20
        )
      } yield {
        assertEquals(
          first._2.left.toOption,
          Some(SoftphoneError.RtpPortsExhausted(19120, 19121))
        )
        assert(first._1.body.contains("m=audio 19120"), first._1.body)
        assert(second.body.contains("m=audio 19120"), second.body)
      }
    }
  }

  test(
    "a range of RTP ports with no pair in it is refused when the phone starts"
  ) {
    SoftphoneStreaming
      .resource[IO](
        config.copy(
          baseRtpPort = port"19130",
          maxRtpPort = Some(port"19130")
        ),
        audio,
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map(result =>
        assert(
          result.left.exists(_.isInstanceOf[SoftphoneError.RtpPortsInvalid]),
          result
        )
      )
  }

  test(
    "a message is sent to the callee over UDP and delivered when answered 200"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      for {
        sending <- softphone
          .sendMessage(callee, MessageContent.plain("hi bob, é ✓"))
          .start
        message <- requestOf(peer, SipMethod.Message)
        _ <- peer.send(us, TransactionMessages.responseTo(message, 200, "OK"))
        result <- sending.joinWithNever.timeout(5.seconds)
      } yield {
        assertEquals(result, Right(()))
        assertEquals(message.body, "hi bob, é ✓")
        assertEquals(message.requestUri.render, "sip:bob@127.0.0.1")
        assertEquals(message.headers.contentType, Some(MediaType.Plain))
        // From us, as the account, with a tag: outside a dialog the To has none.
        assertEquals(message.headers.to.tag, None)
        assert(message.headers.from.tag.isDefined)
      }
    }
  }

  test("a message that is refused says why") {
    (phone, remote).tupled.use { (softphone, peer) =>
      for {
        sending <- softphone
          .sendMessage(callee, MessageContent.plain("hi"))
          .start
        message <- requestOf(peer, SipMethod.Message)
        _       <- peer.send(
          us,
          TransactionMessages.responseTo(message, 403, "Forbidden")
        )
        result <- sending.joinWithNever.timeout(5.seconds)
      } yield assertEquals(
        result,
        Left(MessageFailure.Rejected(403, "Forbidden"))
      )
    }
  }

  test("a BYE for a call that does not exist is answered 481") {
    (phone, remote).tupled.use { (_, peer) =>
      val stray = SipRequest(
        SipMethod.Bye,
        uri("sip:test@127.0.0.1:19070"),
        SipHeaders(
          callId = callId("nobody@nowhere"),
          from = nameAddr("<sip:bob@127.0.0.1>;tag=r1"),
          to = nameAddr("<sip:test@127.0.0.1>;tag=l1"),
          vias = NonEmptyList.one(
            via("SIP/2.0/UDP 127.0.0.1:19071;branch=z9hG4bKstray")
          ),
          cseq = cseq(1, SipMethod.Bye)
        )
      )
      for {
        _      <- peer.send(us, stray)
        answer <- peer.messages
          .collectFirst { case Inbound(_, r: SipResponse) => r }
          .compile
          .lastOrError
          .timeout(5.seconds)
      } yield assertEquals(answer.statusCode, 481)
    }
  }

  test("registering over UDP binds the account and unregistering removes it") {
    (phone, remote).tupled.use { (softphone, peer) =>
      def registerOf(peer: Transport[IO]) = requestOf(peer, SipMethod.Register)
      def answer(request: SipRequest, granted: Int) = {
        val base =
          TransactionMessages.responseTo(request, 200, "OK", Some(tag("srv")))
        peer.send(
          us,
          base.copy(headers =
            base.headers.copy(contacts =
              request.headers.contacts.map(c =>
                c.withExpires(Expires(granted).fold(err => fail(err), identity))
              )
            )
          )
        )
      }
      softphone.register.allocated.flatMap { case (registration, release) =>
        for {
          first     <- registerOf(peer)
          _         <- answer(first, 3600)
          _         <- registration.awaitRegistered.timeout(5.seconds)
          state     <- registration.state.get
          releasing <- release.start
          removal   <- registerOf(peer)
          _         <- answer(removal, 0)
          _         <- releasing.join
        } yield {
          assertEquals(first.requestUri.render, "sip:127.0.0.1")
          assertEquals(
            first.headers.expires,
            Some(Expires(3600).fold(err => fail(err), identity))
          )
          assertEquals(
            state,
            softphone4s.registration.RegistrationState.Registered(3600.seconds)
          )
          assertEquals(removal.headers.expires, Some(Expires.Zero))
          assertEquals(removal.headers.callId, first.headers.callId)
        }
      }
    }
  }

  test("no codec configured is an error before anything is bound") {
    SoftphoneStreaming
      .resource[IO](config.copy(codecs = Nil), audio, NoOpLogger.impl[IO])
      .use_
      .attempt
      .map(r =>
        assertEquals(r.left.toOption, Some(SoftphoneError.NoCodecsConfigured))
      )
  }

  test("a codec nobody supplied is refused when the softphone starts") {
    SoftphoneStreaming
      .resource[IO](
        config.copy(codecs = List(CodecChoice.Named("opus"))),
        audio,
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map(r =>
        assertEquals(r.left.toOption, Some(SoftphoneError.UnknownCodec("opus")))
      )
  }

  test("a codec the program supplies is accepted, and listed as it chooses") {
    SoftphoneStreaming
      .resource[IO](
        config
          .copy(codecs = List(CodecChoice.Named("x-half"), CodecChoice.Pcmu)),
        audio,
        NoOpLogger.impl[IO],
        List(HalfByteCodec)
      )
      .use_
  }

  test("G.722 beside G.711 is refused: one audio format cannot serve both") {
    SoftphoneStreaming
      .resource[IO](
        config.copy(codecs = List(CodecChoice.G722, CodecChoice.Pcmu)),
        audio,
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map(r =>
        assertEquals(r.left.toOption, Some(SoftphoneError.MixedCodecRates))
      )
  }

  test("G.722 alone starts on a 16 kHz audio format") {
    SoftphoneStreaming
      .resource[IO](
        config.copy(codecs = List(CodecChoice.G722)),
        new AudioFormat(16000f, 16, 1, true, false),
        NoOpLogger.impl[IO]
      )
      .use_
  }

  test("codecs that work at another rate than the audio format are refused") {
    SoftphoneStreaming
      .resource[IO](
        config,
        new AudioFormat(16000f, 16, 1, true, false),
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map(r =>
        assertEquals(
          r.left.toOption,
          Some(SoftphoneError.AudioFormatMismatch(8000, 16000))
        )
      )
  }
}
