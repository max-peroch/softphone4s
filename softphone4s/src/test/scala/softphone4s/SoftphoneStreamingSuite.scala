package softphone4s

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.config.SoftphoneConfig
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
}
