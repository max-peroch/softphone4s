package softphone4s

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*
import softphone4s.transport.RawPeer

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** A softphone that accepts TCP connections, against a raw client. */
class TcpListenSuite extends CatsEffectSuite {

  private val audio = new AudioFormat(8000f, 16, 1, true, false)
  private val ours  = port"19140"

  private def phone =
    SoftphoneStreaming.resource[IO](
      SoftphoneConfig(
        user = sipUser("test"),
        password = Secret("test"),
        realm = host"127.0.0.1",
        localSipPort = ours,
        baseRtpPort = port"19800",
        bindAddress = ip"127.0.0.1",
        hangupTimeout = 2.seconds.positive,
        listen = Set(TransportProtocol.Tcp)
      ),
      audio,
      NoOpLogger.impl[IO]
    )

  private def client: Resource[IO, RawPeer] =
    Network[IO].connect(SocketAddress(ip"127.0.0.1", ours)).flatMap(RawPeer(_))

  private val offer =
    "v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\n" +
      "m=audio 40000 RTP/AVP 0\r\na=rtpmap:0 PCMU/8000\r\n"

  // Nothing listens on 19999: the client can be reached only on its connection.
  private val invite = SipRequest(
    SipMethod.Invite,
    uri("sip:test@127.0.0.1:19140"),
    SipHeaders(
      callId = callId("tcp-in@host"),
      from = nameAddr("<sip:bob@127.0.0.1>;tag=ft"),
      to = nameAddr("<sip:test@127.0.0.1>"),
      vias = NonEmptyList.one(
        via("SIP/2.0/TCP 127.0.0.1:19999;branch=z9hG4bKtcp1;alias")
      ),
      cseq = cseq(1, SipMethod.Invite),
      contacts = List(nameAddr("<sip:bob@127.0.0.1:19999;transport=tcp>")),
      contentType = Some(MediaType.Sdp)
    ),
    offer
  )

  private def untilFinal(peer: RawPeer): IO[SipResponse] =
    peer.next.flatMap {
      case r: SipResponse if r.statusCode >= 200 => IO.pure(r)
      case _                                     => untilFinal(peer)
    }

  test(
    "an incoming call over TCP is answered on its connection, and our BYE comes back down it"
  ) {
    (phone, client).tupled.use { (softphone, peer) =>
      for {
        _        <- peer.send(invite)
        incoming <- softphone.incoming.head.compile.lastOrError
          .timeout(5.seconds)
        accepted <- incoming.accept.allocated
        ok       <- untilFinal(peer)
        ack = SipRequest(
          SipMethod.Ack,
          uri("sip:test@127.0.0.1:19140"),
          invite.headers.copy(
            to = ok.headers.to,
            vias = NonEmptyList.one(
              via("SIP/2.0/TCP 127.0.0.1:19999;branch=z9hG4bKtcp2;alias")
            ),
            cseq = cseq(1, SipMethod.Ack)
          )
        )
        _ <- peer.send(ack)
        _ <- accepted._1.awaitPickup.timeout(5.seconds)
        // Hanging up sends a BYE to the Contact, which only the alias can reach.
        _   <- accepted._2.start
        bye <- peer.next.iterateUntil {
          case r: SipRequest => r.method == SipMethod.Bye
          case _             => false
        }
      } yield {
        assertEquals(ok.statusCode, 200)
        assertEquals(ok.headers.topVia.transport, TransportProtocol.Tcp)
        assertEquals(
          ok.headers.contacts.flatMap(_.uri.transport),
          List(TransportProtocol.Tcp)
        )
        bye match {
          case r: SipRequest =>
            assertEquals(r.headers.topVia.transport, TransportProtocol.Tcp)
          case other => fail(s"expected a BYE, got $other")
        }
      }
    }
  }

  test(
    "a client whose stream cannot be framed is dropped, and others carry on"
  ) {
    (phone, client, client).tupled.use { (softphone, bad, good) =>
      for {
        // A request with no Content-Length: nothing says where it ends.
        _ <- bad.sendRaw(
          "INVITE sip:test@127.0.0.1 SIP/2.0\r\nCall-ID: x\r\n\r\n"
        )
        _        <- good.send(invite)
        incoming <- softphone.incoming.head.compile.lastOrError
          .timeout(5.seconds)
      } yield assertEquals(incoming.caller.uri.render, "sip:bob@127.0.0.1")
    }
  }
}
