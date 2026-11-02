package softphone4s

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import fs2.io.net.tls.TLSParameters
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*
import softphone4s.transport.{RawPeer, TlsConfig, TlsFixtures}

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** A softphone that accepts TLS connections, against a raw TLS client. */
class TlsListenSuite extends CatsEffectSuite {

  private val audio = new AudioFormat(8000f, 16, 1, true, false)
  private val files = TlsFixtures.files
  private val tls   = port"19150"

  private def phone =
    SoftphoneStreaming.resource[IO](
      SoftphoneConfig(
        user = sipUser("test"),
        password = Secret("test"),
        realm = host"127.0.0.1",
        localSipPort = port"19151",
        baseRtpPort = port"19900",
        bindAddress = ip"127.0.0.1",
        hangupTimeout = 2.seconds,
        listen = Set(TransportProtocol.Tls),
        tlsPort = tls,
        tls = TlsConfig(serverCertificate =
          Some(TlsFixtures.trusting(files.server))
        )
      ),
      audio,
      NoOpLogger.impl[IO]
    )

  private def client: Resource[IO, RawPeer] =
    for {
      context <- TlsFixtures.clientContext(files.trustsServer)
      raw     <- Network[IO].connect(SocketAddress(ip"127.0.0.1", tls))
      secured <- context
        .clientBuilder(raw)
        .withParameters(
          TLSParameters(serverNames =
            Some(List(new javax.net.ssl.SNIHostName("localhost")))
          )
        )
        .build
      peer <- RawPeer(secured)
    } yield peer

  private val offer =
    "v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\n" +
      "m=audio 40000 RTP/AVP 0\r\na=rtpmap:0 PCMU/8000\r\n"

  private def inviteTo(target: String) = SipRequest(
    SipMethod.Invite,
    uri(target),
    SipHeaders(
      callId = callId("tls-in@host"),
      from = nameAddr("<sips:bob@127.0.0.1>;tag=ft"),
      to = nameAddr("<sips:test@127.0.0.1>"),
      vias = NonEmptyList.one(
        via("SIP/2.0/TLS 127.0.0.1:19999;branch=z9hG4bKtls1;alias")
      ),
      cseq = cseq(1, SipMethod.Invite),
      contacts = List(nameAddr("<sips:bob@127.0.0.1:19999>")),
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
    "an incoming call over TLS is answered on its connection, with a sips: Contact for a sips: request"
  ) {
    (phone, client).tupled.use { (softphone, peer) =>
      for {
        _        <- peer.send(inviteTo("sips:test@127.0.0.1:19150"))
        incoming <- softphone.incoming.head.compile.lastOrError
          .timeout(5.seconds)
        accepted <- incoming.accept.allocated
        ok       <- untilFinal(peer)
        _        <- accepted._2.start
      } yield {
        assertEquals(ok.statusCode, 200)
        assertEquals(ok.headers.topVia.transport, TransportProtocol.Tls)
        assertEquals(
          ok.headers.contacts.map(_.uri.scheme),
          List(SipScheme.Sips)
        )
      }
    }
  }

  test("a sip: request over TLS gets a sip: Contact that names TLS") {
    (phone, client).tupled.use { (softphone, peer) =>
      for {
        _        <- peer.send(inviteTo("sip:test@127.0.0.1:19150"))
        incoming <- softphone.incoming.head.compile.lastOrError
          .timeout(5.seconds)
        accepted <- incoming.accept.allocated
        ok       <- untilFinal(peer)
        _        <- accepted._2.start
      } yield {
        assertEquals(ok.headers.contacts.map(_.uri.scheme), List(SipScheme.Sip))
        assertEquals(
          ok.headers.contacts.flatMap(_.uri.transport),
          List(TransportProtocol.Tls)
        )
      }
    }
  }
}
