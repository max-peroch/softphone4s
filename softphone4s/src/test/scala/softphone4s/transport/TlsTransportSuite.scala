package softphone4s.transport

import cats.data.NonEmptyList
import cats.effect.std.Queue
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import fs2.io.net.tls.TLSParameters
import fs2.Stream
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.codec.SipCodec
import softphone4s.model.*

import java.nio.file.Path
import scala.concurrent.duration.*

/** The TLS transport against a TLS server on loopback, with certificates made
  * for the purpose.
  */
class TlsTransportSuite extends CatsEffectSuite {

  private val files = TlsFixtures.files

  private def optionsTo(host: String, n: Int = 1) = SipRequest(
    SipMethod.Options,
    uri(s"sip:bob@$host"),
    SipHeaders(
      callId = callId(s"tls$n@host"),
      from = nameAddr("<sip:alice@example.com>;tag=a"),
      to = nameAddr(s"<sip:bob@$host>"),
      vias =
        NonEmptyList.one(via(s"SIP/2.0/TLS 127.0.0.1:5061;branch=z9hG4bK$n")),
      cseq = cseq(n, SipMethod.Options)
    ),
    body = "hello"
  )

  private final case class Server(
      address: SocketAddress[IpAddress],
      received: Queue[IO, SipMessage]
  )

  /** A TLS server showing `identity`. `serverParameters` shapes its side of the
    * handshake. Whatever arrives is queued; `reply` is sent back on the
    * connection.
    */
  private def server(
      identity: Path = files.server,
      clientsTrusted: Option[Path] = None,
      serverParameters: TLSParameters = TLSParameters.Default,
      reply: Option[SipMessage] = None
  ): Resource[IO, Server] =
    for {
      context  <- TlsFixtures.serverContext(identity, clientsTrusted)
      received <- Resource.eval(Queue.unbounded[IO, SipMessage])
      bound    <- Network[IO].bind(SocketAddress(ip"127.0.0.1", port"0"))
      address = bound.address match {
        case a: SocketAddress[?] =>
          a.host match {
            case ip: IpAddress => SocketAddress(ip, a.port)
            case _             => fail("not an IP address")
          }
        case _ => fail("not an IP address")
      }
      _ <- bound.accept
        .map { socket =>
          Stream
            .resource(
              context
                .serverBuilder(socket)
                .withParameters(serverParameters)
                .build
            )
            .flatMap { tls =>
              tls.reads
                .through(StreamFraming.frames[IO]())
                .evalMap {
                  case Right(bytes) =>
                    SipCodec.decode(bytes).toOption.traverse_(received.offer) >>
                      reply.traverse_(r => tls.write(SipCodec.encode(r)))
                  case Left(_) => IO.unit
                }
            }
            .handleErrorWith(_ => Stream.empty)
        }
        .parJoinUnbounded
        .compile
        .drain
        .background
    } yield Server(address, received)

  private def client(config: TlsConfig) =
    TlsTransport.resource[IO](ip"127.0.0.1", None, config, NoOpLogger.impl[IO])

  private val trustsServer =
    TlsConfig(trustStore = Some(TlsFixtures.trusting(files.trustsServer)))

  private def first(srv: Server): IO[SipMessage] =
    Stream
      .fromQueueUnterminated(srv.received, limit = 1)
      .head
      .compile
      .lastOrError
      .timeout(5.seconds)

  test(
    "a message reaches a server whose certificate we trust and that is for the host we meant"
  ) {
    (server(), client(trustsServer)).tupled.use { (srv, transport) =>
      transport.send(srv.address, optionsTo("localhost")) >>
        first(srv).map(assertEquals(_, optionsTo("localhost")))
    }
  }

  test("what the server sends back on the connection is delivered") {
    val answer = optionsTo("localhost", 2)
    (server(reply = Some(answer)), client(trustsServer)).tupled.use {
      (srv, transport) =>
        transport.send(srv.address, optionsTo("localhost")) >>
          transport.messages.head.compile.lastOrError
            .timeout(5.seconds)
            .map(assertEquals(_, Inbound(srv.address, answer)))
    }
  }

  test("messages to one server share a connection") {
    (server(), client(trustsServer)).tupled.use { (srv, transport) =>
      (1 to 3).toList.traverse_(n =>
        transport.send(srv.address, optionsTo("localhost", n))
      ) >>
        Stream
          .fromQueueUnterminated(srv.received, limit = 1)
          .take(3)
          .compile
          .toList
          .timeout(5.seconds)
          .map(r =>
            assertEquals(r.map(_.headers.cseq.seq.value), List(1, 2, 3))
          )
    }
  }

  test(
    "a certificate nobody we trust vouches for is refused, and nothing is sent"
  ) {
    (server(identity = files.stranger), client(trustsServer)).tupled.use {
      (srv, transport) =>
        transport.send(srv.address, optionsTo("localhost")).attempt.flatMap {
          result =>
            IO.sleep(200.millis) >> srv.received.tryTake.map { delivered =>
              assert(result.isLeft, "the handshake must fail")
              assertEquals(delivered, None)
            }
        }
    }
  }

  test(
    "with no trust store of our own the JVM's trust is used, which does not know a test certificate"
  ) {
    (server(), client(TlsConfig())).tupled.use { (srv, transport) =>
      transport
        .send(srv.address, optionsTo("localhost"))
        .attempt
        .map(r => assert(r.isLeft))
    }
  }

  test("a certificate for another host than the one we meant is refused") {
    (server(), client(trustsServer)).tupled.use { (srv, transport) =>
      transport
        .send(srv.address, optionsTo("other.example"))
        .attempt
        .map(r => assert(r.isLeft))
    }
  }

  test("the same mismatch passes when verification is switched off") {
    val skipping =
      trustsServer.copy(hostnameVerification = HostnameVerification.Skip)
    (server(), client(skipping)).tupled.use { (srv, transport) =>
      transport.send(srv.address, optionsTo("other.example")) >>
        first(srv).map(assertEquals(_, optionsTo("other.example")))
    }
  }

  test(
    "a destination given as an IP address has no name to check, so it fails closed"
  ) {
    (server(), client(trustsServer)).tupled.use { (srv, transport) =>
      transport
        .send(srv.address, optionsTo("127.0.0.1"))
        .attempt
        .map(r => assert(r.isLeft))
    }
  }

  test(
    "a server that only speaks TLS 1.2 is refused when we require 1.3, and accepted when we allow 1.2"
  ) {
    val old = TLSParameters(protocols = Some(List("TLSv1.2")))
    (
      server(serverParameters = old),
      client(trustsServer.copy(minVersion = TlsVersion.Tls13)),
      client(trustsServer)
    ).tupled
      .use { (srv, strict, lenient) =>
        for {
          refused <- strict.send(srv.address, optionsTo("localhost")).attempt
          _       <- lenient.send(srv.address, optionsTo("localhost", 2))
          got     <- first(srv)
        } yield {
          assert(refused.isLeft, "TLS 1.3 required, 1.2 offered")
          assertEquals(got, optionsTo("localhost", 2))
        }
      }
  }

  test(
    "a server that asks for a client certificate gets ours, and refuses us without one"
  ) {
    val mutual   = TLSParameters(needClientAuth = true)
    val withCert = trustsServer.copy(clientCertificate =
      Some(TlsFixtures.trusting(files.client))
    )
    (
      server(
        clientsTrusted = Some(files.trustsClient),
        serverParameters = mutual
      ),
      client(withCert),
      client(trustsServer)
    ).tupled
      .use { (srv, presenting, anonymous) =>
        for {
          refused <- anonymous.send(srv.address, optionsTo("localhost")).attempt
          // With TLS 1.3 the failure may only show on the first read, so
          // check that nothing arrived rather than the send's result.
          _    <- IO.sleep(300.millis)
          none <- srv.received.tryTake
          _    <- presenting.send(srv.address, optionsTo("localhost", 2))
          got  <- first(srv)
        } yield {
          assertEquals(none, None, s"anonymous send: $refused")
          assertEquals(got, optionsTo("localhost", 2))
        }
      }
  }

  test("a key store that cannot be read stops the transport being built") {
    val missing = TlsConfig(trustStore =
      Some(KeyStoreConfig(Path.of("/no/such/store.p12"), "x"))
    )
    client(missing).use_.attempt.map {
      case Left(SoftphoneError.TlsConfigurationInvalid(_)) => ()
      case other => fail(s"unexpected $other")
    }
  }

  test("a wrong key store password is a configuration error too") {
    val wrong = TlsConfig(trustStore =
      Some(KeyStoreConfig(files.trustsServer, "not the password"))
    )
    client(wrong).use_.attempt.map {
      case Left(SoftphoneError.TlsConfigurationInvalid(_)) => ()
      case other => fail(s"unexpected $other")
    }
  }

  // ---- What a connection is told ----------------------------------------------------

  test("a request is addressed to its first Route hop, else its Request-URI") {
    val plain  = optionsTo("example.com")
    val routed = plain.copy(headers =
      plain.headers.copy(routes = List(nameAddr("<sip:proxy.example.net;lr>")))
    )
    assertEquals(
      TlsTransport.serverNameOf(plain).map(_.toString),
      Some("example.com")
    )
    assertEquals(
      TlsTransport.serverNameOf(routed).map(_.toString),
      Some("proxy.example.net")
    )
    assertEquals(
      TlsTransport.serverNameOf(SipResponse(200, "OK", plain.headers)),
      None
    )
  }

  test("the server's name is sent as SNI, unless it is an IP address") {
    val named =
      TlsTransport.parameters(TlsConfig(), Host.fromString("sip.example.com"))
    assertEquals(
      named.serverNames.map(_.map(n => new String(n.getEncoded, "US-ASCII"))),
      Some(List("sip.example.com"))
    )
    assertEquals(
      TlsTransport
        .parameters(TlsConfig(), Host.fromString("192.0.2.1"))
        .serverNames,
      None
    )
    assertEquals(TlsTransport.parameters(TlsConfig(), None).serverNames, None)
  }

  test(
    "hostname verification and the protocol floor reach the handshake parameters"
  ) {
    val strict =
      TlsTransport.parameters(TlsConfig(minVersion = TlsVersion.Tls13), None)
    assertEquals(strict.endpointIdentificationAlgorithm, Some("HTTPS"))
    assertEquals(strict.protocols, Some(List("TLSv1.3")))
    val lax = TlsTransport.parameters(
      TlsConfig(hostnameVerification = HostnameVerification.Skip),
      None
    )
    assertEquals(lax.endpointIdentificationAlgorithm, None)
    assertEquals(lax.protocols, Some(List("TLSv1.2", "TLSv1.3")))
  }

  // ---- Accepting connections ------------------------------------------------------

  private def listeningConfig(
      authentication: ClientAuthentication = ClientAuthentication.Off
  ) =
    TlsConfig(
      trustStore = Some(TlsFixtures.trusting(files.trustsClient)),
      serverCertificate = Some(TlsFixtures.trusting(files.server)),
      clientAuthentication = authentication
    )

  private def listening(config: TlsConfig) =
    TlsTransport
      .resource[IO](ip"127.0.0.1", Some(port"0"), config, NoOpLogger.impl[IO])

  /** A TLS client for the server at `address`, reading the host as `localhost`.
    */
  private def connecting(
      address: SocketAddress[IpAddress],
      identity: Option[Path] = None
  ): Resource[IO, RawPeer] =
    for {
      context <- TlsFixtures.clientContext(files.trustsServer, identity)
      raw     <- Network[IO].connect(address)
      tls     <- context
        .clientBuilder(raw)
        .withParameters(
          TLSParameters(serverNames =
            Some(List(new javax.net.ssl.SNIHostName("localhost")))
          )
        )
        .build
      peer <- RawPeer(tls)
    } yield peer

  test(
    "a TLS client that connects to us is served, and our reply goes back to it"
  ) {
    listening(listeningConfig()).use { transport =>
      connecting(transport.localAddress).use { peer =>
        for {
          _       <- peer.send(optionsTo("localhost"))
          inbound <- transport.messages.head.compile.lastOrError
            .timeout(5.seconds)
          _ <- transport.send(
            inbound.from,
            SipResponse(200, "OK", optionsTo("localhost").headers)
          )
          back <- peer.next
        } yield {
          assertEquals(inbound.message, optionsTo("localhost"))
          assertEquals(
            back,
            SipResponse(200, "OK", optionsTo("localhost").headers)
          )
        }
      }
    }
  }

  test("listening for TLS needs a server certificate") {
    listening(TlsConfig()).use_.attempt.map {
      case Left(SoftphoneError.TlsConfigurationInvalid(_)) => ()
      case other => fail(s"unexpected $other")
    }
  }

  test(
    "a client with no certificate is turned away when one is required, and a trusted one is let in"
  ) {
    listening(listeningConfig(ClientAuthentication.Require)).use { transport =>
      for {
        anonymous <- connecting(transport.localAddress).use { peer =>
          peer.send(optionsTo("localhost")) >> IO.sleep(300.millis)
        }.attempt
        none <- transport.messages.head.compile.last.timeout(500.millis).attempt
        _    <- connecting(transport.localAddress, Some(files.client))
          .use(_.send(optionsTo("localhost", 2)))
        got <- transport.messages.head.compile.lastOrError.timeout(5.seconds)
      } yield {
        assert(
          none.isLeft,
          s"nothing must be delivered to an anonymous client: $anonymous"
        )
        assertEquals(got.message, optionsTo("localhost", 2))
      }
    }
  }

  test(
    "a client with a certificate nobody we trust vouches for is turned away"
  ) {
    val untrusted = files.stranger
    listening(listeningConfig(ClientAuthentication.Require)).use { transport =>
      connecting(transport.localAddress, Some(untrusted)).use { peer =>
        peer.send(optionsTo("localhost")) >> IO.sleep(300.millis)
      }.attempt >>
        transport.messages.head.compile.last
          .timeout(500.millis)
          .attempt
          .map(r => assert(r.isLeft))
    }
  }
}
