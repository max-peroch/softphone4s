package softphone4s

import cats.effect.std.Queue
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.Network
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.codec.SipCodec
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*
import softphone4s.transaction.TransactionMessages
import softphone4s.transport.{StreamFraming, TlsConfig, TlsFixtures}

import java.nio.file.Path
import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** Calls placed over TLS to a bare SIP server on loopback. */
class TlsCallSuite extends CatsEffectSuite {

  private val audio = new AudioFormat(8000f, 16, 1, true, false)
  private val files = TlsFixtures.files

  private final case class Server(
      port: Port,
      connections: Ref[IO, Int],
      requests: Queue[IO, SipRequest]
  )

  private def okTo(request: SipRequest, listening: Port): SipResponse = {
    val base =
      TransactionMessages.responseTo(request, 200, "OK", Some(tag("rtag")))
    base.copy(
      headers = base.headers.copy(
        contacts =
          List(nameAddr(s"<sip:bob@localhost:$listening;transport=tls>")),
        contentType = Some(MediaType.Sdp)
      ),
      body = "v=0\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\nm=audio 40000 RTP/AVP 0\r\n"
    )
  }

  private def server(identity: Path): Resource[IO, Server] =
    for {
      context     <- TlsFixtures.serverContext(identity)
      connections <- Resource.eval(Ref.of[IO, Int](0))
      requests    <- Resource.eval(Queue.unbounded[IO, SipRequest])
      bound       <- Network[IO].bind(SocketAddress(ip"127.0.0.1", port"0"))
      listening   <- Resource.eval(bound.address match {
        case a: SocketAddress[?] => IO.pure(a.port)
        case _ => IO.raiseError(new RuntimeException("no port"))
      })
      _ <- bound.accept
        .map { socket =>
          Stream
            .resource(context.serverBuilder(socket).build)
            .flatMap { tls =>
              Stream.eval(connections.update(_ + 1)) >>
                tls.reads
                  .through(StreamFraming.frames[IO]())
                  .evalMap {
                    case Right(bytes) =>
                      SipCodec.decode(bytes) match {
                        case Right(request: SipRequest) =>
                          requests.offer(request) >>
                            Option
                              .when(
                                request.method == SipMethod.Invite ||
                                  request.method == SipMethod.Bye
                              )(okTo(request, listening))
                              .traverse_(r => tls.write(SipCodec.encode(r)))
                        case _ => IO.unit
                      }
                    case Left(_) => IO.unit
                  }
            }
            .handleErrorWith(_ => Stream.empty)
        }
        .parJoinUnbounded
        .compile
        .drain
        .background
    } yield Server(listening, connections, requests)

  private def phone(
      serverPort: Port,
      ours: Port,
      trust: Path,
      realm: String = "127.0.0.1"
  ) =
    SoftphoneStreaming.resource[IO](
      SoftphoneConfig(
        user = "test",
        password = "test",
        realm = realm,
        localSipPort = ours,
        serverPort = serverPort,
        baseRtpPort = port"19600",
        bindAddress = ip"127.0.0.1",
        hangupTimeout = 2.seconds,
        transport = TransportProtocol.Tls,
        tls = TlsConfig(trustStore = Some(TlsFixtures.trusting(trust)))
      ),
      audio,
      NoOpLogger.impl[IO]
    )

  private def callee(srv: Server) =
    Callee(s"bob@localhost:${srv.port}").fold(err => fail(err), identity)

  private def next(srv: Server, method: SipMethod) =
    Stream
      .fromQueueUnterminated(srv.requests, limit = 1)
      .filter(_.method == method)
      .head
      .compile
      .lastOrError
      .timeout(5.seconds)

  test(
    "a call over TLS uses one connection for the INVITE, the ACK and the BYE"
  ) {
    server(files.server)
      .flatMap(srv =>
        phone(srv.port, port"19120", files.trustsServer).map(srv -> _)
      )
      .use { (srv, phone) =>
        for {
          invite <- phone.call(callee(srv)).use { call =>
            call.awaitPickup >> next(srv, SipMethod.Invite) <* next(
              srv,
              SipMethod.Ack
            )
          }
          bye         <- next(srv, SipMethod.Bye)
          connections <- srv.connections.get
        } yield {
          assertEquals(invite.headers.topVia.transport, TransportProtocol.Tls)
          assertEquals(
            invite.headers.contacts.flatMap(_.uri.transport),
            List(TransportProtocol.Tls)
          )
          assertEquals(bye.headers.topVia.transport, TransportProtocol.Tls)
          assertEquals(connections, 1)
        }
      }
  }

  test("a server whose certificate we do not trust cannot be called") {
    server(files.stranger)
      .flatMap(srv =>
        phone(srv.port, port"19121", files.trustsServer).map(srv -> _)
      )
      .use { (srv, phone) =>
        phone.call(callee(srv)).use(_.awaitPickup).attempt.flatMap { result =>
          srv.requests.tryTake.map { seen =>
            assertEquals(result, Left(SoftphoneError.TransportFailed))
            assertEquals(seen, None)
          }
        }
      }
  }

  test(
    "registering over TLS reaches the server with a Via and Contact that say TLS"
  ) {
    server(files.server)
      .flatMap(srv =>
        phone(srv.port, port"19122", files.trustsServer, realm = "localhost")
          .map(srv -> _)
      )
      .use { (srv, phone) =>
        phone.register.use { _ =>
          next(srv, SipMethod.Register).map { register =>
            assertEquals(
              register.headers.topVia.transport,
              TransportProtocol.Tls
            )
            assertEquals(
              register.headers.contacts.flatMap(_.uri.transport),
              List(TransportProtocol.Tls)
            )
          }
        }
      }
  }
}
