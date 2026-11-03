package softphone4s

import cats.effect.std.Queue
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.{Network, Socket}
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.codec.SipCodec
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*
import softphone4s.transaction.TransactionMessages
import softphone4s.transport.StreamFraming

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** Calls placed over TCP to a bare SIP server on loopback. */
class TcpCallSuite extends CatsEffectSuite {

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  /** A server that records the connections it takes and the requests that come
    * in, and answers them with `answer` on the connection they came on.
    */
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
          List(nameAddr(s"<sip:bob@127.0.0.1:$listening;transport=tcp>")),
        contentType = Some(MediaType.Sdp)
      ),
      body = "v=0\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\nm=audio 40000 RTP/AVP 0\r\n"
    )
  }

  private def server(
      answer: (SipRequest, Port) => Option[SipResponse]
  ): Resource[IO, Server] =
    for {
      connections <- Resource.eval(Ref.of[IO, Int](0))
      requests    <- Resource.eval(Queue.unbounded[IO, SipRequest])
      bound       <- Network[IO].bind(SocketAddress(ip"127.0.0.1", port"0"))
      listening   <- Resource.eval(
        bound.address match {
          case a: SocketAddress[?] => IO.pure(a.port)
          case _ => IO.raiseError(new RuntimeException("no port"))
        }
      )
      _ <- bound.accept
        .map { (socket: Socket[IO]) =>
          Stream.eval(connections.update(_ + 1)) >>
            socket.reads
              .through(StreamFraming.frames[IO]())
              .evalMap {
                case Right(bytes) =>
                  SipCodec.decode(bytes) match {
                    case Right(request: SipRequest) =>
                      requests.offer(request) >>
                        answer(request, listening).traverse_(r =>
                          socket.write(SipCodec.encode(r))
                        )
                    case _ => IO.unit
                  }
                case Left(_) => IO.unit
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
      transport: TransportProtocol = TransportProtocol.Tcp
  ) =
    SoftphoneStreaming.resource[IO](
      SoftphoneConfig(
        user = sipUser("test"),
        password = Secret("test"),
        realm = host"127.0.0.1",
        localSipPort = ours,
        serverPort = serverPort,
        baseRtpPort = port"19500",
        bindAddress = ip"127.0.0.1",
        hangupTimeout = 2.seconds.positive,
        transport = transport
      ),
      audio,
      NoOpLogger.impl[IO]
    )

  private val bob = Callee("bob").fold(err => fail(err), identity)

  private def next(srv: Server, method: SipMethod) =
    Stream
      .fromQueueUnterminated(srv.requests, limit = 1)
      .filter(_.method == method)
      .head
      .compile
      .lastOrError
      .timeout(5.seconds)

  test(
    "a call over TCP uses one connection for the INVITE, the ACK and the BYE"
  ) {
    val answer = (r: SipRequest, listening: Port) =>
      r.method match {
        case SipMethod.Invite | SipMethod.Bye => Some(okTo(r, listening))
        case _                                => None
      }
    server(answer)
      .flatMap(srv => phone(srv.port, port"19110").map(srv -> _))
      .use { (srv, phone) =>
        for {
          invite <- phone.call(bob).use { call =>
            call.awaitPickup >> next(srv, SipMethod.Invite) <* next(
              srv,
              SipMethod.Ack
            )
          }
          bye         <- next(srv, SipMethod.Bye)
          connections <- srv.connections.get
        } yield {
          assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
          assertEquals(
            invite.headers.contacts.flatMap(_.uri.transport),
            List(TransportProtocol.Tcp)
          )
          assertEquals(bye.headers.topVia.transport, TransportProtocol.Tcp)
          assertEquals(connections, 1)
        }
      }
  }

  test("over TCP an unanswered INVITE is sent once, not repeated") {
    server((_, _) => None)
      .flatMap(srv => phone(srv.port, port"19111").map(srv -> _))
      .use { (srv, phone) =>
        phone.call(bob).use { _ =>
          next(srv, SipMethod.Invite) >>
            // UDP would have repeated it after 500 ms and again after 1.5 s.
            IO.sleep(2.seconds) >>
            srv.requests.tryTake
              .map(extra => assertEquals(extra.map(_.method), None))
        }
      }
  }

  test("a transport we cannot speak is refused, not tried") {
    server((_, _) => None)
      .flatMap(srv =>
        phone(srv.port, port"19112", TransportProtocol.Sctp).map(srv -> _)
      )
      .use { (srv, phone) =>
        for {
          call <- phone.call(bob).use_.attempt
          reg  <- phone.register.use_.attempt
          seen <- srv.connections.get
        } yield {
          assertEquals(call, Left(SoftphoneError.TransportUnavailable("sctp")))
          assertEquals(reg, Left(SoftphoneError.TransportUnavailable("sctp")))
          assertEquals(seen, 0)
        }
      }
  }

  test(
    "over UDP a call whose INVITE is too big for a datagram goes to the server over TCP"
  ) {
    val answer = (r: SipRequest, listening: Port) =>
      r.method match {
        case SipMethod.Invite | SipMethod.Bye => Some(okTo(r, listening))
        case _                                => None
      }
    server(answer)
      .flatMap(srv =>
        phone(srv.port, port"19114", TransportProtocol.Udp).map(srv -> _)
      )
      .use { (srv, phone) =>
        val big = Map("X-Context" -> List("x" * 1500))
        for {
          invite <- phone.call(bob, big).use { call =>
            call.awaitPickup >> next(srv, SipMethod.Invite)
          }
        } yield {
          // Nothing listens for UDP here: only the TCP server can have answered.
          assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
          assertEquals(
            invite.headers.contacts.flatMap(_.uri.transport),
            List(TransportProtocol.Tcp)
          )
        }
      }
  }

  test("over UDP a small INVITE does not touch the TCP server") {
    server((_, _) => None)
      .flatMap(srv =>
        phone(srv.port, port"19115", TransportProtocol.Udp).map(srv -> _)
      )
      .use { (srv, phone) =>
        phone.call(bob).use { _ =>
          IO.sleep(700.millis) >>
            srv.requests.tryTake.map(seen => assertEquals(seen, None))
        }
      }
  }
}
