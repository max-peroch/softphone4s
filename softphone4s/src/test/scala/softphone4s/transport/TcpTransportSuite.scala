package softphone4s.transport

import cats.data.NonEmptyList
import cats.effect.std.Queue
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{Network, Socket}
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.codec.SipCodec
import softphone4s.model.*

import scala.concurrent.duration.*

/** The TCP transport against a bare server on loopback. */
class TcpTransportSuite extends CatsEffectSuite {

  private def options(n: Int) = SipRequest(
    SipMethod.Options,
    uri("sip:bob@example.com"),
    SipHeaders(
      callId = callId(s"tcp$n@host"),
      from = nameAddr("<sip:alice@example.com>;tag=a"),
      to = nameAddr("<sip:bob@example.com>"),
      vias =
        NonEmptyList.one(via(s"SIP/2.0/TCP 127.0.0.1:5060;branch=z9hG4bK$n")),
      cseq = cseq(n, SipMethod.Options)
    ),
    body = "hello"
  )

  /** A server that records connections and what each sends, and hands each
    * connection to `serve`.
    */
  private final case class Server(
      address: SocketAddress[IpAddress],
      connections: Ref[IO, Int],
      received: Queue[IO, SipMessage]
  )

  private def server(
      serve: Socket[IO] => IO[Unit] = _ => IO.unit
  ): Resource[IO, Server] =
    for {
      connections <- Resource.eval(Ref.of[IO, Int](0))
      received    <- Resource.eval(Queue.unbounded[IO, SipMessage])
      bound       <- Network[IO].bind(SocketAddress(ip"127.0.0.1", port"0"))
      address = bound.address match {
        case a: SocketAddress[?] =>
          a.host match {
            case ip: IpAddress => SocketAddress(ip, a.port)
            case _ => fail("the server is not bound to an IP address")
          }
        case _ => fail("the server is not bound to an IP address")
      }
      _ <- bound.accept
        .map { socket =>
          Stream.eval(connections.update(_ + 1)) >>
            socket.reads
              .through(StreamFraming.frames[IO]())
              .evalMap {
                case Right(bytes) =>
                  SipCodec.decode(bytes).toOption.traverse_(received.offer)
                case Left(_) => IO.unit
              }
              .concurrently(Stream.eval(serve(socket)))
              .handleErrorWith(_ => Stream.empty)
        }
        .parJoinUnbounded
        .compile
        .drain
        .background
    } yield Server(address, connections, received)

  /** The next `n` messages the server received. */
  private def firstN(srv: Server, n: Int): IO[List[SipMessage]] =
    Stream
      .fromQueueUnterminated(srv.received, limit = 1)
      .take(n.toLong)
      .compile
      .toList
      .timeout(5.seconds)

  private def tcp(config: StreamConfig = StreamConfig()) =
    TcpTransport.resource[IO](ip"127.0.0.1", config, NoOpLogger.impl[IO])

  test("a message sent arrives whole at the server") {
    (server(), tcp()).tupled.use { (srv, transport) =>
      transport.send(srv.address, options(1)) >>
        firstN(srv, 1).map(_.head).map(assertEquals(_, options(1)))
    }
  }

  test(
    "what the server sends back on the connection is delivered, from the destination"
  ) {
    val reply = options(2)
    server(socket => socket.write(SipCodec.encode(reply)))
      .flatMap(srv => tcp().map(srv -> _))
      .use { (srv, transport) =>
        transport.send(srv.address, options(1)) >>
          transport.messages.head.compile.lastOrError
            .timeout(5.seconds)
            .map(assertEquals(_, Inbound(srv.address, reply)))
      }
  }

  test("messages to one destination share a connection") {
    (server(), tcp()).tupled.use { (srv, transport) =>
      (1 to 5).toList.traverse_(n => transport.send(srv.address, options(n))) >>
        firstN(srv, 5)
          .map(received =>
            assertEquals(
              received.map(_.headers.cseq.seq.value),
              List(1, 2, 3, 4, 5)
            )
          ) >>
        srv.connections.get.map(assertEquals(_, 1))
    }
  }

  test("concurrent sends to a new destination make one connection") {
    (server(), tcp()).tupled.use { (srv, transport) =>
      (1 to 10).toList.parTraverse_(n =>
        transport.send(srv.address, options(n))
      ) >>
        IO.sleep(200.millis) >>
        srv.connections.get.map(assertEquals(_, 1))
    }
  }

  test(
    "a connection idle for the timeout is closed, and the next send opens another"
  ) {
    val config = StreamConfig(idleTimeout = 200.millis)
    (server(), tcp(config)).tupled.use { (srv, transport) =>
      transport.send(srv.address, options(1)) >>
        IO.sleep(800.millis) >>
        transport.send(srv.address, options(2)) >>
        firstN(srv, 2) >>
        srv.connections.get.map(assertEquals(_, 2))
    }
  }

  test("when the server closes the connection the next send opens a new one") {
    (server(socket => IO.sleep(100.millis) >> socket.endOfOutput), tcp()).tupled
      .use { (srv, transport) =>
        transport.send(srv.address, options(1)) >>
          IO.sleep(500.millis) >>
          transport.send(srv.address, options(2)) >>
          firstN(srv, 2) >>
          srv.connections.get.map(c => assert(c >= 2, s"connections: $c"))
      }
  }

  test("sending to an address nobody listens on fails") {
    tcp()
      .use(_.send(SocketAddress(ip"127.0.0.1", port"1"), options(1)).attempt)
      .map(r => assert(r.isLeft))
  }

  test(
    "a stream that cannot be framed is dropped, and the transport carries on"
  ) {
    val garbage =
      Chunk.array("INVITE x SIP/2.0\r\nContent-Length: nope\r\n\r\n".getBytes)
    (server(socket => socket.write(garbage)), tcp()).tupled.use {
      (srv, transport) =>
        transport.send(srv.address, options(1)) >>
          IO.sleep(300.millis) >>
          transport.send(srv.address, options(2)) >>
          firstN(srv, 2)
            .map(r => assertEquals(r.size, 2))
    }
  }
}
