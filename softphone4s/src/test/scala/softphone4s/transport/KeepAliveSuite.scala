package softphone4s.transport

import cats.data.NonEmptyList
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Chunk
import fs2.io.net.{Network, Socket}
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.codec.SipCodec
import softphone4s.model.*

import java.nio.charset.StandardCharsets.ISO_8859_1
import scala.concurrent.duration.*

/** CRLF keep-alives (RFC 5626 §4.4.1) on a TCP connection to a bare server. */
class KeepAliveSuite extends CatsEffectSuite {

  private val request = SipRequest(
    SipMethod.Options,
    uri("sip:bob@example.com"),
    SipHeaders(
      callId = callId("ka@host"),
      from = nameAddr("<sip:alice@example.com>;tag=a"),
      to = nameAddr("<sip:bob@example.com>"),
      vias = NonEmptyList.one(
        via("SIP/2.0/TCP 127.0.0.1:5060;branch=z9hG4bKka")
      ),
      cseq = cseq(1, SipMethod.Options)
    ),
    body = "hello"
  )

  private val sent = new String(SipCodec.encode(request).toArray, ISO_8859_1)

  private final case class Server(
      address: SocketAddress[IpAddress],
      connections: Ref[IO, Int],
      received: Ref[IO, String]
  )

  private def text(chunk: Chunk[Byte]): String =
    new String(chunk.toArray, ISO_8859_1)

  private def bytes(s: String): Chunk[Byte] =
    Chunk.array(s.getBytes(ISO_8859_1))

  /** A server that keeps the raw text it receives and lets `react` write back
    * whenever something arrives.
    */
  private def server(
      react: (Socket[IO], String) => IO[Unit]
  ): Resource[IO, Server] =
    for {
      connections <- Resource.eval(Ref.of[IO, Int](0))
      received    <- Resource.eval(Ref.of[IO, String](""))
      bound       <- Network[IO].bind(SocketAddress(ip"127.0.0.1", port"0"))
      address     <- Resource.eval(RawPeer.addressOf(bound.address))
      _           <- bound.accept
        .map(socket =>
          fs2.Stream.eval(connections.update(_ + 1)) >>
            socket.reads.chunks
              .evalMap(chunk =>
                received.updateAndGet(_ + text(chunk)).flatMap(react(socket, _))
              )
              .handleErrorWith(_ => fs2.Stream.empty)
        )
        .parJoinUnbounded
        .compile
        .drain
        .background
    } yield Server(address, connections, received)

  private def tcp(config: StreamConfig) =
    TcpTransport.resource[IO](ip"127.0.0.1", None, config, NoOpLogger.impl[IO])

  private def pinging(
      interval: FiniteDuration,
      timeout: FiniteDuration = 5.seconds
  ) = StreamConfig(keepAlive = Some(KeepAlive(interval, timeout)))

  test("a ping from the server is answered with a pong") {
    server((socket, got) =>
      // The ping goes out once the client has spoken.
      socket.write(bytes("\r\n\r\n")).whenA(got == sent)
    ).flatMap(srv => tcp(StreamConfig()).map(srv -> _)).use {
      (srv, transport) =>
        transport.send(srv.address, request) >>
          srv.received.get
            .iterateUntil(_ == sent + "\r\n")
            .timeout(5.seconds)
            .map(assertEquals(_, sent + "\r\n"))
    }
  }

  test("a quiet connection is pinged when keep-alives are configured") {
    server((_, _) => IO.unit)
      .flatMap(srv => tcp(pinging(200.millis)).map(srv -> _))
      .use { (srv, transport) =>
        transport.send(srv.address, request) >>
          srv.received.get
            .iterateUntil(_.startsWith(sent + "\r\n\r\n"))
            .timeout(5.seconds)
            .void
      }
  }

  test("a connection that answers its pings stays open") {
    server((socket, got) =>
      socket.write(bytes("\r\n")).whenA(got.endsWith("\r\n\r\n"))
    ).flatMap(srv => tcp(pinging(150.millis, 500.millis)).map(srv -> _)).use {
      (srv, transport) =>
        transport.send(srv.address, request) >>
          IO.sleep(1500.millis) >>
          transport.send(srv.address, request) >>
          srv.connections.get.map(assertEquals(_, 1))
    }
  }

  test(
    "a connection that does not answer a ping is closed, and the next send opens another"
  ) {
    server((_, _) => IO.unit)
      .flatMap(srv => tcp(pinging(150.millis, 300.millis)).map(srv -> _))
      .use { (srv, transport) =>
        transport.send(srv.address, request) >>
          IO.sleep(1500.millis) >>
          transport.send(srv.address, request) >>
          // The server counts a connection when it accepts it.
          srv.connections.get
            .iterateUntil(_ >= 2)
            .timeout(5.seconds)
            .map(assertEquals(_, 2))
      }
  }
}
