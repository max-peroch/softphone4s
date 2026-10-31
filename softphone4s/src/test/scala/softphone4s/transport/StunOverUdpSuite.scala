package softphone4s.transport

import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.stun.StunServer

import scala.concurrent.duration.*

/** STUN on the socket SIP uses. */
class StunOverUdpSuite extends CatsEffectSuite {

  private val server = SocketAddress(ip"127.0.0.1", port"19170")
  private val ours   = SocketAddress(ip"127.0.0.1", port"19171")

  private def udp =
    SipTransport
      .udp[IO](ours, NoOpLogger.impl[IO])
      // The socket is read only while the messages are being consumed.
      .flatMap(t => t.messages.compile.drain.background.as(t))

  test("the server says which address the request came from") {
    StunServer(server).flatMap(s => udp.map(s -> _)).use { (_, transport) =>
      transport
        .bindingRequest(server, 200.millis, 3)
        .map(assertEquals(_, Right(ours)))
    }
  }

  test("the request is repeated until it is answered") {
    StunServer(server, ignore = 2).flatMap(s => udp.map(s -> _)).use {
      (s, transport) =>
        transport.bindingRequest(server, 100.millis, 5).flatMap { result =>
          s.requests.get.map { n =>
            assertEquals(result, Right(ours))
            assertEquals(n, 3)
          }
        }
    }
  }

  test("a server that never answers is given up on") {
    StunServer(server, ignore = 100).flatMap(s => udp.map(s -> _)).use {
      (_, transport) =>
        transport
          .bindingRequest(server, 50.millis, 3)
          .map(result => assert(result.isLeft, result.toString))
    }
  }

  test("an answer from somewhere else than the server asked is not believed") {
    val impostor = SocketAddress(ip"127.0.0.1", port"19172")
    (
      StunServer(server, ignore = 100),
      StunServer(
        impostor,
        says = Some(SocketAddress(ip"203.0.113.9", port"1"))
      ),
      udp
    ).tupled
      .use { (_, _, transport) =>
        transport
          .bindingRequest(server, 50.millis, 2)
          .map(result => assert(result.isLeft, result.toString))
      }
  }
}
