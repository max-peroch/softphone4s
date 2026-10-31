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

  private val any = SocketAddress(ip"127.0.0.1", port"0")

  private def udp =
    SipTransport
      .udp[IO](any, NoOpLogger.impl[IO])
      // The socket is read only while the messages are being consumed.
      .flatMap(t => t.messages.compile.drain.background.as(t))

  test("the server says which address the request came from") {
    (StunServer(), udp).tupled.use { (server, transport) =>
      transport
        .bindingRequest(server.address, 200.millis, 3)
        .map(assertEquals(_, Right(transport.localAddress)))
    }
  }

  test("the request is repeated until it is answered") {
    (StunServer(ignore = 2), udp).tupled.use { (server, transport) =>
      transport.bindingRequest(server.address, 100.millis, 5).flatMap {
        result =>
          server.requests.get.map { n =>
            assertEquals(result, Right(transport.localAddress))
            assertEquals(n, 3)
          }
      }
    }
  }

  test("a server that never answers is given up on") {
    (StunServer(ignore = 100), udp).tupled.use { (server, transport) =>
      transport
        .bindingRequest(server.address, 50.millis, 3)
        .map(result => assert(result.isLeft, result.toString))
    }
  }

  test("an answer from somewhere else than the server asked is not believed") {
    val lie = SocketAddress(ip"203.0.113.9", port"1")
    (StunServer(ignore = 100), StunServer(says = Some(lie)), udp).tupled
      .use { (server, _, transport) =>
        transport
          .bindingRequest(server.address, 50.millis, 2)
          .map(result => assert(result.isLeft, result.toString))
      }
  }
}
