package softphone4s.stun

import cats.effect.{IO, Resource}
import com.comcast.ip4s.*
import fs2.Chunk
import fs2.io.net.{Datagram, Network}
import munit.CatsEffectSuite
import softphone4s.config.Attempts

import scala.concurrent.duration.*

/** Asking a STUN server how a port is mapped, from a socket bound to that port.
  */
class StunProbeSuite extends CatsEffectSuite {

  private val local   = SocketAddress(ip"127.0.0.1", port"19985")
  private val outside = SocketAddress(ip"203.0.113.7", port"41000")

  private def probe(
      server: StunServer.Running,
      retransmit: FiniteDuration = 100.millis,
      attempts: Attempts = Attempts(2).fold(e => sys.error(e), identity)
  ) =
    StunProbe.mapping[IO](local, server.address, retransmit, attempts)

  /** A server that, for each request, sends a stale answer (to another request,
    * saying `outside`) before the real one (saying `seen`).
    */
  private def staleThenReal(
      seen: SocketAddress[IpAddress]
  ): Resource[IO, StunServer.Running] =
    for {
      socket <- Network[IO].bindDatagramSocket(
        address = SocketAddress(ip"127.0.0.1", port"0")
      )
      requests <- Resource.eval(IO.ref(0))
      _        <- socket.reads
        .evalMap { datagram =>
          val id    = Chunk.array(datagram.bytes.drop(8).take(12).toArray)
          val stale = Chunk.array(Array.fill[Byte](12)(1))
          requests.update(_ + 1) >>
            socket.write(
              Datagram(datagram.remote, StunServer.response(stale, outside))
            ) >> socket.write(
              Datagram(datagram.remote, StunServer.response(id, seen))
            )
        }
        .compile
        .drain
        .background
    } yield StunServer.Running(socket.address.asIpUnsafe, requests)

  test("the answer is the address the server says it saw") {
    StunServer(says = Some(outside)).use(server =>
      probe(server).map(assertEquals(_, Right(outside)))
    )
  }

  test("the request comes from the port that was asked for, and not another") {
    // A server that says nothing of its own answers with the address it saw the
    // request come from, which is the port the probe was bound to.
    StunServer().use(server => probe(server).map(assertEquals(_, Right(local))))
  }

  test(
    "a request that gets no answer is sent again, and the second one's answer is taken"
  ) {
    StunServer(says = Some(outside), ignore = 1).use(server =>
      for {
        result   <- probe(server)
        requests <- server.requests.get
      } yield {
        assertEquals(result, Right(outside))
        assertEquals(requests, 2)
      }
    )
  }

  test(
    "a server that never answers is Left(NoAnswer), after the waits and no longer"
  ) {
    StunServer(ignore = 1000).use(server =>
      probe(server, retransmit = 100.millis).timed.map { (took, result) =>
        assertEquals(result, Left(Stun.Error.NoAnswer))
        // 100 ms and then 200 ms
        assert(took >= 250.millis && took < 2.seconds, took.toString)
      }
    )
  }

  test("the port is free again once the answer is in") {
    StunServer(says = Some(outside)).use(server =>
      probe(server) >>
        Network[IO]
          .bindDatagramSocket(address = local)
          .use_
          .attempt
          .map(r => assert(r.isRight, r.toString))
    )
  }

  test(
    "an answer to another request comes first, and is passed over for the real one"
  ) {
    val real = SocketAddress(ip"198.51.100.9", port"52000")
    staleThenReal(real).use(server =>
      probe(server).map(assertEquals(_, Right(real)))
    )
  }

  test(
    "an answer that is nothing to be reached at (port 0, no address, a group) is not an answer"
  ) {
    val unusable = List(
      SocketAddress(ip"203.0.113.7", port"0"),
      SocketAddress(ip"0.0.0.0", port"41000"),
      SocketAddress(ip"224.0.0.1", port"41000"),
      SocketAddress(ip"::", port"41000"),
      SocketAddress(ip"ff02::1", port"41000")
    )
    unusable.foldLeft(IO.unit)((done, address) =>
      done >> StunServer(says = Some(address)).use(server =>
        probe(server).map(result =>
          // a bad message is passed over, and the wait goes on to its end
          assertEquals(result, Left(Stun.Error.NoAnswer), address.toString)
        )
      )
    )
  }
}
