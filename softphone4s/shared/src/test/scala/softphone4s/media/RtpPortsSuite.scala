package softphone4s.media

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import munit.CatsEffectSuite
import softphone4s.model.SoftphoneError
import softphone4s.transport.RawPeer

class RtpPortsSuite extends CatsEffectSuite {

  private def range(from: Port, to: Port) =
    RtpPortRange(from, to).fold(e => fail(e), identity)

  private val alwaysFree: Port => IO[Boolean] = _ => IO.pure(true)

  private def pool(
      pairs: Int,
      probe: Port => IO[Boolean] = alwaysFree,
      attempts: Int = 100
  ) =
    RtpPorts[IO](
      range(port"20000", Port.fromInt(20000 + pairs * 2 - 1).get),
      probe,
      attempts
    )

  test("calls are given pairs one after the other, from the first") {
    for {
      ports <- pool(4)
      got   <- (ports.allocate, ports.allocate, ports.allocate).tupled
        .use(IO.pure)
    } yield assertEquals(got, (port"20000", port"20002", port"20004"))
  }

  test(
    "a pair is held until its resource is released, and then it counts as free"
  ) {
    for {
      ports  <- pool(4)
      during <- ports.allocate.use(_ => ports.inUse)
      after  <- ports.inUse
    } yield assertEquals((during, after), (1, 0))
  }

  test(
    "a pair given back is not given again until the range has been gone round"
  ) {
    for {
      ports <- pool(3)
      first <- ports.allocate.use(IO.pure)
      next  <- ports.allocate.use(IO.pure)
      third <- ports.allocate.use(IO.pure)
      again <- ports.allocate.use(IO.pure)
    } yield assertEquals(
      List(first, next, third, again),
      List(port"20000", port"20002", port"20004", port"20000")
    )
  }

  test("with every pair held the next call fails, saying which range") {
    for {
      ports  <- pool(1)
      failed <- ports.allocate.use(_ => ports.allocate.use_.attempt)
    } yield assertEquals(
      failed.left.toOption,
      Some(SoftphoneError.RtpPortsExhausted(20000, 20001))
    )
  }

  test("a pair that cannot be used is passed over, and does not stay held") {
    for {
      ports <- pool(3, probe = port => IO.pure(port != port"20000"))
      got   <- ports.allocate.use(port => ports.inUse.map(port -> _))
    } yield assertEquals(got, (port"20002", 1))
  }

  test("when none can be used the call fails and nothing is left held") {
    for {
      ports  <- pool(3, probe = _ => IO.pure(false))
      failed <- ports.allocate.use_.attempt
      held   <- ports.inUse
    } yield {
      assertEquals(
        failed.left.toOption,
        Some(SoftphoneError.RtpPortsExhausted(20000, 20005))
      )
      assertEquals(held, 0)
    }
  }

  test("only so many pairs are tried before giving up") {
    for {
      tried <- Ref.of[IO, Int](0)
      ports <- pool(
        10,
        probe = _ => tried.update(_ + 1).as(false),
        attempts = 3
      )
      failed <- ports.allocate.use_.attempt
      count  <- tried.get
    } yield {
      assert(failed.isLeft)
      assertEquals(count, 3)
    }
  }

  test("calls at the same time are given different pairs") {
    for {
      ports <- pool(100)
      got   <- (1 to 50).toList.parTraverse(_ => ports.allocate.allocated)
      ports50 = got.map(_._1)
      held <- ports.inUse
      _    <- got.traverse_(_._2)
      left <- ports.inUse
    } yield {
      assertEquals(ports50.distinct.size, 50)
      assertEquals((held, left), (50, 0))
    }
  }

  // ---- Against real sockets ----------------------------------------------------------

  private val loopback = ip"127.0.0.1"

  /** An even port with room for two pairs above it, found by binding. */
  private def freeEven: IO[Int] =
    Network[IO]
      .bindDatagramSocket(address = SocketAddress(loopback, port"0"))
      .use(socket => RawPeer.addressOf(socket.address))
      .map(a => a.port.value - a.port.value % 2 + 2)

  private def socketAt(p: Int): Resource[IO, ?] =
    Network[IO].bindDatagramSocket(
      address = SocketAddress(loopback, Port.fromInt(p).get)
    )

  test("a pair whose RTP port another process holds is passed over") {
    freeEven.flatMap { first =>
      val r = range(Port.fromInt(first).get, Port.fromInt(first + 3).get)
      socketAt(first)
        .use { _ =>
          RtpPorts.binding[IO](r, loopback).flatMap(_.allocate.use(IO.pure))
        }
        .map(got => assertEquals(got.value, first + 2))
    }
  }

  test("a pair whose RTCP port another process holds is passed over too") {
    freeEven.flatMap { first =>
      val r = range(Port.fromInt(first).get, Port.fromInt(first + 3).get)
      socketAt(first + 1)
        .use { _ =>
          RtpPorts.binding[IO](r, loopback).flatMap(_.allocate.use(IO.pure))
        }
        .map(got => assertEquals(got.value, first + 2))
    }
  }

  test(
    "a pair that is free is given, and the ports are free again once it is released"
  ) {
    freeEven.flatMap { first =>
      val r = range(Port.fromInt(first).get, Port.fromInt(first + 1).get)
      for {
        ports <- RtpPorts.binding[IO](r, loopback)
        got   <- ports.allocate.use(IO.pure)
        // The call that held it is over: its ports can be bound again.
        again <- socketAt(got.value)
          .use(_ => socketAt(got.value + 1).use_)
          .attempt
      } yield {
        assertEquals(got.value, first)
        assert(again.isRight, again)
      }
    }
  }
}
