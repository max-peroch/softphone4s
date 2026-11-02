package softphone4s.loopback

import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.port
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.Call
import softphone4s.model.{FailureStatus, SoftphoneError}

import java.lang.management.ManagementFactory
import scala.concurrent.duration.*

/** Calls between two softphones over real UDP: that they work, how they end
  * when they go wrong, and that nothing is left behind.
  */
class LoopbackCallSuite extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 3.minutes

  /** A second of loud audio: 16-bit little-endian samples of 8000. */
  private val tone: Chunk[Byte] =
    Chunk.array(
      Array.tabulate[Byte](16000)(i => if i % 2 == 0 then 0x40 else 0x1f)
    )

  private def loud(heard: Chunk[Byte]): Boolean =
    heard.toArray
      .grouped(2)
      .exists(s =>
        s.length == 2 && math.abs(
          ((s(1) << 8) | (s(0) & 0xff)).toShort.toInt
        ) > 4000
      )

  private def hears(call: Call[IO]): IO[Boolean] =
    call.source.take(16000).compile.to(Chunk).map(loud)

  /** One frame of 20 ms: 160 samples of the tone. */
  private val frame: Chunk[Byte] = tone.take(320)

  /** Talks into `call` as a microphone does, a frame every 20 ms, for as long
    * as `listening` takes to hear it. (All of it at once would leave before the
    * other side has bound its RTP socket, and be lost.)
    */
  private def speaksWhile[A](call: Call[IO])(listening: IO[A]): IO[A] =
    Stream
      .fixedRate[IO](20.millis)
      .flatMap(_ => Stream.chunk(frame))
      .through(call.sink)
      .compile
      .drain
      .background
      .surround(listening)

  test(
    "a call connects, audio goes both ways, and the caller's hang-up reaches the callee"
  ) {
    Loopback.pair().use { p =>
      p.alice.call(p.callee()).allocated.flatMap { (aliceCall, hangUp) =>
        for {
          incoming <- p.bob.incoming.head.compile.lastOrError
            .timeout(10.seconds)
          _      <- incoming.ringing
          result <- incoming.accept.use { bobCall =>
            for {
              _ <- (aliceCall.awaitPickup, bobCall.awaitPickup).parTupled
                .timeout(10.seconds)
              heard <- (
                speaksWhile(aliceCall)(hears(bobCall).timeout(20.seconds)),
                speaksWhile(bobCall)(hears(aliceCall).timeout(20.seconds))
              ).parTupled
              _ <- hangUp
              _ <- bobCall.awaitHangup.timeout(10.seconds)
            } yield heard
          }
        } yield {
          assertEquals(incoming.account, p.bob.accounts.head)
          assertEquals(result, (true, true))
        }
      }
    }
  }

  test(
    "a call cancelled before it is answered is cancelled for the callee too"
  ) {
    Loopback.pair().use { p =>
      p.alice.call(p.callee()).allocated.flatMap { (_, hangUp) =>
        for {
          incoming <- p.bob.incoming.head.compile.lastOrError
            .timeout(10.seconds)
          _ <- incoming.ringing
          _ <- hangUp
          _ <- incoming.cancelled.timeout(10.seconds)
        } yield ()
      }
    }
  }

  test("a call the callee rejects fails for the caller with the status") {
    Loopback.pair().use { p =>
      p.alice.call(p.callee()).use { call =>
        for {
          incoming <- p.bob.incoming.head.compile.lastOrError
            .timeout(10.seconds)
          _      <- incoming.reject(FailureStatus.Decline)
          result <- call.awaitPickup.attempt.timeout(10.seconds)
        } yield assertEquals(
          result,
          Left(SoftphoneError.CallRejected(603, "Decline"))
        )
      }
    }
  }

  test("a call nobody answers is turned away after the answer timeout") {
    Loopback.pair(Loopback.Settings(answerTimeout = 1.second)).use { p =>
      p.alice.call(p.callee()).use { call =>
        call.awaitPickup.attempt.timeout(15.seconds).map {
          case Left(SoftphoneError.CallRejected(480, _)) => ()
          case other => fail(s"expected 480, got $other")
        }
      }
    }
  }

  test("a phone with one account takes a call for any user at its address") {
    Loopback.pair().use { p =>
      p.alice.call(p.callee("anyone")).use { call =>
        for {
          incoming <- p.bob.incoming.head.compile.lastOrError
            .timeout(10.seconds)
          _      <- incoming.reject()
          result <- call.awaitPickup.attempt.timeout(10.seconds)
        } yield {
          assertEquals(incoming.account, p.bob.accounts.head)
          assertEquals(
            result,
            Left(SoftphoneError.CallRejected(603, "Decline"))
          )
        }
      }
    }
  }

  test(
    "a phone with several accounts answers 404 to a user it does not hold, and delivers one it does"
  ) {
    Loopback.pair(Loopback.Settings(bobAlsoHolds = Some("carol"))).use { p =>
      for {
        unknown <- p.alice
          .call(p.callee("nobody"))
          .use(_.awaitPickup.attempt.timeout(10.seconds))
        known <- p.alice.call(p.callee("carol")).use { call =>
          for {
            incoming <- p.bob.incoming.head.compile.lastOrError
              .timeout(10.seconds)
            _ <- incoming.reject()
            _ <- call.awaitPickup.attempt.timeout(10.seconds)
          } yield incoming.account
        }
      } yield {
        unknown match {
          case Left(SoftphoneError.CallRejected(404, _)) => ()
          case other => fail(s"expected 404, got $other")
        }
        assertEquals(known, p.bob.accounts.last)
      }
    }
  }

  test(
    "a call to an address where nobody listens fails with a typed error, and does not hang"
  ) {
    Loopback.pair(Loopback.Settings(inviteTimeout = 3.seconds)).use { p =>
      p.alice.call(p.callee(port = port"19429")).use { call =>
        call.awaitPickup.attempt.timeout(20.seconds).map {
          case Left(_: SoftphoneError) => ()
          case other => fail(s"expected a SoftphoneError, got $other")
        }
      }
    }
  }

  // ---- Nothing is left behind ----------------------------------------------------

  private def openFileDescriptors: IO[Long] =
    IO.delay(ManagementFactory.getOperatingSystemMXBean match {
      case unix: com.sun.management.UnixOperatingSystemMXBean =>
        unix.getOpenFileDescriptorCount
      case _ => -1L
    })

  /** A whole call, answered and hung up. */
  private def oneCall(p: Loopback.Pair): IO[Unit] =
    p.alice.call(p.callee()).use { aliceCall =>
      for {
        incoming <- p.bob.incoming.head.compile.lastOrError.timeout(10.seconds)
        _        <- incoming.accept.use { bobCall =>
          (aliceCall.awaitPickup, bobCall.awaitPickup).parTupled
            .timeout(10.seconds)
            .void
        }
      } yield ()
    }

  test(
    "many calls one after another do not use up the RTP ports, which are four pairs"
  ) {
    // Each phone has four pairs of ports. If a call kept its pair, the fifth
    // call would fail with RtpPortsExhausted.
    Loopback.pair().use { p =>
      (1 to 40).toList.traverse_(_ => oneCall(p))
    }
  }

  test("calls of every ending leave no socket open, and no port taken") {
    Loopback.pair(Loopback.Settings(answerTimeout = 1.second)).use { p =>
      val answered  = oneCall(p)
      val cancelled =
        p.alice.call(p.callee()).allocated.flatMap { (_, hangUp) =>
          p.bob.incoming.head.compile.lastOrError
            .timeout(10.seconds)
            .flatMap(_ => hangUp)
        }
      val rejected = p.alice.call(p.callee()).use { call =>
        p.bob.incoming.head.compile.lastOrError
          .timeout(10.seconds)
          .flatMap(_.reject()) >>
          call.awaitPickup.attempt.void
      }
      val unanswered = p.alice
        .call(p.callee())
        .use(_.awaitPickup.attempt.timeout(15.seconds).void) >>
        p.bob.incoming.head.compile.lastOrError.timeout(5.seconds).attempt.void
      val cycle = answered >> cancelled >> rejected >> unanswered
      for {
        _ <- (1 to 3).toList
          .traverse_(_ => cycle) // warm up: first uses open what they keep
        before <- openFileDescriptors
        _      <- (1 to 8).toList.traverse_(_ => cycle)
        after  <- openFileDescriptors
      } yield {
        assume(before >= 0, "no file descriptor count on this platform")
        assert(
          after - before <= 4,
          s"file descriptors went from $before to $after over 32 calls"
        )
      }
    }
  }
}
