package softphone4s.media

import softphone4s.model.Ssrc
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.testkit.TestControl
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.SipLiterals.pt

import scala.concurrent.duration.*

/** The buffer at work, against a clock that is made up. */
class JitterPlayoutSuite extends CatsEffectSuite {

  private val period = 20.millis
  private val config = JitterConfig(3, 12).fold(e => fail(e), identity)

  private def packet(sequence: Int) =
    JitterPacket(
      sequence,
      sequence * 160L,
      Ssrc.fromBits(1),
      marker = false,
      pt(0),
      Chunk.array(Array((sequence >> 8).toByte, sequence.toByte))
    )

  private def show(out: Playout): String = out match {
    case Playout.Frame(packet) =>
      val p = packet.payload
      s"F${((p(0) & 0xff) << 8) | (p(1) & 0xff)}"
    case Playout.Lost    => "L"
    case Playout.Waiting => "."
  }

  /** What the playout gives while `packets` run, and until they end. */
  private def frames(packets: Stream[IO, JitterPacket]): IO[List[Playout]] =
    IO.ref(Vector.empty[Playout]).flatMap { played =>
      JitterPlayout.run(config, period, packets)((out, _) =>
        played.update(_ :+ out)
      ) >> played.get.map(_.toList)
    }

  /** What is played when packets arrive at the times given, ending a while
    * after the last.
    */
  private def played(
      arrivals: List[(FiniteDuration, Int)]
  ): IO[List[String]] = {
    val source = Stream
      .emits(arrivals.sortBy(_._1))
      .covary[IO]
      .evalMap { (at, sequence) =>
        IO.monotonic
          .flatMap(now => IO.sleep((at - now).max(Duration.Zero)))
          .as(packet(sequence))
      } ++ Stream.sleep[IO](500.millis).drain
    TestControl
      .executeEmbed(frames(source).map(_.map(show)))
      .map(_.dropWhile(_ == "."))
  }

  /** Packets on time, one every 20 ms from `start`. */
  private def steady(sequences: Range, start: FiniteDuration = Duration.Zero) =
    sequences.toList.map(n => (start + period * (n - sequences.head).toLong, n))

  test(
    "packets on time are played in order, and the pauses before and after are waiting"
  ) {
    played(steady(0 until 10)).map { out =>
      assertEquals(out.take(10), (0 until 10).map(n => s"F$n").toList)
      assert(out.drop(10).forall(_ == "."), out.toString)
    }
  }

  test(
    "jitter within the cushion changes nothing: packets 30 ms early or late are played in order"
  ) {
    // Each packet is up to 30 ms off its place, but the order of arrival is kept.
    val offsets  = List(0, 25, -5, 30, 10, 0, 28, -3, 15, 0).map(_.millis)
    val arrivals =
      steady(0 until 10, 107.millis).zip(offsets).map { case ((at, n), o) =>
        (at + o, n)
      }
    played(arrivals).map { out =>
      assertEquals(out.take(10), (0 until 10).map(n => s"F$n").toList)
    }
  }

  test(
    "packets that come in a different order are put right if they are within the cushion"
  ) {
    // 2 arrives after 3, and 5 after 6.
    val shifts   = Map(2 -> 25, 3 -> -25, 5 -> 25, 6 -> -25)
    val arrivals = steady(0 until 8, 107.millis).map { (at, n) =>
      (at + shifts.getOrElse(n, 0).millis, n)
    }
    played(arrivals).map(out =>
      assertEquals(out.take(8), (0 until 8).map(n => s"F$n").toList)
    )
  }

  test(
    "two packets that never arrive are two frames to conceal, and the clock does not slip"
  ) {
    val arrivals =
      steady(0 until 12, 107.millis).filterNot((_, n) => n == 5 || n == 6)
    played(arrivals).map { out =>
      assertEquals(
        out.take(12),
        List(
          "F0",
          "F1",
          "F2",
          "F3",
          "F4",
          "L",
          "L",
          "F7",
          "F8",
          "F9",
          "F10",
          "F11"
        )
      )
    }
  }

  test(
    "an outage lets the buffer run dry, and when packets return it fills again before playing"
  ) {
    val before = steady(0 until 5, 107.millis)
    val after  = steady(20 until 26, 107.millis + period * 20L)
    played(before ::: after).map { out =>
      // The five, then waiting, then the six that follow the outage.
      assertEquals(out.take(5), (0 until 5).map(n => s"F$n").toList)
      val rest = out.drop(5)
      assert(rest.startsWith(List(".")), rest.toString)
      assertEquals(
        rest.filterNot(_ == "."),
        (20 until 26).map(n => s"F$n").toList
      )
    }
  }

  test(
    "a packet delayed past its turn is dropped when it comes, and the gap was concealed"
  ) {
    val arrivals = steady(0 until 10, 107.millis).map { (at, n) =>
      if n == 4 then (at + 150.millis, n) else (at, n)
    }
    played(arrivals).map { out =>
      assertEquals(
        out.take(10),
        List("F0", "F1", "F2", "F3", "L", "F5", "F6", "F7", "F8", "F9")
      )
    }
  }

  test(
    "an error in the packets is the playout's error, after what was played"
  ) {
    val boom   = new RuntimeException("socket closed")
    val source = Stream.emit(packet(0)).covary[IO] ++
      Stream.sleep[IO](100.millis).drain ++ Stream.raiseError[IO](boom)
    TestControl
      .executeEmbed(frames(source).attempt)
      .map(result =>
        assertEquals(result.left.map(_.getMessage), Left("socket closed"))
      )
  }

  test("an error in what takes the frames is the playout's error") {
    val boom = new RuntimeException("cannot play")
    TestControl
      .executeEmbed(
        JitterPlayout
          .run(config, period, Stream.sleep[IO](1.minute).drain)((_, _) =>
            IO.raiseError(boom)
          )
          .attempt
      )
      .map(result =>
        assertEquals(result.left.map(_.getMessage), Left("cannot play"))
      )
  }

  test("stopping the playout stops what feeds it, and runs its finalizer") {
    TestControl.executeEmbed(
      for {
        stopped <- IO.ref(false)
        enough  <- IO.deferred[Unit]
        count   <- IO.ref(0)
        packets = Stream
          .iterate(0)(_ + 1)
          .covary[IO]
          .evalMap(n => IO.sleep(period).as(packet(n)))
          .onFinalize(stopped.set(true))
        _ <- IO.race(
          JitterPlayout.run(config, period, packets)((_, _) =>
            count
              .updateAndGet(_ + 1)
              .flatMap(n => enough.complete(()).whenA(n == 5))
          ),
          enough.get
        )
        ended <- stopped.get
        n     <- count.get
      } yield {
        assertEquals(n, 5)
        assert(
          ended,
          "the packets were still being read after the playout stopped"
        )
      }
    )
  }

  test(
    "a frame that takes longer than a period is made once for the periods missed, and the schedule is kept"
  ) {
    // The first frame takes 70 ms, so the ticks of 40 and 60 ms are missed: one
    // frame is made for them as soon as the first is done, and the 20 ms grid
    // goes on from there.
    TestControl.executeEmbed(
      for {
        times <- IO.ref(Vector.empty[FiniteDuration])
        first <- IO.ref(true)
        start <- IO.monotonic
        _     <- IO.race(
          JitterPlayout.run(config, period, Stream.sleep[IO](1.minute).drain)(
            (_, _) =>
              IO.monotonic.flatMap(now =>
                times.update(_ :+ (now - start)) >>
                  first
                    .getAndSet(false)
                    .flatMap(slow => IO.sleep(70.millis).whenA(slow))
              )
          ),
          IO.sleep(190.millis)
        )
        seen <- times.get
      } yield assertEquals(
        seen.map(_.toMillis),
        Vector(20L, 90L, 100L, 120L, 140L, 160L, 180L)
      )
    )
  }

  test(
    "its ticks fall where Stream.fixedRate's do, slow frames included"
  ) {
    // Frames that take 10, 50, 0, 130... ms, in turn, against a period of 20.
    val costs = List(10, 50, 0, 130, 0, 0, 25, 0, 0, 0).map(_.millis)

    /** When each frame is made, `drive` being what makes them. */
    def times(drive: IO[Unit] => IO[Unit]): IO[Vector[Long]] =
      for {
        seen  <- IO.ref(Vector.empty[Long])
        queue <- IO.ref(costs)
        start <- IO.monotonic
        frame =
          for {
            now  <- IO.monotonic
            _    <- seen.update(_ :+ (now - start).toMillis)
            cost <- queue.modify {
              case head :: tail => (tail, head)
              case Nil          => (Nil, 0.millis)
            }
            _ <- IO.sleep(cost)
          } yield ()
        // ends between two ticks, so that neither wins a tie with the clock
        _   <- IO.race(drive(frame), IO.sleep(1.second + 7.millis))
        out <- seen.get
      } yield out

    TestControl.executeEmbed(
      for {
        ours <- times(frame =>
          JitterPlayout.run(config, period, Stream.sleep[IO](1.minute).drain)(
            (_, _) => frame
          )
        )
        theirs <- times(frame =>
          Stream.fixedRate[IO](period).evalMap(_ => frame).compile.drain
        )
      } yield {
        assert(ours.size > 10, ours.toString)
        assertEquals(ours, theirs)
      }
    )
  }

  test(
    "a period of zero is the shortest there is, and not an arithmetic error"
  ) {
    TestControl.executeEmbed(
      for {
        count  <- IO.ref(0)
        result <- IO
          .race(
            JitterPlayout.run(
              config,
              Duration.Zero,
              Stream.sleep[IO](1.minute).drain
            )((_, _) => count.update(_ + 1)),
            IO.sleep(5.micros)
          )
          .attempt
        n <- count.get
      } yield {
        assert(result.isRight, result.toString)
        assert(n > 0, "no frame was made")
      }
    )
  }
}
