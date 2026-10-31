package softphone4s.media

import cats.effect.IO
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
      1L,
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
      .executeEmbed(
        JitterPlayout(config, period, source)
          .map(p => show(p._1))
          .compile
          .toList
      )
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
}
