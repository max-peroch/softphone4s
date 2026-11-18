package softphone4s.loopback

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite

import scala.concurrent.duration.*

/** How long audio takes from one softphone to the other, over real UDP, when
  * the reader of the audio starts at once and when it starts late.
  *
  * It runs in real time, with bounds of 500 and 700 ms against 57 to 100 ms
  * measured on an idle machine. A machine under heavy load could still trip
  * them: check the load before reading a failure as a delay in the code.
  */
class AudioDelaySuite extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 3.minutes

  private def frameOf(level: Int): Chunk[Byte] =
    Chunk.array(
      Array.tabulate[Byte](320)(i =>
        if i % 2 == 0 then (level & 0xff).toByte
        else ((level >> 8) & 0xff).toByte
      )
    )

  private val quiet = frameOf(200)

  /** Eight loud levels, 1500 apart, which telephone coding keeps apart. */
  private val Labels                           = 8
  private def marker(number: Int): Chunk[Byte] =
    frameOf(12000 + 1500 * (number % Labels))

  private def level(frame: Chunk[Byte]): Int =
    math.abs(((frame(1) << 8) | (frame(0) & 0xff)).toShort.toInt)

  /** Which marker, modulo `Labels`, a frame is, if it is one. */
  private def labelOf(frame: Chunk[Byte]): Option[Int] =
    Some(level(frame)).filter(_ > 10000).map(l => ((l - 12000 + 750) / 1500))

  /** The time each marker took, from the one who speaks to the one who reads,
    * with the reader starting `readerDelay` after both are connected.
    */
  private def delays(
      readerDelay: FiniteDuration,
      every: Int = 50,
      count: Int = 8,
      settings: Loopback.Settings = Loopback.Settings()
  ): IO[List[FiniteDuration]] =
    Loopback.pair(settings).use { p =>
      p.alice.call(p.callee()).use { aliceCall =>
        for {
          incoming <- p.bob.incoming.head.compile.lastOrError
            .timeout(10.seconds)
          _   <- incoming.ringing
          out <- incoming.accept.use { bobCall =>
            for {
              _ <- (aliceCall.awaitPickup, bobCall.awaitPickup).parTupled
                .timeout(10.seconds)
              sent <- Ref.of[IO, Vector[(Int, FiniteDuration)]](Vector.empty)
              speech = Stream
                .fixedRate[IO](20.millis)
                .zipWithIndex
                .evalMap { (_, i) =>
                  if i % every == 0 then
                    IO.monotonic
                      .flatMap(t => sent.update(_ :+ ((i.toInt / every, t))))
                      .as(marker(i.toInt / every))
                  else IO.pure(quiet)
                }
                .flatMap(Stream.chunk)
                .through(bobCall.sink)
                .compile
                .drain
              received <- speech.background.surround(
                IO.sleep(readerDelay) >>
                  aliceCall.source
                    .chunkN(320)
                    // As a speaker takes it, at the pace it plays.
                    .metered(20.millis)
                    .map(labelOf)
                    .unNone
                    .evalMap(label => IO.monotonic.map(label -> _))
                    .take(count.toLong)
                    .compile
                    .toList
                    .timeout(40.seconds)
              )
              times <- sent.get
            } yield received.flatMap { (label, heardAt) =>
              // The latest marker of that label sent before it was heard.
              times
                .filter((n, t) => n % Labels == label && t <= heardAt)
                .lastOption
                .map((_, t) => heardAt - t)
            }
          }
        } yield out
      }
    }

  /** The delay once the buffers have settled: the last markers of eight. */
  private def settled(d: List[FiniteDuration]): FiniteDuration =
    d.drop(4).maxOption.getOrElse(1.hour)

  test("audio takes about the jitter cushion when the reader starts at once") {
    delays(Duration.Zero).map(d =>
      assert(settled(d) < 500.millis, s"markers took ${d.map(_.toMillis)} ms")
    )
  }

  test(
    "a reader that starts late hears the present, not what came while nobody read"
  ) {
    delays(3.seconds).map(d =>
      assert(settled(d) < 700.millis, s"markers took ${d.map(_.toMillis)} ms")
    )
  }
}
