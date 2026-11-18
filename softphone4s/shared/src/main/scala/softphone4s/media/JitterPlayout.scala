package softphone4s.media

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

/** A jitter buffer at work: packets go in as they arrive and a frame comes out
  * every `period`, along with how the buffer stands.
  *
  * It is a loop of effects and not a stream, because a frame is made fifty
  * times a second for every call, and a stream of ticks merged with a stream of
  * packets costs a handshake between fibers for each. The packets are put in
  * the buffer by one fiber and the ticks take from it in another, through a
  * `Ref`.
  */
object JitterPlayout {

  /** Runs the playout and gives each frame to `onFrame`, until the packets end,
    * when it returns. An error in the packets, or in `onFrame`, is its error;
    * and if it is stopped, so are both. A `period` of zero or less is taken as
    * the shortest there is, a nanosecond, which the schedule can divide by.
    */
  def run[F[_]: Async](
      config: JitterConfig,
      period: FiniteDuration,
      packets: Stream[F, JitterPacket]
  )(onFrame: (Playout, JitterStats) => F[Unit]): F[Unit] =
    Ref.of[F, JitterBuffer](JitterBuffer.empty(config)).flatMap { buffer =>
      val feed = packets
        .evalMap(packet => buffer.update(_.push(packet)._1))
        .compile
        .drain
      val tick = buffer
        .modify { current =>
          val (next, out) = current.pull
          (next, (out, next.stats))
        }
        .flatMap(onFrame.tupled)
      Async[F].monotonic
        .flatMap(start =>
          Async[F].race(ticks(period.max(Shortest), start)(tick), feed).void
        )
    }

  private val Shortest: FiniteDuration = FiniteDuration(1, NANOSECONDS)

  /** `tick` every `period` from `last`, on a fixed schedule: how long `tick`
    * takes does not move the next one. If it takes more than a period, one tick
    * is made at once for all the periods that were missed, and the schedule is
    * kept (as `Stream.fixedRate` does).
    */
  private def ticks[F[_]](period: FiniteDuration, last: FiniteDuration)(
      tick: F[Unit]
  )(using F: Async[F]): F[Unit] =
    F.monotonic.flatMap { now =>
      val next = last + period
      if next > now then
        F.sleep(next - now) >> tick >> ticks(period, next)(tick)
      else {
        val missed = (now.toNanos - last.toNanos - 1) / period.toNanos
        (if missed < 0 then F.unit else tick) >>
          ticks(period, last + period * missed)(tick)
      }
    }
}
