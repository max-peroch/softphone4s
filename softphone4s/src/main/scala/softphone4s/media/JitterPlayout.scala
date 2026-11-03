package softphone4s.media

import cats.effect.{Async, Deferred, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.concurrent.duration.FiniteDuration

/** A jitter buffer at work: packets go in as they arrive and a frame comes out
  * every `period`, along with how the buffer stands. It ends when the packets
  * do.
  */
object JitterPlayout {

  def apply[F[_]: Async](
      config: JitterConfig,
      period: FiniteDuration,
      packets: Stream[F, JitterPacket]
  ): Stream[F, (Playout, JitterStats)] =
    Stream
      .eval(
        (
          Ref.of[F, JitterBuffer](JitterBuffer.empty(config)),
          Deferred[F, Unit]
        ).tupled
      )
      .flatMap { (buffer, done) =>
        val feed = packets
          .evalMap(packet => buffer.update(_.push(packet)._1))
          .compile
          .drain
        Stream
          .fixedRate[F](period)
          .evalMap(_ =>
            buffer.modify { current =>
              val (next, out) = current.pull
              (next, (out, next.stats))
            }
          )
          .interruptWhen(done.get.map(_.asRight[Throwable]))
          .concurrently(Stream.exec(feed >> done.complete(()).void))
      }
}
