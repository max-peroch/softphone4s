package softphone4s.media

import cats.effect.Async
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
  ): Stream[F, (Playout, JitterStats)] = {
    val arrivals = packets.map(Left(_))
    val ticks    = Stream.fixedRate[F](period).map(_ => Right(()))
    arrivals
      .mergeHaltL(ticks)
      .mapAccumulate(JitterBuffer.empty(config)) {
        case (buffer, Left(packet)) => (buffer.push(packet)._1, None)
        case (buffer, Right(_))     =>
          val (next, out) = buffer.pull
          (next, Some((out, next.stats)))
      }
      .map(_._2)
      .unNone
  }
}
