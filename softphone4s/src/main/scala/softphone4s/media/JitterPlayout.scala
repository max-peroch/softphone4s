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
    val arrivals = packets.map(Event.Arrived(_))
    val ticks    = Stream.fixedRate[F](period).map(_ => Event.Tick)
    arrivals
      .mergeHaltL(ticks)
      .mapAccumulate(JitterBuffer.empty(config)) {
        case (buffer, Event.Arrived(packet)) => (buffer.push(packet)._1, None)
        case (buffer, Event.Tick)            =>
          val (next, out) = buffer.pull
          (next, Some((out, next.stats)))
      }
      .map(_._2)
      .unNone
  }

  /** What the buffer is told of. */
  private enum Event {
    case Arrived(packet: JitterPacket)
    case Tick
  }
}
