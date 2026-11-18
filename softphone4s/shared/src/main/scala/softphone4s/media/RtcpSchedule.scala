package softphone4s.media

import scala.concurrent.duration.*

/** When to send RTCP reports (RFC 3550 §6.3): about every `interval`, each wait
  * drawn between half and one and a half times it so that the two ends of a
  * call do not fall into step, and the first after half as long, so that the
  * other side hears from us early.
  *
  * The RFC scales the interval with the size of the session and corrects its
  * randomization by 1/1.21828; a call of two parties needs neither.
  */
final case class RtcpSchedule private (interval: FiniteDuration) {

  /** The wait before the first report, for a `random` in [0, 1). */
  def first(random: Double): FiniteDuration = scaled(interval / 2, random)

  /** The wait before the next one, for a `random` in [0, 1). */
  def next(random: Double): FiniteDuration = scaled(interval, random)

  private def scaled(base: FiniteDuration, random: Double): FiniteDuration =
    (base.toNanos * (0.5 + math.max(0.0, math.min(1.0, random)))).toLong.nanos
}

object RtcpSchedule {

  /** The 5 seconds the RFC gives for a session with little bandwidth. */
  val Default: RtcpSchedule = new RtcpSchedule(5.seconds)

  /** `interval` if it is more than nothing. */
  def apply(interval: FiniteDuration): Either[String, RtcpSchedule] =
    Either.cond(
      interval > Duration.Zero,
      new RtcpSchedule(interval),
      s"RTCP interval must be positive: $interval"
    )
}
