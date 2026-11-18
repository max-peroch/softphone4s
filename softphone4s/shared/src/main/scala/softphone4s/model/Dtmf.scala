package softphone4s.model

import scala.concurrent.duration.*

/** What a digit sent by us is allowed to be. */
object Dtmf {

  /** How long a digit lasts when the caller does not say: a tone phones make.
    */
  val DefaultDuration: FiniteDuration = 160.millis

  private val Shortest = 20.millis

  /** The longest a telephone event can last: its duration is 16 bits at 8 000 a
    * second.
    */
  private val Longest = 8.seconds

  /** `duration` if a digit can last that long. */
  def checkedDuration(
      duration: FiniteDuration
  ): Either[SoftphoneError, FiniteDuration] =
    Either.cond(
      duration >= Shortest && duration <= Longest,
      duration,
      SoftphoneError.InvalidDtmfDuration(duration.toMillis)
    )
}
