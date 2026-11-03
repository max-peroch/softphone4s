package softphone4s.model

import cats.{Eq, Show}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

import scala.concurrent.duration.*

/** A length of time above zero: a timeout, a wait before sending again, the
  * period of a refresh. Zero would make a timeout fire at once and a loop that
  * sleeps for it spin, and a negative duration is no time at all.
  */
opaque type PositiveDuration = FiniteDuration

object PositiveDuration {

  def apply(duration: FiniteDuration): Either[String, PositiveDuration] =
    Either.cond(
      duration > Duration.Zero,
      duration,
      s"$duration is not above zero"
    )

  /** `duration`, which must be above zero, unchecked: for the defaults of this
    * library, which are literals. A test checks every one of them.
    */
  private[softphone4s] def constant(
      duration: FiniteDuration
  ): PositiveDuration =
    duration

  extension (duration: PositiveDuration) {
    def value: FiniteDuration = duration
  }

  given Eq[PositiveDuration]   = Eq.fromUniversalEquals
  given Show[PositiveDuration] = Show.show(_.toString)

  given ConfigReader[PositiveDuration] = ConfigReader[FiniteDuration].emap(d =>
    apply(d).left.map(CannotConvert(d.toString, "PositiveDuration", _))
  )
}
