package softphone4s.config

import cats.{Eq, Show}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** How many times to try, at least once. */
opaque type Attempts = Int

object Attempts {

  def apply(times: Int): Either[String, Attempts] =
    Either.cond(times >= 1, times, s"$times attempts is below one")

  /** Five, which is what STUN asks for before it gives up on a round. */
  val Default: Attempts = 5

  extension (attempts: Attempts) {
    def value: Int = attempts
  }

  given Eq[Attempts]   = Eq.fromUniversalEquals
  given Show[Attempts] = Show.show(_.toString)

  given ConfigReader[Attempts] = ConfigReader[Int].emap(n =>
    apply(n).left.map(CannotConvert(n.toString, "Attempts", _))
  )
}
