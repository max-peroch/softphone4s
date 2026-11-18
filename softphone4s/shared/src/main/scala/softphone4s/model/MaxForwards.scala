package softphone4s.model

import cats.{Eq, Show}

/** The Max-Forwards hop limit: 0 to 255 (RFC 3261 §8.1.1.4). */
opaque type MaxForwards = Int

object MaxForwards {

  def apply(n: Int): Either[String, MaxForwards] =
    Either.cond(n >= 0 && n <= 255, n, s"invalid Max-Forwards: $n")

  /** The recommended initial value. */
  val Default: MaxForwards = 70

  extension (m: MaxForwards) {
    def value: Int = m

    /** One hop less, or `None` when the limit is spent (answer 483). */
    def decrement: Option[MaxForwards] = Option.when(m > 0)(m - 1)
  }

  given Eq[MaxForwards]   = Eq.fromUniversalEquals
  given Show[MaxForwards] = Show.show(_.toString)
}
