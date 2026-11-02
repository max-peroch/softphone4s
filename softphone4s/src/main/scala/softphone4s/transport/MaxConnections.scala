package softphone4s.transport

import cats.{Eq, Show}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** How many connections made to us are served at once, at least one. */
opaque type MaxConnections = Int

object MaxConnections {

  def apply(n: Int): Either[String, MaxConnections] =
    Either.cond(n >= 1, n, s"$n connections is below one")

  val Default: MaxConnections = 256

  extension (max: MaxConnections) {
    def value: Int = max
  }

  given Eq[MaxConnections]   = Eq.fromUniversalEquals
  given Show[MaxConnections] = Show.show(_.toString)

  given ConfigReader[MaxConnections] = ConfigReader[Int].emap(n =>
    apply(n).left.map(CannotConvert(n.toString, "MaxConnections", _))
  )
}
