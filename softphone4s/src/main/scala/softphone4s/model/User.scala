package softphone4s.model

import cats.{Eq, Show}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** The user part of a SIP URI: unreserved characters and `%HH` escapes, kept
  * percent-encoded.
  */
opaque type User = String

object User {
  private val syntax = UriSyntax.token(UriSyntax.UserChars, Emptiness.Forbidden)

  def apply(s: String): Either[String, User] =
    UriSyntax.check("user", syntax, s)

  extension (u: User) def value: String = u

  given Eq[User]   = Eq.fromUniversalEquals
  given Show[User] = Show.show(identity)

  given ConfigReader[User] = ConfigReader[String].emap(s =>
    apply(s).left.map(CannotConvert(s, "User", _))
  )
}
