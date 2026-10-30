package softphone4s.model

import cats.{Eq, Show}

/** The user part of a SIP URI: unreserved characters and `%HH` escapes, kept
  * percent-encoded.
  */
opaque type User = String

object User {
  private val syntax = UriSyntax.token(UriSyntax.UserChars, nonEmpty = true)

  def apply(s: String): Either[String, User] =
    UriSyntax.check("user", syntax, s)

  extension (u: User) def value: String = u

  given Eq[User]   = Eq.fromUniversalEquals
  given Show[User] = Show.show(identity)
}
