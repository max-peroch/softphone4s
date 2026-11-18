package softphone4s.model

import cats.{Eq, Show}

opaque type Password = String

object Password {
  private val syntax =
    UriSyntax.token(UriSyntax.PasswordChars, Emptiness.Allowed)

  def apply(s: String): Either[String, Password] =
    UriSyntax.check("password", syntax, s)

  extension (p: Password) def value: String = p

  given Eq[Password] = Eq.fromUniversalEquals
  // A credential never prints itself.
  given Show[Password] = Show.show(_ => "***")
}
