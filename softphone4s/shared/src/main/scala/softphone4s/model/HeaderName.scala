package softphone4s.model

import cats.{Eq, Show}

opaque type HeaderName = String

object HeaderName {
  private val syntax =
    UriSyntax.token(UriSyntax.HeaderChars, Emptiness.Forbidden)

  def apply(s: String): Either[String, HeaderName] =
    UriSyntax.check("URI header name", syntax, s)

  extension (n: HeaderName) def value: String = n

  given Eq[HeaderName]   = Eq.fromUniversalEquals
  given Show[HeaderName] = Show.show(identity)
}
