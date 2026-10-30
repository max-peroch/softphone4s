package softphone4s.model

import cats.{Eq, Show}

opaque type HeaderValue = String

object HeaderValue {
  private val syntax = UriSyntax.token(UriSyntax.HeaderChars, nonEmpty = false)

  def apply(s: String): Either[String, HeaderValue] =
    UriSyntax.check("URI header value", syntax, s)

  extension (v: HeaderValue) def value: String = v

  given Eq[HeaderValue]   = Eq.fromUniversalEquals
  given Show[HeaderValue] = Show.show(identity)
}
