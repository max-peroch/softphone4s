package softphone4s.model

import cats.{Eq, Show}

/** The name of a header field not modelled by [[SipHeaders]]: a token, kept as
  * written and compared case-insensitively.
  */
opaque type FieldName = String

object FieldName {
  private val syntax =
    UriSyntax.token(UriSyntax.TokenChars, Emptiness.Forbidden)

  def apply(s: String): Either[String, FieldName] =
    UriSyntax.check("header name", syntax, s)

  val Authorization: FieldName      = "Authorization"
  val ProxyAuthorization: FieldName = "Proxy-Authorization"
  val WwwAuthenticate: FieldName    = "WWW-Authenticate"
  val ProxyAuthenticate: FieldName  = "Proxy-Authenticate"
  val Allow: FieldName              = "Allow"
  val Accept: FieldName             = "Accept"

  extension (n: FieldName) {
    def value: String                   = n
    def matches(other: String): Boolean = n.equalsIgnoreCase(other)
  }

  given Eq[FieldName]   = Eq.instance(_.equalsIgnoreCase(_))
  given Show[FieldName] = Show.show(identity)
}
