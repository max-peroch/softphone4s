package softphone4s.model

import cats.{Eq, Show}

/** The value of a header parameter (e.g. on Via or Contact): a token, a host,
  * or a quoted string, kept as written (quotes included).
  */
opaque type HeaderParamValue = String

object HeaderParamValue {
  // A token or host, or a quoted string; escapes are not `%HH` here.
  private val unquoted =
    new UriSyntax.Syntax(
      UriSyntax.ParamValueChars,
      Emptiness.Forbidden,
      Escapes.Literal
    )

  def apply(s: String): Either[String, HeaderParamValue] =
    Either.cond(
      unquoted.matches(s) || UriSyntax.isQuotedString(s),
      s,
      s"invalid header parameter value: '$s'"
    )

  /** A number as a parameter value, which is always a valid token. */
  def fromInt(n: Int): HeaderParamValue = n.toString

  extension (v: HeaderParamValue) def value: String = v

  given Eq[HeaderParamValue]   = Eq.fromUniversalEquals
  given Show[HeaderParamValue] = Show.show(identity)
}
