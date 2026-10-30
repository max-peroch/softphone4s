package softphone4s.model

import cats.{Eq, Show}

/** The value of a header parameter (e.g. on Via or Contact): a token, a host,
  * or a quoted string, kept as written (quotes included).
  */
opaque type HeaderParamValue = String

object HeaderParamValue {
  private val syntax =
    s"""(?:[${UriSyntax.TokenChars}\\[\\]:]+|"(?:[^"\\\\]|\\\\.)*")""".r

  def apply(s: String): Either[String, HeaderParamValue] =
    UriSyntax.check("header parameter value", syntax, s)

  extension (v: HeaderParamValue) def value: String = v

  given Eq[HeaderParamValue]   = Eq.fromUniversalEquals
  given Show[HeaderParamValue] = Show.show(identity)
}
