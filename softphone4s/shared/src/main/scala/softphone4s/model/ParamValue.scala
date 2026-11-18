package softphone4s.model

import cats.{Eq, Show}

opaque type ParamValue = String

object ParamValue {
  private val syntax =
    UriSyntax.token(UriSyntax.ParamChars, Emptiness.Forbidden)

  def apply(s: String): Either[String, ParamValue] =
    UriSyntax.check("parameter value", syntax, s)

  extension (v: ParamValue) def value: String = v

  given Eq[ParamValue]   = Eq.fromUniversalEquals
  given Show[ParamValue] = Show.show(identity)
}
