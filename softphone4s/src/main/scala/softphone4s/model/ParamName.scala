package softphone4s.model

import cats.{Eq, Show}

/** A URI parameter name, case-insensitive and therefore stored lower-cased. */
opaque type ParamName = String

object ParamName {
  private val syntax = UriSyntax.token(UriSyntax.ParamChars, nonEmpty = true)

  def apply(s: String): Either[String, ParamName] =
    UriSyntax.check("parameter name", syntax, s).map(_.toLowerCase)

  /** Parameters that `SipUri` models as typed fields. */
  private[model] val Transport: ParamName = "transport"
  private[model] val Lr: ParamName        = "lr"

  extension (n: ParamName) def value: String = n

  given Eq[ParamName]   = Eq.fromUniversalEquals
  given Show[ParamName] = Show.show(identity)
}
