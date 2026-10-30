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

  /** Header parameters that Via and NameAddr model as typed fields. */
  private[model] val Branch: ParamName   = "branch"
  private[model] val RPort: ParamName    = "rport"
  private[model] val Received: ParamName = "received"
  private[model] val TagParam: ParamName = "tag"

  extension (n: ParamName) def value: String = n

  given Eq[ParamName]   = Eq.fromUniversalEquals
  given Show[ParamName] = Show.show(identity)
}
