package softphone4s.model

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** A SIP call destination, e.g. `1234`, `alice`, or `alice@sip.example.com`.
  */
opaque type Callee = String

object Callee {
  private val valid = "^[A-Za-z0-9+*#.\\-]+(@[A-Za-z0-9.\\-]+(:\\d+)?)?$".r

  /** Validates `s` as a callee, e.g. rejecting empty or malformed strings. */
  def apply(s: String): Either[String, Callee] =
    if valid.matches(s) then Right(s)
    else Left(s"Invalid callee: $s")

  /** Bypasses validation. Only use for values already known to be valid. */
  def unsafe(s: String): Callee = s

  extension (c: Callee) def value: String = c

  given ConfigReader[Callee] = ConfigReader.stringConfigReader.emap(s =>
    Callee(s).left.map(CannotConvert(s, "Callee", _))
  )
}
