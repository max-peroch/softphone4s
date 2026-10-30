package softphone4s.model

import cats.syntax.all.*
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** A SIP call destination. Either the short form, `1234`, `alice` or
  * `alice@sip.example.com`, or a whole SIP URI such as `sips:alice@example.com`
  * or `sip:alice@example.com;transport=tcp`, which is how to ask for a
  * transport.
  */
opaque type Callee = String

object Callee {
  private val short  = "^[A-Za-z0-9+*#.\\-]+(@[A-Za-z0-9.\\-]+(:\\d+)?)?$".r
  private val Scheme = "(?i)^sips?:.*".r

  /** Validates `s` as a callee, e.g. rejecting empty or malformed strings. */
  def apply(s: String): Either[String, Callee] =
    if Scheme.matches(s) then
      SipUri.parse(s).left.map(e => s"Invalid callee: $s ($e)").as(s)
    else if short.matches(s) then Right(s)
    else Left(s"Invalid callee: $s")

  /** Bypasses validation. Only use for values already known to be valid. */
  def unsafe(s: String): Callee = s

  extension (c: Callee) {
    def value: String = c

    /** The URI to dial: a whole URI as given; otherwise the callee as given
      * when it names a host (`alice@sip.example.com`), else at `realm`. A `#`
      * is percent-encoded, as the user part of a SIP URI requires.
      */
    def toSipUri(realm: String): Either[SoftphoneError, SipUri] =
      if Scheme.matches(c) then
        SipUri.parse(c).left.map(SoftphoneError.InvalidSipUri(c, _))
      else {
        val user  = c.replace("#", "%23")
        val input =
          if user.contains('@') then s"sip:$user" else s"sip:$user@$realm"
        SipUri.parse(input).left.map(SoftphoneError.InvalidSipUri(input, _))
      }
  }

  given ConfigReader[Callee] = ConfigReader.stringConfigReader.emap(s =>
    Callee(s).left.map(CannotConvert(s, "Callee", _))
  )
}
