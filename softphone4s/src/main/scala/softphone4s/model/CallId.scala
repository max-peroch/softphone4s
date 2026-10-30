package softphone4s.model

import cats.{Eq, Show}
import com.comcast.ip4s.Host

/** The Call-ID of a dialog: `word ["@" word]` (RFC 3261 §25.1). */
opaque type CallId = String

object CallId {
  private val syntax = UriSyntax.token(UriSyntax.CallIdChars, nonEmpty = true)

  def apply(s: String): Either[String, CallId] =
    UriSyntax.check("Call-ID", syntax, s)

  /** `<random hex>@host`, which is always a valid Call-ID. */
  def fromBytes(bytes: Array[Byte], host: Host): CallId =
    s"${UriSyntax.hex(bytes)}@$host"

  extension (c: CallId) def value: String = c

  given Eq[CallId]   = Eq.fromUniversalEquals
  given Show[CallId] = Show.show(identity)
}
