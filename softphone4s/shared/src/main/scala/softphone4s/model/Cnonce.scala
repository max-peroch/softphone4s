package softphone4s.model

import cats.{Eq, Show}

/** The client nonce we send in a Digest response. */
opaque type Cnonce = String

object Cnonce {

  def apply(s: String): Either[String, Cnonce] =
    Either.cond(s.nonEmpty, s, "empty cnonce")

  /** Random bytes in hex, which is always a valid cnonce. */
  def fromBytes(bytes: Array[Byte]): Cnonce = UriSyntax.hex(bytes)

  extension (c: Cnonce) def value: String = c

  given Eq[Cnonce]   = Eq.fromUniversalEquals
  given Show[Cnonce] = Show.show(identity)
}
