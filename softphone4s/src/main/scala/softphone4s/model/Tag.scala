package softphone4s.model

import cats.{Eq, Show}

/** A `tag` parameter of a From or To header: a token (RFC 3261 §25.1). */
opaque type Tag = String

object Tag {
  private val syntax = UriSyntax.token(UriSyntax.TokenChars, nonEmpty = true)

  def apply(s: String): Either[String, Tag] =
    UriSyntax.check("tag", syntax, s)

  /** A tag of random bytes in hex, which is always a valid token. */
  def fromBytes(bytes: Array[Byte]): Tag = UriSyntax.hex(bytes)

  extension (t: Tag) def value: String = t

  given Eq[Tag]   = Eq.fromUniversalEquals
  given Show[Tag] = Show.show(identity)
}
