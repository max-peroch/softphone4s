package softphone4s.model

import cats.{Eq, Show}

/** A server nonce of a Digest challenge: non-empty text. */
opaque type Nonce = String

object Nonce {

  def apply(s: String): Either[String, Nonce] =
    Either.cond(s.nonEmpty, s, "empty nonce")

  extension (n: Nonce) def value: String = n

  given Eq[Nonce]   = Eq.fromUniversalEquals
  given Show[Nonce] = Show.show(identity)
}
