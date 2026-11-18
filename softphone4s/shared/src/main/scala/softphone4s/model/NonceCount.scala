package softphone4s.model

import cats.{Eq, Show}

/** How many requests we have sent with one server nonce (`nc`), from 1. */
opaque type NonceCount = Int

object NonceCount {

  val One: NonceCount = 1

  def apply(n: Int): Either[String, NonceCount] =
    Either.cond(n >= 1, n, s"invalid nonce count: $n")

  extension (n: NonceCount) {
    def value: Int       = n
    def next: NonceCount = if n == Int.MaxValue then n else n + 1

    /** Eight lower-case hex digits, as the `nc` parameter is written. */
    def render: String = "%08x".format(n)
  }

  given Eq[NonceCount]   = Eq.fromUniversalEquals
  given Show[NonceCount] = Show.show(_.render)
}
