package softphone4s.model

import cats.{Order, Show}

/** The synchronization source of an RTP stream (RFC 3550 §3): 32 bits that name
  * one sender within a session. Kept unsigned in a `Long`.
  */
opaque type Ssrc = Long

object Ssrc {

  /** The source named by the 32 bits of `bits`, however they are signed. */
  def fromBits(bits: Int): Ssrc = bits & 0xffffffffL

  /** `number` if it fits in 32 bits unsigned. */
  def apply(number: Long): Either[String, Ssrc] =
    Either.cond(
      number >= 0 && number <= 0xffffffffL,
      number,
      s"invalid SSRC: $number"
    )

  extension (ssrc: Ssrc) {

    /** The number, 0 to 2^32 − 1. */
    def value: Long = ssrc

    /** The 32 bits as they go on the wire. */
    def bits: Int = ssrc.toInt
  }

  given Order[Ssrc] = Order.by[Ssrc, Long](identity)

  given Show[Ssrc] = Show.show(ssrc => f"0x$ssrc%08x")
}
