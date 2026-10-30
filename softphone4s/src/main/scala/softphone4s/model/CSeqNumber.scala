package softphone4s.model

import cats.{Order, Show}

/** The sequence number of a CSeq header: 0 to 2^31 - 1 (RFC 3261 §8.1.1.5). */
opaque type CSeqNumber = Int

object CSeqNumber {

  def apply(n: Int): Either[String, CSeqNumber] =
    Either.cond(n >= 0, n, s"invalid CSeq number: $n")

  /** The usual first sequence number of a dialog. */
  val One: CSeqNumber = 1

  extension (n: CSeqNumber) {
    def value: Int = n

    /** The following number, saturating at the maximum instead of wrapping. */
    def next: CSeqNumber = if n == Int.MaxValue then n else n + 1
  }

  given Order[CSeqNumber] = Order.by[CSeqNumber, Int](identity)
  given Show[CSeqNumber]  = Show.show(_.toString)
}
