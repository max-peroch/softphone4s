package softphone4s.model

import cats.{Order, Show}

/** An RTP payload type: the seven bits in the header that say what the payload
  * is (RFC 3550 §5.1), 0 to 127. Those below 96 are fixed by RFC 3551 or
  * unassigned; 96 to 127 are given a meaning by the SDP of the call.
  */
opaque type PayloadType = Int

object PayloadType {

  /** `number` if it fits in seven bits. */
  def apply(number: Int): Either[String, PayloadType] =
    Either.cond(
      number >= 0 && number <= 127,
      number,
      s"invalid payload type: $number"
    )

  /** The payload type for a number, if there is one. */
  def fromInt(number: Int): Option[PayloadType] = apply(number).toOption

  /** G.711 μ-law (RFC 3551). */
  val Pcmu: PayloadType = 0

  /** G.711 A-law. */
  val Pcma: PayloadType = 8

  /** G.722. */
  val G722: PayloadType = 9

  /** GSM, G.723 and G.729, which RFC 3551 numbers; we do not speak them, but an
    * offer may list them.
    */
  val Gsm: PayloadType  = 3
  val G723: PayloadType = 4
  val G729: PayloadType = 18

  /** Comfort noise at 8 000 a second (RFC 3389, RFC 3551). */
  val ComfortNoise: PayloadType = 13

  /** The number we receive telephone events on (RFC 4733), the one nearly every
    * phone uses.
    */
  val TelephoneEvent: PayloadType = 101

  /** The ones an SDP gives a meaning to, from 96 to 127. */
  val Dynamic: List[PayloadType] = (96 to 127).toList

  extension (p: PayloadType) {
    def value: Int = p
  }

  given Order[PayloadType] = Order.by[PayloadType, Int](identity)
  given Show[PayloadType]  = Show.show(_.toString)
}
