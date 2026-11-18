package softphone4s.media

/** The fraction of packets lost since the last report, in 256ths (RFC 3550
  * §6.4.1): 0 to 255, one byte on the wire.
  */
opaque type LossFraction = Int

object LossFraction {

  val None: LossFraction = 0

  /** The fraction whose byte is the low eight bits of `bits`. */
  def fromBits(bits: Int): LossFraction = bits & 0xff

  /** `number` if it is a byte. */
  def apply(number: Int): Either[String, LossFraction] =
    Either.cond(
      number >= 0 && number <= 255,
      number,
      s"invalid fraction lost: $number"
    )

  /** `lost` out of `expected`, held to what a byte can say: nothing if none
    * were lost or none expected.
    */
  def of(lost: Long, expected: Long): LossFraction =
    if expected <= 0 || lost <= 0 then 0
    else math.min(255L, (lost << 8) / expected).toInt

  extension (fraction: LossFraction) {

    /** The number of 256ths, 0 to 255. */
    def value: Int = fraction
  }
}
