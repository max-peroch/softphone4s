package softphone4s.media

import com.comcast.ip4s.Port

/** The ports a softphone may receive media on, as pairs: an even port for RTP
  * and the odd one after it for RTCP (RFC 3550 §11). `first` is the lowest even
  * port at or above the one asked for, and `last` the highest port of the last
  * pair.
  */
final case class RtpPortRange private (first: Int, last: Int) {

  /** How many pairs there are. */
  def pairs: Int = (last - first + 1) / 2

  /** The RTP port of pair `index`, from 0. */
  def portOf(index: Int): Option[Port] =
    if index >= 0 && index < pairs then Port.fromInt(first + 2 * index)
    else None

  /** The pair `port` is the RTP port of, if it is one. */
  def indexOf(port: Port): Option[Int] =
    Option.when(
      port.value >= first && (port.value - first) % 2 == 0 &&
        (port.value - first) / 2 < pairs
    )((port.value - first) / 2)
}

object RtpPortRange {

  /** The pairs from the first even port at or above `from` up to `to`, if there
    * is one.
    */
  def apply(from: Port, to: Port): Either[String, RtpPortRange] = {
    val first = from.value + from.value % 2
    Either.cond(
      to.value >= first + 1,
      new RtpPortRange(first, to.value - (to.value - first + 1) % 2),
      s"no pair of RTP ports from ${from.value} to ${to.value}"
    )
  }
}
