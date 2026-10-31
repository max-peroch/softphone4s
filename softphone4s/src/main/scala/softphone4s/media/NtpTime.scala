package softphone4s.media

import scala.concurrent.duration.FiniteDuration

/** The 64-bit time of RFC 3550 §4: seconds since 1900 in the high 32 bits and
  * the fraction of a second, in 2^-32 s, in the low 32.
  */
object NtpTime {

  /** Seconds from 1900-01-01 to the Unix epoch, 1970-01-01. */
  private val EpochOffset = 2_208_988_800L

  private val Nanos = 1_000_000_000L

  /** The time that is `sinceEpoch` after the Unix epoch. The seconds wrap at 32
    * bits, as they do on the wire (in 2036).
    */
  def fromEpoch(sinceEpoch: FiniteDuration): Long = {
    val nanos    = sinceEpoch.toNanos
    val seconds  = (nanos / Nanos + EpochOffset) & 0xffffffffL
    val fraction = nanos % Nanos * (1L << 32) / Nanos
    (seconds << 32) | fraction
  }
}
