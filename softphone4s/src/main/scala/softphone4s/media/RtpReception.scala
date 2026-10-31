package softphone4s.media

import softphone4s.media.RtcpPacket.ReportBlock

import scala.concurrent.duration.FiniteDuration

/** What we have heard of one RTP source, as RFC 3550 counts it (appendices A.1,
  * A.3 and A.8): the extended highest sequence number, how many packets should
  * have come and how many did, and the interarrival jitter. It is fed every
  * packet as it arrives, before the jitter buffer reorders or drops any, since
  * a report says what the network did and not what was played.
  *
  * A source whose sequence jumps far is taken for a restarted one, and counted
  * afresh.
  */
final case class RtpReception private (
    ssrc: Long,
    clockRate: Int,
    firstSequence: Int,
    highestSequence: Int,
    cycles: Long,
    received: Long,
    expectedAtReport: Long,
    receivedAtReport: Long,
    jitter: Double,
    transit: Long,
    lastSender: Option[RtpReception.SenderHeard]
) {
  import RtpReception.*

  /** The highest sequence number with the wraps counted in the high 16 bits. */
  def extendedHighest: Long = cycles + highestSequence

  /** How many packets should have come since the first. */
  def expected: Long = extendedHighest - firstSequence + 1

  /** Lost since the first; negative if duplicates outnumber the losses. */
  def lost: Long = expected - received

  /** The interarrival jitter in the units of the RTP clock. */
  def jitterUnits: Long = jitter.toLong

  /** A packet of this source with `sequence` and `timestamp`, heard at `at`
    * (any fixed origin of the local clock).
    */
  def hear(sequence: Int, timestamp: Long, at: FiniteDuration): RtpReception = {
    val step = (sequence - highestSequence) & 0xffff
    if step >= MaxDropout && step <= Modulus - MaxMisorder then
      // Too far to be loss or reordering: a source that began again.
      start(ssrc, clockRate, sequence, timestamp, at)
    else {
      val moved =
        if step < MaxDropout && step != 0 then
          copy(
            highestSequence = sequence,
            cycles =
              if sequence < highestSequence then cycles + Modulus else cycles
          )
        else this
      moved.withJitter(timestamp, at).copy(received = received + 1)
    }
  }

  private def withJitter(timestamp: Long, at: FiniteDuration): RtpReception = {
    val arrival = at.toNanos * clockRate / 1_000_000_000L
    val now     = arrival - timestamp
    // The timestamp wraps at 32 bits, and so must the difference.
    val raw  = now - transit
    val diff = ((raw + Half) & Mask) - Half
    copy(jitter = jitter + (math.abs(diff) - jitter) / 16, transit = now)
  }

  /** The sender report this source sent, its NTP time's middle 32 bits, heard
    * at `at`: a report block quotes it back.
    */
  def senderReportHeard(middle: Long, at: FiniteDuration): RtpReception =
    copy(lastSender = Some(SenderHeard(middle, at)))

  /** The block that reports this source at `now`, and the reception after it,
    * from which the next block counts the fraction lost.
    */
  def report(now: FiniteDuration): (RtpReception, ReportBlock) = {
    val expectedInterval = expected - expectedAtReport
    val lostInterval     = expectedInterval - (received - receivedAtReport)
    val fraction         =
      if expectedInterval <= 0 || lostInterval <= 0 then 0
      else math.min(255L, (lostInterval << 8) / expectedInterval).toInt
    val (quoted, delay) = lastSender.fold((0L, 0L))(heard =>
      (
        heard.middle,
        math.max(0L, (now - heard.at).toNanos * 65536L / 1_000_000_000L)
      )
    )
    (
      copy(expectedAtReport = expected, receivedAtReport = received),
      ReportBlock(
        ssrc,
        fraction,
        math
          .max(Int.MinValue.toLong, math.min(Int.MaxValue.toLong, lost))
          .toInt,
        extendedHighest,
        jitterUnits,
        quoted,
        delay
      )
    )
  }
}

object RtpReception {

  /** The middle 32 bits of the NTP time of a sender report, and when it came.
    */
  final case class SenderHeard(middle: Long, at: FiniteDuration)

  private val Modulus     = 1 << 16
  private val MaxDropout  = 3000
  private val MaxMisorder = 100
  private val Half        = 1L << 31
  private val Mask        = 0xffffffffL

  /** The reception that begins with the first packet heard of `ssrc`. */
  def start(
      ssrc: Long,
      clockRate: Int,
      sequence: Int,
      timestamp: Long,
      at: FiniteDuration
  ): RtpReception =
    new RtpReception(
      ssrc,
      clockRate,
      sequence,
      sequence,
      0L,
      1L,
      0L,
      0L,
      0.0,
      at.toNanos * clockRate / 1_000_000_000L - timestamp,
      None
    )
}
