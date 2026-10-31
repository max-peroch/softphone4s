package softphone4s.media

import cats.data.NonEmptyList
import softphone4s.model.Ssrc

/** The RTCP packets of RFC 3550 that a softphone makes and reads. Fields hold
  * what is on the wire; a packet of a type we do not use is not one of them,
  * and is skipped when a compound packet is read.
  */
enum RtcpPacket {

  /** What a sender says of itself and what it has heard of the others. */
  case SenderReport(
      ssrc: Ssrc,
      info: RtcpPacket.SenderInfo,
      blocks: List[RtcpPacket.ReportBlock]
  )

  /** What a receiver that is not sending has heard. */
  case ReceiverReport(ssrc: Ssrc, blocks: List[RtcpPacket.ReportBlock])

  /** Who the SSRCs are: the canonical name of each (SDES item CNAME). */
  case SourceDescription(names: NonEmptyList[RtcpPacket.SourceName])

  /** The sources leave, and perhaps why. */
  case Goodbye(ssrcs: NonEmptyList[Ssrc], reason: Option[String])
}

object RtcpPacket {

  /** The most blocks, or sources, a packet counts (5 bits). */
  val MaxCount: Int = 31

  /** The canonical name of a source, like `user@host`. */
  final case class SourceName(ssrc: Ssrc, name: String)

  /** @param ntp
    *   wall-clock time as 32.32 fixed point seconds since 1900.
    * @param rtpTimestamp
    *   the same instant on the stream's clock.
    * @param packets
    *   sent so far (32 bits, wrapping).
    * @param octets
    *   bytes of payload sent so far (32 bits, wrapping).
    */
  final case class SenderInfo(
      ntp: Long,
      rtpTimestamp: Long,
      packets: Long,
      octets: Long
  ) {

    /** The middle 32 bits of the NTP time, which a receiver quotes back. */
    def middle: Long = (ntp >>> 16) & 0xffffffffL
  }

  /** What one receiver heard of one source.
    *
    * @param cumulativeLost
    *   lost since the start; negative when duplicates outnumber losses. 24 bits
    *   on the wire.
    * @param highestSequence
    *   the highest sequence number received, with the wraps counted in the high
    *   16 bits.
    * @param jitter
    *   the interarrival jitter, in RTP clock units.
    * @param lastSender
    *   the middle 32 bits of the NTP time of the last sender report from this
    *   source, or 0.
    * @param delaySinceLastSender
    *   the time since it, in 65536ths of a second, or 0.
    */
  final case class ReportBlock(
      ssrc: Ssrc,
      fractionLost: LossFraction,
      cumulativeLost: Int,
      highestSequence: Long,
      jitter: Long,
      lastSender: Long,
      delaySinceLastSender: Long
  )
}
