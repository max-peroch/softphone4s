package softphone4s.media

/** The RTCP packets of RFC 3550 that a softphone makes and reads. Fields hold
  * what is on the wire, as unsigned values widened to `Long` where 32 bits are
  * used.
  */
enum RtcpPacket {

  /** What a sender says of itself and what it has heard of the others. */
  case SenderReport(
      ssrc: Long,
      info: RtcpPacket.SenderInfo,
      blocks: List[RtcpPacket.ReportBlock]
  )

  /** What a receiver that is not sending has heard. */
  case ReceiverReport(ssrc: Long, blocks: List[RtcpPacket.ReportBlock])

  /** Who the SSRCs are: the canonical name of each (SDES item CNAME). */
  case SourceDescription(names: List[(Long, String)])

  /** The sources leave, and perhaps why. */
  case Goodbye(ssrcs: List[Long], reason: Option[String])

  /** A packet of a type we do not use, which is skipped. */
  case Other(packetType: Int)
}

object RtcpPacket {

  /** The most blocks, or sources, a packet counts (5 bits). */
  val MaxCount: Int = 31

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
    * @param fractionLost
    *   0 to 255: lost since the last report, in 256ths.
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
      ssrc: Long,
      fractionLost: Int,
      cumulativeLost: Int,
      highestSequence: Long,
      jitter: Long,
      lastSender: Long,
      delaySinceLastSender: Long
  )
}
