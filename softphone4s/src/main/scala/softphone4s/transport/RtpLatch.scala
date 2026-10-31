package softphone4s.transport

import com.comcast.ip4s.{IpAddress, SocketAddress}

/** Symmetric RTP (RFC 4961): when the media we receive comes from somewhere
  * other than where the remote party said it would, a NAT has changed its
  * address or port, and the media we send should go back to where theirs comes
  * from. To keep a stray packet from taking the media away, it takes a run of
  * packets from one source, all of one stream (SSRC).
  */
final case class RtpLatch(candidate: Option[RtpLatch.Candidate])

object RtpLatch {

  /** How many packets in a row must come from the new source. */
  val Threshold = 5

  /** A source that is not the target, and how many packets in a row it sent. */
  final case class Candidate(
      source: SocketAddress[IpAddress],
      ssrc: Long,
      packets: Int
  )

  val empty: RtpLatch = RtpLatch(None)

  /** What a packet from `source` does: the new latch, and the address to send
    * to from now on if this packet is the one that moves it. Nothing moves
    * while there is no target, which is while the call is on hold.
    */
  def hear(
      latch: RtpLatch,
      target: Option[SocketAddress[IpAddress]],
      source: SocketAddress[IpAddress],
      ssrc: Long
  ): (RtpLatch, Option[SocketAddress[IpAddress]]) =
    if target.forall(_ == source) then (empty, None)
    else {
      val seen = latch.candidate
        .filter(c => c.source == source && c.ssrc == ssrc)
        .fold(1)(_.packets + 1)
      if seen >= Threshold then (empty, Some(source))
      else (RtpLatch(Some(Candidate(source, ssrc, seen))), None)
    }
}
