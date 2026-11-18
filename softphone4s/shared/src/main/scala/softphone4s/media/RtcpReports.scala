package softphone4s.media

import cats.data.NonEmptyList
import softphone4s.media.RtcpPacket.{ReportBlock, SenderInfo, SourceName}
import softphone4s.model.Ssrc

/** The compound packets we send (RFC 3550 §6.1): a report first, as a sender's
  * if we have sent anything and as a receiver's if not, then who we are.
  */
object RtcpReports {

  /** The packets of a regular report from `ssrc`, which has `sender` to say of
    * itself if it has sent, and `blocks` for what it has heard.
    */
  def regular(
      ssrc: Ssrc,
      sender: Option[SenderInfo],
      blocks: List[ReportBlock],
      name: String
  ): List[RtcpPacket] =
    List(
      sender.fold[RtcpPacket](RtcpPacket.ReceiverReport(ssrc, blocks))(
        RtcpPacket.SenderReport(ssrc, _, blocks)
      ),
      RtcpPacket.SourceDescription(NonEmptyList.one(SourceName(ssrc, name)))
    )

  /** The same, and a goodbye after it, for when we leave. */
  def goodbye(
      ssrc: Ssrc,
      sender: Option[SenderInfo],
      blocks: List[ReportBlock],
      name: String
  ): List[RtcpPacket] =
    regular(ssrc, sender, blocks, name) :+
      RtcpPacket.Goodbye(NonEmptyList.one(ssrc), None)
}
