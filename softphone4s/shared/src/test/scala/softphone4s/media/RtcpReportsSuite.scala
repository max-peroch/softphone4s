package softphone4s.media

import munit.FunSuite
import softphone4s.media.RtcpPacket.{ReportBlock, SenderInfo}
import softphone4s.model.Ssrc

class RtcpReportsSuite extends FunSuite {

  private val me   = Ssrc.fromBits(1)
  private val them = Ssrc.fromBits(2)

  private val block =
    ReportBlock(them, LossFraction.None, 0, 100L, 3L, 0L, 0L)

  private val info = SenderInfo(1L << 32, 160L, 10L, 1600L)

  test("a party that has sent reports as a sender, with who it is") {
    val packets = RtcpReports.regular(me, Some(info), List(block), "me@here")
    assertEquals(packets.size, 2)
    assertEquals(
      packets.head,
      RtcpPacket.SenderReport(me, info, List(block))
    )
    packets(1) match {
      case RtcpPacket.SourceDescription(names) =>
        assertEquals(names.head.name, "me@here")
        assertEquals(names.head.ssrc, me)
      case other => fail(s"expected a description: $other")
    }
  }

  test("a party that has not sent reports as a receiver") {
    val packets = RtcpReports.regular(me, None, List(block), "me@here")
    assertEquals(packets.head, RtcpPacket.ReceiverReport(me, List(block)))
  }

  test("a report with nothing heard has no blocks") {
    val packets = RtcpReports.regular(me, None, Nil, "me@here")
    assertEquals(packets.head, RtcpPacket.ReceiverReport(me, Nil))
  }

  test(
    "a goodbye is the regular report and then the goodbye, which comes last"
  ) {
    val packets = RtcpReports.goodbye(me, Some(info), List(block), "me@here")
    assertEquals(
      packets.init,
      RtcpReports.regular(me, Some(info), List(block), "me@here")
    )
    packets.last match {
      case RtcpPacket.Goodbye(sources, reason) =>
        assertEquals(sources.toList, List(me))
        assertEquals(reason, None)
      case other => fail(s"expected a goodbye: $other")
    }
  }
}
