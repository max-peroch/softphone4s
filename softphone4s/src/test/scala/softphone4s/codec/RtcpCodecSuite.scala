package softphone4s.codec

import fs2.Chunk
import munit.FunSuite
import softphone4s.media.{RtcpError, RtcpPacket}
import softphone4s.media.RtcpPacket.{ReportBlock, SenderInfo}

class RtcpCodecSuite extends FunSuite {

  private val block =
    ReportBlock(0xdeadbeefL, 25, 1234, 0x00010005L, 77, 0x12345678L, 65536)

  private val sender = RtcpPacket.SenderReport(
    0xcafebabeL,
    SenderInfo(0xe0f1a2b3c4d5e6f7L, 160000L, 1000L, 160000L),
    List(block)
  )

  private def roundTrip(packets: List[RtcpPacket]) =
    RtcpCodec.decode(RtcpCodec.encode(packets))

  test("a sender report is read back as it was written") {
    assertEquals(roundTrip(List(sender)), Right(List(sender)))
  }

  test("a receiver report with no blocks, and with several, is read back") {
    val empty = RtcpPacket.ReceiverReport(1L, Nil)
    val many = RtcpPacket.ReceiverReport(2L, List(block, block.copy(ssrc = 9L)))
    assertEquals(roundTrip(List(empty, many)), Right(List(empty, many)))
  }

  test("a loss count below zero keeps its sign across 24 bits") {
    val negative =
      RtcpPacket.ReceiverReport(1L, List(block.copy(cumulativeLost = -5)))
    assertEquals(roundTrip(List(negative)), Right(List(negative)))
  }

  test("a loss count beyond 24 bits is held at the most the wire can say") {
    val huge = RtcpPacket.ReceiverReport(
      1L,
      List(block.copy(cumulativeLost = 10_000_000))
    )
    assertEquals(
      roundTrip(List(huge)),
      Right(
        List(
          RtcpPacket.ReceiverReport(
            1L,
            List(block.copy(cumulativeLost = 0x7fffff))
          )
        )
      )
    )
  }

  test("a source description keeps the names, whatever their length") {
    val names = RtcpPacket.SourceDescription(
      List(1L -> "a", 2L -> "abc", 3L -> "four", 4L -> "softphone@host.example")
    )
    assertEquals(roundTrip(List(names)), Right(List(names)))
  }

  test("a goodbye with and without a reason is read back") {
    val plain  = RtcpPacket.Goodbye(List(1L, 2L), None)
    val reason = RtcpPacket.Goodbye(List(3L), Some("call ended"))
    assertEquals(roundTrip(List(plain, reason)), Right(List(plain, reason)))
  }

  test(
    "a compound packet is a report then the names, and every length is a whole number of words"
  ) {
    val compound = List(
      RtcpPacket.ReceiverReport(1L, List(block)),
      RtcpPacket.SourceDescription(List(1L -> "me@here"))
    )
    val bytes = RtcpCodec.encode(compound)
    assertEquals(bytes.size % 4, 0)
    assertEquals(RtcpCodec.decode(bytes), Right(compound))
  }

  test("a receiver report is laid out as RFC 3550 section 6.4.2 has it") {
    val bytes = RtcpCodec.encode(
      List(RtcpPacket.ReceiverReport(0x01020304L, List(block)))
    )
    val hex = bytes.toArray.map(b => f"${b & 0xff}%02x").mkString
    assertEquals(
      hex,
      "81c90007" + "01020304" + "deadbeef" + "190004d2" + "00010005" + "0000004d" + "12345678" + "00010000"
    )
  }

  test("a packet of a type we do not use is skipped, not refused") {
    val app = Array[Byte](0x80.toByte, 204.toByte, 0, 1, 0, 0, 0, 1)
    val all = Chunk.array(app) ++ RtcpCodec.encode(List(sender))
    assertEquals(
      RtcpCodec.decode(all),
      Right(List(RtcpPacket.Other(204), sender))
    )
  }

  test("padding is taken off before the packet is read") {
    // A goodbye of one source, padded with one word whose last byte says 4.
    val bytes =
      Array[Byte](0xa1.toByte, 203.toByte, 0, 2, 0, 0, 0, 7, 0, 0, 0, 4)
    assertEquals(
      RtcpCodec.decode(Chunk.array(bytes)),
      Right(List(RtcpPacket.Goodbye(List(7L), None)))
    )
  }

  test("a padding count that is zero or too long is refused") {
    val zero =
      Array[Byte](0xa1.toByte, 203.toByte, 0, 2, 0, 0, 0, 7, 0, 0, 0, 0)
    assertEquals(
      RtcpCodec.decode(Chunk.array(zero)),
      Left(RtcpError.BadPadding(203))
    )
  }

  test("the wrong version is refused") {
    val bytes = RtcpCodec.encode(List(sender)).toArray
    bytes(0) = (bytes(0) & 0x3f | 0x40).toByte
    assertEquals(
      RtcpCodec.decode(Chunk.array(bytes)),
      Left(RtcpError.BadVersion(1))
    )
  }

  test("a packet cut short is refused, and so are a few stray bytes") {
    val bytes = RtcpCodec.encode(List(sender))
    assert(RtcpCodec.decode(bytes.take(bytes.size - 4)).isLeft)
    assert(RtcpCodec.decode(Chunk.array(Array[Byte](0x80.toByte, 1))).isLeft)
  }

  test("a block count the packet has no room for is refused") {
    val bytes =
      RtcpCodec.encode(List(RtcpPacket.ReceiverReport(1L, Nil))).toArray
    bytes(0) = (bytes(0) | 3).toByte // claims three blocks
    assertEquals(
      RtcpCodec.decode(Chunk.array(bytes)),
      Left(RtcpError.BadLength(201, 0))
    )
  }

  test("empty input is no packets") {
    assertEquals(RtcpCodec.decode(Chunk.empty), Right(Nil))
  }

  test("more than 31 blocks are cut to 31 and the packet stays valid") {
    val many = RtcpPacket.ReceiverReport(1L, List.fill(40)(block))
    assertEquals(
      roundTrip(List(many)),
      Right(List(RtcpPacket.ReceiverReport(1L, List.fill(31)(block))))
    )
  }

  test("a source description skips items that are not names") {
    // One chunk: ssrc 5, item 2 (NAME) "ab", item 1 (CNAME) "xy", end, padding.
    val bytes = Array[Byte](
      0x81.toByte,
      202.toByte,
      0,
      3,
      0,
      0,
      0,
      5,
      2,
      2,
      'a',
      'b',
      1,
      2,
      'x',
      'y',
      0,
      0,
      0,
      0
    )
    // Length is 5 words: fix the header to match what was written.
    bytes(3) = 4
    assertEquals(
      RtcpCodec.decode(Chunk.array(bytes)),
      Right(List(RtcpPacket.SourceDescription(List(5L -> "xy"))))
    )
  }
}
