package softphone4s.model

import softphone4s.model.Ssrc
import munit.FunSuite
import softphone4s.codec.RtpCodec
import softphone4s.sdp.{RtpMap, SessionDescription}

import fs2.Chunk

class PayloadTypeSuite extends FunSuite {

  test("a payload type fits in seven bits") {
    assertEquals(PayloadType(0).map(_.value), Right(0))
    assertEquals(PayloadType(127).map(_.value), Right(127))
    assert(PayloadType(-1).isLeft)
    assert(PayloadType(128).isLeft)
    assertEquals(PayloadType.fromInt(200), None)
  }

  test("the fixed ones are the numbers of RFC 3551") {
    assertEquals(
      List(
        PayloadType.Pcmu,
        PayloadType.Gsm,
        PayloadType.G723,
        PayloadType.Pcma,
        PayloadType.G722,
        PayloadType.G729
      ).map(_.value),
      List(0, 3, 4, 8, 9, 18)
    )
    assertEquals(PayloadType.TelephoneEvent.value, 101)
  }

  test("the dynamic ones are 96 to 127") {
    assertEquals(PayloadType.Dynamic.map(_.value), (96 to 127).toList)
  }

  test("payload types are ordered by number") {
    assertEquals(
      List(PayloadType.G722, PayloadType.Pcmu, PayloadType.Pcma).sorted(using
        cats.kernel.Order.catsKernelOrderingForOrder[PayloadType]
      ),
      List(PayloadType.Pcmu, PayloadType.Pcma, PayloadType.G722)
    )
  }

  test("an rtpmap for a number that is not a payload type is not read") {
    assertEquals(RtpMap.parse("200 PCMU/8000"), None)
    assertEquals(
      RtpMap.parse("96 opus/48000/2").map(_.payloadType.value),
      Some(96)
    )
  }

  test(
    "a format that is not a payload type is left out of the ones an m= line has"
  ) {
    val sdp = SessionDescription
      .parse(
        "v=0\r\no=- 1 1 IN IP4 192.0.2.1\r\ns=-\r\nc=IN IP4 192.0.2.1\r\nt=0 0\r\n" +
          "m=audio 4000 RTP/AVP 0 200 8 x 101\r\n"
      )
      .fold(e => fail(e.message), identity)
    assertEquals(sdp.media.head.payloadTypes.map(_.value), List(0, 8, 101))
  }

  test("a header read from the wire always has a payload type, from 0 to 127") {
    (0 to 127).foreach { n =>
      val packet = RtpCodec.encode(
        PayloadType(n).fold(e => fail(e), identity),
        1,
        0L,
        Ssrc.fromBits(1),
        Chunk.array(Array[Byte](1, 2, 3)),
        marker = n % 2 == 0
      )
      val (header, payload) =
        RtpCodec.decode(packet).getOrElse(fail(s"not RTP: $n"))
      assertEquals(header.payloadType.value, n)
      assertEquals(header.marker, n % 2 == 0)
      assertEquals(payload.toList, List[Byte](1, 2, 3))
    }
  }
}
