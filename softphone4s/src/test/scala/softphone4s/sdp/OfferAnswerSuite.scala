package softphone4s.sdp

import cats.data.NonEmptyList
import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.model.MediaDirection

class OfferAnswerSuite extends FunSuite {

  private val local = LocalMedia(ip"192.0.2.10", port"20000", 7, 8)

  private def crlf(lines: String*): String = lines.mkString("", "\r\n", "\r\n")

  private def parsed(text: String): SessionDescription =
    SessionDescription.parse(text).fold(e => fail(e.message), identity)

  private val opus = Codec("opus", 48000, 2, None, Some("useinbandfec=1"))

  private def offerOf(lines: String*): SessionDescription =
    parsed(
      crlf(
        List(
          "v=0",
          "o=- 1 1 IN IP4 198.51.100.1",
          "s=-",
          "c=IN IP4 198.51.100.1",
          "t=0 0"
        ) ++ lines*
      )
    )

  private def answered(
      offer: SessionDescription,
      caps: Capabilities = Capabilities.Default
  ) =
    OfferAnswer.answer(offer, local, caps).fold(r => fail(r.message), identity)

  test("our offer lists our codecs and telephone events, with the direction") {
    val sdp =
      OfferAnswer.offer(local, Capabilities.Default, MediaDirection.SendRecv)
    assertEquals(
      sdp.render,
      crlf(
        "v=0",
        "o=- 7 8 IN IP4 192.0.2.10",
        "s=-",
        "c=IN IP4 192.0.2.10",
        "t=0 0",
        "m=audio 20000 RTP/AVP 0 101",
        "a=rtpmap:0 PCMU/8000",
        "a=rtpmap:101 telephone-event/8000",
        "a=fmtp:101 0-16",
        "a=sendrecv"
      )
    )
  }

  test(
    "codecs without a static number are given dynamic ones that skip telephone events"
  ) {
    val caps = Capabilities(NonEmptyList.of(opus, Codec.Pcmu), Some(96))
    assertEquals(caps.offered, List(97 -> opus, 0 -> Codec.Pcmu))
    val sdp = OfferAnswer.offer(local, caps, MediaDirection.SendRecv)
    assertEquals(sdp.media.head.formats, List("97", "0", "96"))
    assertEquals(sdp.media.head.fmtp(97), Some("useinbandfec=1"))
    assert(sdp.render.contains("a=rtpmap:97 opus/48000/2"))
  }

  test(
    "the answer takes our first codec among those offered, whatever order they come in"
  ) {
    val caps   = Capabilities(NonEmptyList.of(Codec.Pcma, Codec.Pcmu), None)
    val result = answered(offerOf("m=audio 4000 RTP/AVP 0 8"), caps)
    assertEquals(result.plan.codec, Codec.Pcma)
    assertEquals(result.plan.payloadType, 8)
    assertEquals(result.description.media.head.formats, List("8"))
  }

  test(
    "a codec is found by its rtpmap, with a number of the offerer's choosing"
  ) {
    val caps   = Capabilities(NonEmptyList.of(opus), None)
    val result = answered(
      offerOf("m=audio 4000 RTP/AVP 111 0", "a=rtpmap:111 OPUS/48000/2"),
      caps
    )
    assertEquals(result.plan.payloadType, 111)
    assert(result.description.render.contains("a=rtpmap:111 opus/48000/2"))
  }

  test("a codec with the same name and another clock rate is another codec") {
    val caps = Capabilities(NonEmptyList.of(opus), None)
    assertEquals(
      OfferAnswer
        .answer(
          offerOf("m=audio 4000 RTP/AVP 111", "a=rtpmap:111 opus/16000/2"),
          local,
          caps
        )
        .left
        .toOption,
      Some(Reject.NoCommonCodec)
    )
  }

  test(
    "telephone events are answered only on the payload type we receive them on"
  ) {
    val on101 = answered(
      offerOf("m=audio 4000 RTP/AVP 0 101", "a=rtpmap:101 telephone-event/8000")
    )
    assertEquals(on101.plan.telephoneEventType, Some(101))
    assertEquals(on101.description.media.head.formats, List("0", "101"))
    val on96 = answered(
      offerOf("m=audio 4000 RTP/AVP 0 96", "a=rtpmap:96 telephone-event/8000")
    )
    assertEquals(on96.plan.telephoneEventType, None)
    assertEquals(on96.description.media.head.formats, List("0"))
    val none = answered(offerOf("m=audio 4000 RTP/AVP 0"))
    assertEquals(none.plan.telephoneEventType, None)
  }

  test("the plan says where to send, from the stream's address and port") {
    val result = answered(offerOf("m=audio 4000 RTP/AVP 0"))
    assertEquals(
      result.plan.remote,
      SocketAddress(ip"198.51.100.1", port"4000")
    )
    val own =
      answered(offerOf("m=audio 4000 RTP/AVP 0", "c=IN IP4 203.0.113.4"))
    assertEquals(own.plan.remote, SocketAddress(ip"203.0.113.4", port"4000"))
  }

  test("the answer's direction is the mirror of the offer's") {
    List(
      "sendrecv" -> MediaDirection.SendRecv,
      "sendonly" -> MediaDirection.RecvOnly,
      "recvonly" -> MediaDirection.SendOnly,
      "inactive" -> MediaDirection.Inactive
    ).foreach { (stated, expected) =>
      val result = answered(offerOf("m=audio 4000 RTP/AVP 0", s"a=$stated"))
      assertEquals(result.plan.direction, expected, stated)
      assert(
        result.description.render.contains(s"a=${expected.sdpAttribute}"),
        stated
      )
    }
  }

  test(
    "a hold is an offer the sender will not receive, by direction or by address"
  ) {
    assert(
      answered(offerOf("m=audio 4000 RTP/AVP 0", "a=sendonly")).plan.isHold
    )
    assert(
      answered(offerOf("m=audio 4000 RTP/AVP 0", "a=inactive")).plan.isHold
    )
    val old = parsed(
      crlf(
        "v=0",
        "o=- 1 1 IN IP4 198.51.100.1",
        "s=-",
        "c=IN IP4 0.0.0.0",
        "t=0 0",
        "m=audio 4000 RTP/AVP 0"
      )
    )
    assert(answered(old).plan.isHold)
    assert(!answered(offerOf("m=audio 4000 RTP/AVP 0")).plan.isHold)
  }

  test(
    "a stream we do not use is declined in its place, and the order is kept"
  ) {
    val result = answered(
      offerOf(
        "m=video 5000 RTP/AVP 99",
        "a=rtpmap:99 h263-1998/90000",
        "m=audio 4000 RTP/AVP 0",
        "m=audio 4002 RTP/AVP 0"
      )
    )
    val sections = result.description.media
    assertEquals(
      sections.map(_.kind),
      List(MediaKind.Video, MediaKind.Audio, MediaKind.Audio)
    )
    assertEquals(sections.map(_.port.value), List(0, 20000, 0))
    assertEquals(
      sections.map(_.formats),
      List(List("99"), List("0"), List("0"))
    )
    assertEquals(result.plan.remote.port.value, 4000)
  }

  test(
    "an offer that is only video, or only declined, or only secured, has no audio for us"
  ) {
    List(
      offerOf("m=video 5000 RTP/AVP 99"),
      offerOf("m=audio 0 RTP/AVP 0"),
      offerOf("m=audio 4000 RTP/SAVP 0")
    ).foreach(offer =>
      assertEquals(
        OfferAnswer.answer(offer, local, Capabilities.Default).left.toOption,
        Some(Reject.NoAudio)
      )
    )
  }

  test("no codec in common, and no address, are refused") {
    assertEquals(
      OfferAnswer
        .answer(offerOf("m=audio 4000 RTP/AVP 8"), local, Capabilities.Default)
        .left
        .toOption,
      Some(Reject.NoCommonCodec)
    )
    val noAddress = parsed(
      crlf(
        "v=0",
        "o=- 1 1 IN IP4 198.51.100.1",
        "s=-",
        "t=0 0",
        "m=audio 4000 RTP/AVP 0"
      )
    )
    assertEquals(
      OfferAnswer.answer(noAddress, local, Capabilities.Default).left.toOption,
      Some(Reject.NoAddress)
    )
  }

  test("an answer to our offer picks the first of its codecs that we offered") {
    val caps = Capabilities(NonEmptyList.of(Codec.Pcmu, Codec.Pcma), Some(101))
    val answer = offerOf(
      "m=audio 4000 RTP/AVP 8 0 101",
      "a=rtpmap:101 telephone-event/8000",
      "a=sendrecv"
    )
    val plan = OfferAnswer
      .accept(answer, caps, MediaDirection.SendRecv)
      .fold(r => fail(r.message), identity)
    assertEquals(plan.codec, Codec.Pcma)
    assertEquals(plan.payloadType, 8)
    assertEquals(plan.telephoneEventType, Some(101))
    assertEquals(plan.remote, SocketAddress(ip"198.51.100.1", port"4000"))
    assertEquals(plan.direction, MediaDirection.SendRecv)
  }

  test(
    "an answer with a payload type we did not offer, or the wrong codec on one, is refused"
  ) {
    val caps = Capabilities.Default
    assertEquals(
      OfferAnswer
        .accept(
          offerOf("m=audio 4000 RTP/AVP 8"),
          caps,
          MediaDirection.SendRecv
        )
        .left
        .toOption,
      Some(Reject.NoCommonCodec)
    )
    assertEquals(
      OfferAnswer
        .accept(
          offerOf("m=audio 4000 RTP/AVP 0", "a=rtpmap:0 PCMA/8000"),
          caps,
          MediaDirection.SendRecv
        )
        .left
        .toOption,
      Some(Reject.NoCommonCodec)
    )
  }

  test(
    "an answer that holds us is a hold, and a declined audio stream is none"
  ) {
    val plan = OfferAnswer.accept(
      offerOf("m=audio 4000 RTP/AVP 0", "a=inactive"),
      Capabilities.Default,
      MediaDirection.SendRecv
    )
    assert(plan.exists(_.isHold))
    assertEquals(
      OfferAnswer
        .accept(
          offerOf("m=audio 0 RTP/AVP 0"),
          Capabilities.Default,
          MediaDirection.SendRecv
        )
        .left
        .toOption,
      Some(Reject.NoAudio)
    )
  }
}
