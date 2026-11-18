package softphone4s.codec

import softphone4s.SipLiterals.pt
import cats.data.NonEmptyList
import fs2.Chunk
import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.model.MediaDirection
import softphone4s.sdp.{
  Capabilities,
  Codec,
  CryptoAttribute,
  LocalCrypto,
  Reject
}
import softphone4s.srtp.{MasterKey, SrtpPolicy, SrtpSuite}

/** The SDP the dialogs use: read through the parser and the offer/answer. */
class SdpCodecSuite extends FunSuite {

  private val caps = Capabilities.Default
  private val ip   = ip"10.0.0.1"
  private val port = port"12000"

  test("buildOffer carries our address, port and every codec of ours") {
    val sdp = SdpCodec.buildOffer(ip, port, caps)
    assert(sdp.contains("c=IN IP4 10.0.0.1"), sdp)
    assert(sdp.contains("m=audio 12000 RTP/AVP 0 8 101"), sdp)
    assert(sdp.contains("a=rtpmap:8 PCMA/8000"), sdp)
  }

  test("buildOffer uses CRLF line endings") {
    val sdp = SdpCodec.buildOffer(ip, port, caps)
    assert(sdp.contains("\r\n"), "SDP must use CRLF line endings")
    assert(!sdp.contains("\n\n"), "SDP must not have bare LF-only newlines")
  }

  private val offerSdp =
    "v=0\r\no=- 1 1 IN IP4 192.0.2.5\r\ns=-\r\nc=IN IP4 192.0.2.5\r\nt=0 0\r\n" +
      "m=audio 40000 RTP/AVP 0 8 101\r\na=rtpmap:0 PCMU/8000\r\n" +
      "a=rtpmap:101 telephone-event/8000\r\n"

  test("parseOffer reads the endpoint, payload types and telephone-event") {
    SdpCodec.parseOffer(offerSdp, caps) match {
      case Some(offer) =>
        assertEquals(offer.ip, ip"192.0.2.5")
        assertEquals(offer.port, port"40000")
        assertEquals(offer.payloadTypes, List(pt(0), pt(8), pt(101)))
        assert(offer.offersTelephoneEvent)
      case None => fail("expected an offer")
    }
  }

  test("an offer is acceptable when a codec of ours is in it, in any order") {
    val pcmaOnly = offerSdp.replace("RTP/AVP 0 8 101", "RTP/AVP 8")
    assert(SdpCodec.parseOffer(pcmaOnly, caps).isDefined)
    assertEquals(
      SdpCodec.parseOffer(
        pcmaOnly,
        Capabilities(NonEmptyList.one(Codec.Pcmu), None)
      ),
      None
    )
    assertEquals(
      SdpCodec
        .parseOffer(offerSdp.replace("RTP/AVP 0 8 101", "RTP/AVP 18"), caps),
      None
    )
  }

  test("telephone-event counts only when 101 is offered and mapped to it") {
    val noMapping =
      offerSdp.replace("a=rtpmap:101 telephone-event/8000\r\n", "")
    assert(!SdpCodec.parseOffer(noMapping, caps).get.offersTelephoneEvent)
    val other =
      offerSdp.replace("0 8 101", "0 8 96").replace("rtpmap:101", "rtpmap:96")
    assert(!SdpCodec.parseOffer(other, caps).get.offersTelephoneEvent)
  }

  test("parseOffer is None without an audio stream or without an address") {
    assertEquals(SdpCodec.parseOffer("v=0\r\nc=IN IP4 1.2.3.4\r\n", caps), None)
    assertEquals(
      SdpCodec.parseOffer("v=0\r\nm=audio 4000 RTP/AVP 0\r\n", caps),
      None
    )
    assertEquals(SdpCodec.parseOffer("", caps), None)
  }

  test(
    "the answer stays within the offer: our first codec, telephone-event only if offered"
  ) {
    val both =
      SdpCodec.buildAnswerTo(
        SdpCodec.parseOffer(offerSdp, caps).get,
        ip,
        port,
        caps
      )
    assert(both.contains("m=audio 12000 RTP/AVP 0 101"), both)
    assert(both.contains("a=rtpmap:101 telephone-event/8000"), both)
    assert(both.contains("c=IN IP4 10.0.0.1"), both)
    val bare = SdpCodec.buildAnswerTo(
      SdpCodec.parseOffer(offerSdp.replace("0 8 101", "0 8"), caps).get,
      ip,
      port,
      caps
    )
    assert(bare.contains("m=audio 12000 RTP/AVP 0\r\n"), bare)
    assert(!bare.contains("telephone-event"), bare)
    assert(!bare.contains("\n\n") && bare.contains("\r\n"))
  }

  private def offerWith(extra: String, address: String = "192.168.1.2") =
    SdpCodec
      .parseOffer(
        s"v=0\r\no=- 1 1 IN IP4 $address\r\ns=-\r\nc=IN IP4 $address\r\nt=0 0\r\nm=audio 20000 RTP/AVP 0\r\n$extra",
        caps
      )
      .getOrElse(fail("unparseable"))

  test("the direction of an offer is read, defaulting to sendrecv") {
    import MediaDirection.*
    assertEquals(offerWith("").direction, SendRecv)
    assertEquals(offerWith("a=sendonly\r\n").direction, SendOnly)
    assertEquals(offerWith("a=recvonly\r\n").direction, RecvOnly)
    assertEquals(offerWith("a=inactive\r\n").direction, Inactive)
  }

  test(
    "an offer is a hold if its sender will not receive, or the address is 0.0.0.0"
  ) {
    assert(offerWith("a=sendonly\r\n").isHold)
    assert(offerWith("a=inactive\r\n").isHold)
    assert(offerWith("", address = "0.0.0.0").isHold)
    assert(!offerWith("a=recvonly\r\n").isHold)
    assert(!offerWith("").isHold)
  }

  test("the answer mirrors the direction of the offer (RFC 3264 §6.1)") {
    List(
      "sendonly" -> "recvonly",
      "recvonly" -> "sendonly",
      "inactive" -> "inactive",
      "sendrecv" -> "sendrecv"
    ).foreach { (offered, answered) =>
      val answer =
        SdpCodec.buildAnswerTo(offerWith(s"a=$offered\r\n"), ip, port, caps)
      assert(answer.contains(s"a=$answered\r\n"), answer)
    }
  }

  test(
    "the plan from an answer to our offer is the codec it chose, where to send and how"
  ) {
    val plan = SdpCodec
      .planFromAnswer(
        "v=0\r\no=- 1 1 IN IP4 192.0.2.5\r\ns=-\r\nc=IN IP4 192.0.2.5\r\nt=0 0\r\nm=audio 40000 RTP/AVP 8 101\r\na=rtpmap:101 telephone-event/8000\r\n",
        caps,
        MediaDirection.SendRecv
      )
      .getOrElse(fail("no plan"))
    assertEquals(plan.codec, Codec.Pcma)
    assertEquals(plan.payloadType, pt(8))
    assertEquals(plan.telephoneEventType, Some(pt(101)))
    assertEquals(plan.remote.port, port"40000")
  }

  test("an answer that is not SDP, or has nothing we offered, gives no plan") {
    assertEquals(
      SdpCodec
        .planFromAnswer("not sdp", caps, MediaDirection.SendRecv)
        .left
        .toOption,
      Some(Reject.NoAudio)
    )
    assertEquals(
      SdpCodec
        .planFromAnswer(
          "v=0\r\no=- 1 1 IN IP4 192.0.2.5\r\ns=-\r\nc=IN IP4 192.0.2.5\r\nt=0 0\r\nm=audio 40000 RTP/AVP 18\r\n",
          caps,
          MediaDirection.SendRecv
        )
        .left
        .toOption,
      Some(Reject.NoCommonCodec)
    )
  }

  test(
    "a secure stream is an offer for a phone that encrypts, and none for one that does not"
  ) {
    val secure =
      "v=0\r\no=- 1 1 IN IP4 10.0.0.9\r\ns=-\r\nc=IN IP4 10.0.0.9\r\nt=0 0\r\n" +
        "m=audio 4000 RTP/SAVP 0\r\n" +
        "a=crypto:1 AES_CM_128_HMAC_SHA1_80 inline:d0RmdmcmVCspeEc3QGZiNWpVLFJhQX1cfHAwJSoj\r\n"
    val ourKey = MasterKey
      .fromBytes(Chunk.array(Array.fill[Byte](30)(7)))
      .fold(e => fail(e.message), identity)
    val keyed = caps.copy(crypto =
      LocalCrypto(
        SrtpPolicy.Optional,
        List(CryptoAttribute(1, SrtpSuite.AesCm128HmacSha1_80, ourKey))
      )
    )
    assert(SdpCodec.parseOffer(secure, keyed).isDefined)
    assert(SdpCodec.parseOffer(secure, caps).isEmpty)
  }
}
