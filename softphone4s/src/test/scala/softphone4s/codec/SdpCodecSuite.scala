package softphone4s.codec

import munit.FunSuite
import com.comcast.ip4s.*

class SdpCodecSuite extends FunSuite {

  private val defaultIp   = IpAddress.fromString("127.0.0.1").get
  private val defaultPort = Port.fromInt(5000).get

  private val validSdp =
    "v=0\r\no=- 0 0 IN IP4 192.168.1.2\r\ns=-\r\nc=IN IP4 192.168.1.2\r\nt=0 0\r\nm=audio 20000 RTP/AVP 0\r\n"

  test("parseAudioEndpoint extracts IP and port from valid SDP") {
    val (ip, port) =
      SdpCodec.parseAudioEndpoint(validSdp, defaultIp, defaultPort)
    assertEquals(ip, IpAddress.fromString("192.168.1.2").get)
    assertEquals(port, Port.fromInt(20000).get)
  }

  test("parseAudioEndpoint falls back to defaultIp when c= line is absent") {
    val sdp     = "v=0\r\nm=audio 20000 RTP/AVP 0\r\n"
    val (ip, _) = SdpCodec.parseAudioEndpoint(sdp, defaultIp, defaultPort)
    assertEquals(ip, defaultIp)
  }

  test("parseAudioEndpoint falls back to defaultPort when m= line is absent") {
    val sdp       = "v=0\r\nc=IN IP4 192.168.1.2\r\n"
    val (_, port) = SdpCodec.parseAudioEndpoint(sdp, defaultIp, defaultPort)
    assertEquals(port, defaultPort)
  }

  test("buildOffer contains the local IP and RTP port") {
    val localIp   = IpAddress.fromString("10.0.0.1").get
    val localPort = Port.fromInt(12000).get
    val sdp       = SdpCodec.buildOffer(localIp, localPort)
    assert(sdp.contains("c=IN IP4 10.0.0.1"), s"got: $sdp")
    assert(sdp.contains("m=audio 12000"), s"got: $sdp")
  }

  test("buildOffer uses CRLF line endings") {
    val sdp = SdpCodec.buildOffer(
      IpAddress.fromString("10.0.0.1").get,
      Port.fromInt(5000).get
    )
    assert(sdp.contains("\r\n"), "SDP must use CRLF line endings")
    assert(!sdp.contains("\n\n"), "SDP must not have bare LF-only newlines")
  }

  private val offerSdp =
    "v=0\r\no=- 1 1 IN IP4 192.0.2.5\r\ns=-\r\nc=IN IP4 192.0.2.5\r\nt=0 0\r\n" +
      "m=audio 40000 RTP/AVP 0 8 101\r\na=rtpmap:0 PCMU/8000\r\n" +
      "a=rtpmap:101 telephone-event/8000\r\n"

  test("parseOffer reads the endpoint, payload types and telephone-event") {
    SdpCodec.parseOffer(offerSdp) match {
      case Some(offer) =>
        assertEquals(offer.ip, IpAddress.fromString("192.0.2.5").get)
        assertEquals(offer.port, Port.fromInt(40000).get)
        assertEquals(offer.payloadTypes, List(0, 8, 101))
        assert(offer.offersTelephoneEvent)
        assert(offer.isAcceptable)
      case None => fail("expected an offer")
    }
  }

  test("an offer without PCMU is not acceptable") {
    val offer =
      SdpCodec.parseOffer(offerSdp.replace("RTP/AVP 0 8 101", "RTP/AVP 8")).get
    assert(!offer.isAcceptable)
  }

  test("telephone-event counts only when 101 is offered and mapped to it") {
    val noMapping =
      offerSdp.replace("a=rtpmap:101 telephone-event/8000\r\n", "")
    assert(!SdpCodec.parseOffer(noMapping).get.offersTelephoneEvent)
    val other =
      offerSdp.replace("0 8 101", "0 8 96").replace("rtpmap:101", "rtpmap:96")
    assert(!SdpCodec.parseOffer(other).get.offersTelephoneEvent)
  }

  test("parseOffer is None without an audio stream or without an address") {
    assertEquals(SdpCodec.parseOffer("v=0\r\nc=IN IP4 1.2.3.4\r\n"), None)
    assertEquals(SdpCodec.parseOffer("v=0\r\nm=audio 4000 RTP/AVP 0\r\n"), None)
    assertEquals(SdpCodec.parseOffer(""), None)
  }

  test(
    "the answer stays within the offer: PCMU, and telephone-event only if offered"
  ) {
    val ip   = IpAddress.fromString("10.0.0.1").get
    val port = Port.fromInt(12000).get
    val both =
      SdpCodec.buildAnswerTo(SdpCodec.parseOffer(offerSdp).get, ip, port)
    assert(both.contains("m=audio 12000 RTP/AVP 0 101"), both)
    assert(both.contains("a=rtpmap:101 telephone-event/8000"), both)
    assert(both.contains("c=IN IP4 10.0.0.1"), both)
    val bare = SdpCodec.buildAnswerTo(
      SdpCodec.parseOffer(offerSdp.replace("0 8 101", "0 8")).get,
      ip,
      port
    )
    assert(bare.contains("m=audio 12000 RTP/AVP 0\r\n"), bare)
    assert(!bare.contains("telephone-event"), bare)
    assert(!bare.contains("\n\n") && bare.contains("\r\n"))
  }

  private def offerWith(extra: String, ip: String = "192.168.1.2") =
    SdpCodec
      .parseOffer(
        s"v=0\r\no=- 1 1 IN IP4 $ip\r\ns=-\r\nc=IN IP4 $ip\r\nt=0 0\r\nm=audio 20000 RTP/AVP 0\r\n$extra"
      )
      .getOrElse(fail("unparseable"))

  test("the direction of an offer is read, defaulting to sendrecv") {
    import softphone4s.model.MediaDirection.*
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
    assert(offerWith("", ip = "0.0.0.0").isHold)
    assert(!offerWith("a=recvonly\r\n").isHold)
    assert(!offerWith("").isHold)
  }

  test("the answer mirrors the direction of the offer (RFC 3264 §6.1)") {
    val ip = IpAddress.fromString("10.0.0.1").get
    List(
      "sendonly" -> "recvonly",
      "recvonly" -> "sendonly",
      "inactive" -> "inactive",
      "sendrecv" -> "sendrecv"
    )
      .foreach { (offered, answered) =>
        val answer =
          SdpCodec.buildAnswerTo(offerWith(s"a=$offered\r\n"), ip, defaultPort)
        assert(answer.contains(s"a=$answered\r\n"), answer)
      }
  }
}
