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
}
