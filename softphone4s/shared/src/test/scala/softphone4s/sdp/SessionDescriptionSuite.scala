package softphone4s.sdp

import softphone4s.SipLiterals.pt
import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.model.MediaDirection

class SessionDescriptionSuite extends FunSuite {

  private def parsed(text: String): SessionDescription =
    SessionDescription.parse(text).fold(e => fail(e.message), identity)

  private def crlf(lines: String*): String = lines.mkString("", "\r\n", "\r\n")

  /** The example of RFC 4566 §5, with a video stream added. */
  private val example = crlf(
    "v=0",
    "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5",
    "s=SDP Seminar",
    "i=A Seminar on the session description protocol",
    "c=IN IP4 224.2.17.12/127",
    "t=2873397496 2873404696",
    "a=recvonly",
    "m=audio 49170 RTP/AVP 0",
    "m=video 51372 RTP/AVP 99",
    "c=IN IP4 203.0.113.9",
    "a=rtpmap:99 h263-1998/90000"
  )

  test("the session and each media section are read") {
    val sdp = parsed(example)
    assertEquals(sdp.origin.username, "jdoe")
    assertEquals(sdp.origin.sessionId, BigInt("2890844526"))
    assertEquals(sdp.origin.sessionVersion, BigInt("2890842807"))
    assertEquals(sdp.name, "SDP Seminar")
    assertEquals(sdp.connection, Some(ip"224.2.17.12"))
    assertEquals(sdp.media.map(_.kind), List(MediaKind.Audio, MediaKind.Video))
    assertEquals(sdp.media.map(_.port.value), List(49170, 51372))
    assertEquals(
      sdp.media.map(_.protocol),
      List(MediaProtocol.RtpAvp, MediaProtocol.RtpAvp)
    )
    assertEquals(sdp.media(1).connection, Some(ip"203.0.113.9"))
  }

  test("a media section has its own address, else the session's") {
    val sdp = parsed(example)
    assertEquals(sdp.addressOf(sdp.media(0)), Some(ip"224.2.17.12"))
    assertEquals(sdp.addressOf(sdp.media(1)), Some(ip"203.0.113.9"))
  }

  test("a direction is the section's, else the session's, else sendrecv") {
    val sdp = parsed(example)
    assertEquals(sdp.directionOf(sdp.media(0)), MediaDirection.RecvOnly)
    val own = parsed(
      example.replace(
        "m=audio 49170 RTP/AVP 0",
        "m=audio 49170 RTP/AVP 0\r\na=sendonly"
      )
    )
    assertEquals(own.directionOf(own.media(0)), MediaDirection.SendOnly)
    val none = parsed(example.replace("a=recvonly\r\n", ""))
    assertEquals(none.directionOf(none.media(0)), MediaDirection.SendRecv)
  }

  test("an address of 0.0.0.0 is a hold, whatever the direction says") {
    val sdp = parsed(
      example
        .replace("224.2.17.12/127", "0.0.0.0")
        .replace("a=recvonly", "a=sendrecv")
    )
    assertEquals(sdp.directionOf(sdp.media(0)), MediaDirection.Inactive)
  }

  test(
    "rtpmap and fmtp are read, and a payload type with neither has its static meaning"
  ) {
    val sdp = parsed(
      crlf(
        "v=0",
        "o=- 1 1 IN IP4 192.0.2.1",
        "s=-",
        "c=IN IP4 192.0.2.1",
        "t=0 0",
        "m=audio 4000 RTP/AVP 0 8 96 101",
        "a=rtpmap:96 opus/48000/2",
        "a=fmtp:96 minptime=10;useinbandfec=1",
        "a=rtpmap:101 telephone-event/8000"
      )
    )
    val section = sdp.media.head
    assertEquals(
      section.meanings,
      List(
        RtpMap(pt(0), "PCMU", 8000, None),
        RtpMap(pt(8), "PCMA", 8000, None),
        RtpMap(pt(96), "opus", 48000, Some(2)),
        RtpMap(pt(101), "telephone-event", 8000, None)
      )
    )
    assertEquals(section.fmtp(pt(96)), Some("minptime=10;useinbandfec=1"))
    assertEquals(section.fmtp(pt(0)), None)
  }

  test("IPv6 addresses are read and printed with their address type") {
    val text = crlf(
      "v=0",
      "o=- 1 1 IN IP6 2001:db8::1",
      "s=-",
      "c=IN IP6 2001:db8::1",
      "t=0 0",
      "m=audio 4000 RTP/AVP 0"
    )
    val sdp = parsed(text)
    assertEquals(sdp.connection, Some(ip"2001:db8::1"))
    assertEquals(sdp.render, text)
  }

  test("what is printed is read back the same") {
    val sdp = parsed(example)
    assertEquals(parsed(sdp.render), sdp)
  }

  test("lines may end in a bare LF, and blank lines are skipped") {
    val sdp = parsed(
      "v=0\no=- 1 1 IN IP4 192.0.2.1\ns=-\n\nc=IN IP4 192.0.2.1\nt=0 0\nm=audio 4000 RTP/AVP 0\n"
    )
    assertEquals(sdp.media.map(_.port.value), List(4000))
  }

  test("a declined stream has port 0") {
    val sdp = parsed(example.replace("m=video 51372", "m=video 0"))
    assertEquals(sdp.media.map(_.isDeclined), List(false, true))
  }

  test(
    "a missing version is named, a missing origin is not held against the peer"
  ) {
    assertEquals(
      SessionDescription.parse(
        crlf("o=- 1 1 IN IP4 192.0.2.1", "s=-", "t=0 0")
      ),
      Left(SdpError.Missing('v'))
    )
    val bare =
      parsed(crlf("v=0", "c=IN IP4 192.0.2.1", "m=audio 4000 RTP/AVP 0"))
    assertEquals(bare.origin, Origin.Unknown)
    assertEquals(bare.media.map(_.port.value), List(4000))
  }

  test("a line that is not type=value is malformed, and says which") {
    assertEquals(
      SessionDescription.parse(crlf("v=0", "nonsense")),
      Left(SdpError.Malformed(2, "not a <type>=<value> line"))
    )
  }

  test("a version other than 0 is refused") {
    assert(
      SessionDescription
        .parse(crlf("v=1", "o=- 1 1 IN IP4 192.0.2.1", "s=-", "t=0 0"))
        .isLeft
    )
  }

  test("a bad port is malformed") {
    val text = crlf(
      "v=0",
      "o=- 1 1 IN IP4 192.0.2.1",
      "s=-",
      "c=IN IP4 192.0.2.1",
      "t=0 0",
      "m=audio 99999 RTP/AVP 0"
    )
    assert(SessionDescription.parse(text).isLeft)
  }

  test("a connection address that is a name is its own error") {
    val text = crlf(
      "v=0",
      "o=- 1 1 IN IP4 192.0.2.1",
      "s=-",
      "c=IN IP4 media.example.com",
      "t=0 0"
    )
    assertEquals(
      SessionDescription.parse(text),
      Left(SdpError.NameAddress("media.example.com"))
    )
  }

  test("a name is fine as the origin's address") {
    val sdp = parsed(
      crlf(
        "v=0",
        "o=- 1 1 IN IP4 pbx.example.com",
        "s=-",
        "c=IN IP4 192.0.2.1",
        "t=0 0"
      )
    )
    assertEquals(sdp.origin.address, host"pbx.example.com")
  }

  test("an empty name prints as a space (RFC 4566 §5.3)") {
    val sdp = parsed(example).copy(name = "")
    assert(sdp.render.contains("\r\ns= \r\n"))
  }
}
