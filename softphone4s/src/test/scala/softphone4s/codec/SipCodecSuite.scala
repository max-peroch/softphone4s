package softphone4s.codec

import munit.FunSuite
import softphone4s.model.*

class SipCodecSuite extends FunSuite {

  private val CR = "\r\n"

  private def msg(lines: String*): String =
    lines.mkString(CR) + CR + CR

  test("decode INVITE request") {
    val raw = msg(
      "INVITE sip:+1234@example.com SIP/2.0",
      "Via: SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bK1",
      "From: <sip:user@realm>;tag=abc",
      "To: <sip:+1234@example.com>",
      "Call-ID: abc@192.168.1.1",
      "CSeq: 1 INVITE",
      "Contact: <sip:user@192.168.1.1:5060>",
      "Content-Length: 0"
    )
    SipCodec.decode(raw) match {
      case Right(req: SipRequest) =>
        assertEquals(req.method, SipMethod.Invite)
        assertEquals(req.requestUri, "sip:+1234@example.com")
        assertEquals(req.headers.callId, "abc@192.168.1.1")
        assertEquals(req.headers.cseq, CSeq(1, SipMethod.Invite))
        assertEquals(req.headers.from, "<sip:user@realm>;tag=abc")
      case other => fail(s"unexpected: $other")
    }
  }

  test("decode 200 OK response") {
    val raw = msg(
      "SIP/2.0 200 OK",
      "Via: SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bK1",
      "From: <sip:user@realm>;tag=abc",
      "To: <sip:callee@host>;tag=xyz",
      "Call-ID: abc@192.168.1.1",
      "CSeq: 1 INVITE",
      "Contact: <sip:callee@192.168.1.2:5060>",
      "Content-Length: 0"
    )
    SipCodec.decode(raw) match {
      case Right(res: SipResponse) =>
        assertEquals(res.statusCode, 200)
        assertEquals(res.reason, "OK")
        assertEquals(res.headers.callId, "abc@192.168.1.1")
        assertEquals(res.headers.to, "<sip:callee@host>;tag=xyz")
      case other => fail(s"unexpected: $other")
    }
  }

  test("decode 407 and extract Proxy-Authenticate header") {
    val raw = msg(
      "SIP/2.0 407 Proxy Authentication Required",
      "Via: SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bK1",
      "From: <sip:user@realm>;tag=abc",
      "To: <sip:callee@host>",
      "Call-ID: abc@192.168.1.1",
      "CSeq: 1 INVITE",
      "Contact: <sip:proxy@host>",
      """Proxy-Authenticate: Digest realm="sip.example.com", nonce="testnonce", algorithm=MD5""",
      "Content-Length: 0"
    )
    SipCodec.decode(raw) match {
      case Right(res: SipResponse) =>
        assertEquals(res.statusCode, 407)
        assert(res.headers.proxyAuthenticate.exists(_.contains("testnonce")))
      case other => fail(s"unexpected: $other")
    }
  }

  test("decode compact header forms v, f, t, m") {
    val raw = msg(
      "INVITE sip:callee@host SIP/2.0",
      "v: SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bK1",
      "f: <sip:user@realm>;tag=abc",
      "t: <sip:callee@host>",
      "Call-ID: compact@host",
      "CSeq: 1 INVITE",
      "m: <sip:user@192.168.1.1:5060>",
      "Content-Length: 0"
    )
    SipCodec.decode(raw) match {
      case Right(req: SipRequest) =>
        assert(req.headers.via.contains("192.168.1.1"))
        assert(req.headers.from.contains("user@realm"))
        assert(req.headers.to.contains("callee@host"))
        assert(req.headers.contact.contains("192.168.1.1"))
      case other => fail(s"unexpected: $other")
    }
  }

  test("unfold multi-line header continuation") {
    val raw = msg(
      "SIP/2.0 200 OK",
      "Via: SIP/2.0/UDP 192.168.1.1:5060",
      "From: <sip:user@realm>;tag=abc",
      "To: <sip:callee@host>",
      "Call-ID: abc@host",
      "CSeq: 1 INVITE",
      "Contact: <sip:callee@host:5060>",
      "Subject: Meeting",
      "  at noon",
      "Content-Length: 0"
    )
    SipCodec.decode(raw) match {
      case Right(res: SipResponse) =>
        val subject =
          res.headers.extraHeaders.get("subject").flatMap(_.headOption)
        assert(subject.exists(_.contains("at noon")), s"got: $subject")
      case other => fail(s"unexpected: $other")
    }
  }

  test("missing Call-ID header yields empty string") {
    val raw = msg(
      "SIP/2.0 200 OK",
      "Via: SIP/2.0/UDP 192.168.1.1:5060",
      "From: <sip:user@realm>;tag=abc",
      "To: <sip:callee@host>",
      "CSeq: 1 INVITE",
      "Contact: <sip:callee@host:5060>",
      "Content-Length: 0"
    )
    SipCodec.decode(raw) match {
      case Right(res) => assertEquals(res.headers.callId, "")
      case Left(err)  => fail(s"unexpected error: $err")
    }
  }

  test("decode fails on empty input") {
    assert(SipCodec.decode("").isLeft)
  }

  test("decode fails on bad request line") {
    assert(SipCodec.decode(msg("BOGUS")).isLeft)
  }

  test("encode/decode round-trip preserves key fields") {
    val original = SipRequest(
      method = SipMethod.Bye,
      requestUri = "sip:callee@host",
      headers = SipHeaders(
        callId = "roundtrip@host",
        from = "<sip:user@realm>;tag=local",
        to = "<sip:callee@host>;tag=remote",
        via = "SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bKtest",
        cseq = CSeq(2, SipMethod.Bye),
        contact = "<sip:user@192.168.1.1:5060>"
      )
    )
    SipCodec.decode(SipCodec.encode(original)) match {
      case Right(decoded: SipRequest) =>
        assertEquals(decoded.method, SipMethod.Bye)
        assertEquals(decoded.headers.callId, "roundtrip@host")
        assertEquals(decoded.headers.cseq, CSeq(2, SipMethod.Bye))
        assertEquals(decoded.requestUri, "sip:callee@host")
      case other => fail(s"unexpected: $other")
    }
  }
}
