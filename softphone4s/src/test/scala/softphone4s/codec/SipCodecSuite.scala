package softphone4s.codec

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
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
        assertEquals(req.requestUri.render, "sip:+1234@example.com")
        assertEquals(req.headers.callId.value, "abc@192.168.1.1")
        assertEquals(req.headers.cseq, CSeq(1, SipMethod.Invite))
        assertEquals(req.headers.from.render, "<sip:user@realm>;tag=abc")
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
        assertEquals(res.headers.callId.value, "abc@192.168.1.1")
        assertEquals(res.headers.to.render, "<sip:callee@host>;tag=xyz")
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
        assert(req.headers.topVia.render.contains("192.168.1.1"))
        assert(req.headers.from.render.contains("user@realm"))
        assert(req.headers.to.render.contains("callee@host"))
        assert(req.headers.contacts.exists(_.render.contains("192.168.1.1")))
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
          res.headers.others.find(_.name.matches("subject")).map(_.value)
        assert(subject.exists(_.contains("at noon")), s"got: $subject")
      case other => fail(s"unexpected: $other")
    }
  }

  test("missing Call-ID header is a decode error") {
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
      case Left(err) => assert(err.contains("Call-ID"), err)
      case Right(m)  => fail(s"expected a decode error, got: $m")
    }
  }

  test("decode fails on empty input") {
    assert(SipCodec.decode("").isLeft)
  }

  test("decode fails when the request-URI is not a SIP URI") {
    assert(SipCodec.decode(msg("INVITE tel:+15551234567 SIP/2.0")).isLeft)
  }

  test("decode fails on bad request line") {
    assert(SipCodec.decode(msg("BOGUS")).isLeft)
  }

  test("encode/decode round-trip preserves key fields") {
    val original = SipRequest(
      method = SipMethod.Bye,
      requestUri = uri("sip:callee@host"),
      headers = SipHeaders(
        callId = callId("roundtrip@host"),
        from = nameAddr("<sip:user@realm>;tag=local"),
        to = nameAddr("<sip:callee@host>;tag=remote"),
        vias = NonEmptyList.one(
          via("SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bKtest")
        ),
        cseq = CSeq(2, SipMethod.Bye),
        contacts = List(nameAddr("<sip:user@192.168.1.1:5060>"))
      )
    )
    SipCodec.decode(SipCodec.encode(original)) match {
      case Right(decoded: SipRequest) =>
        assertEquals(decoded.method, SipMethod.Bye)
        assertEquals(decoded.headers.callId, callId("roundtrip@host"))
        assertEquals(decoded.headers.cseq, CSeq(2, SipMethod.Bye))
        assertEquals(decoded.requestUri.render, "sip:callee@host")
      case other => fail(s"unexpected: $other")
    }
  }

  test("decode keeps several Vias and comma-separated Contacts, in order") {
    val raw = msg(
      "SIP/2.0 200 OK",
      "Via: SIP/2.0/UDP 10.0.0.1:5060;branch=z9hG4bK1, SIP/2.0/UDP 10.0.0.2;branch=z9hG4bK2",
      "Via: SIP/2.0/UDP 10.0.0.3;branch=z9hG4bK3",
      "From: <sip:user@realm>;tag=abc",
      "To: <sip:callee@host>;tag=xyz",
      "Call-ID: multi@host",
      "CSeq: 1 INVITE",
      "Contact: <sip:a@10.0.0.9>, \"B\" <sip:b@10.0.0.9;transport=tcp>",
      "Record-Route: <sip:proxy1.example.com;lr>",
      "Record-Route: <sip:proxy2.example.com;lr>",
      "Content-Length: 0"
    )
    SipCodec.decode(raw) match {
      case Right(res: SipResponse) =>
        assertEquals(
          res.headers.vias.toList.map(_.host.toString),
          List("10.0.0.1", "10.0.0.2", "10.0.0.3")
        )
        assertEquals(res.headers.contacts.size, 2)
        assertEquals(res.headers.contacts(1).displayName, Some("B"))
        assertEquals(
          res.headers.recordRoutes.map(_.uri.render),
          List("sip:proxy1.example.com;lr", "sip:proxy2.example.com;lr")
        )
      case other => fail(s"unexpected: $other")
    }
  }

  test("decode accumulates every header error in one message") {
    val raw = msg(
      "SIP/2.0 200 OK",
      "Via: not a via",
      "From: <tel:+1555>",
      "To: <sip:callee@host>",
      "CSeq: 1 INVITE"
    )
    SipCodec.decode(raw) match {
      case Left(err) =>
        assert(err.contains("Call-ID"), err)
        assert(err.contains("Via"), err)
        assert(err.contains("tel:"), err)
      case Right(m) => fail(s"expected a decode error, got: $m")
    }
  }

  test("decode fails when To is not a SIP URI") {
    val raw = msg(
      "SIP/2.0 200 OK",
      "Via: SIP/2.0/UDP 10.0.0.1;branch=z9hG4bK1",
      "From: <sip:user@realm>;tag=abc",
      "To: <tel:+15551234567>;tag=xyz",
      "Call-ID: x@host",
      "CSeq: 1 INVITE"
    )
    assert(SipCodec.decode(raw).isLeft)
  }

  test("encode writes every Via and Contact, and extension headers last") {
    val req = SipRequest(
      method = SipMethod.Options,
      requestUri = uri("sip:callee@host"),
      headers = SipHeaders(
        callId = callId("enc@host"),
        from = nameAddr("<sip:user@realm>;tag=a"),
        to = nameAddr("<sip:callee@host>"),
        vias = NonEmptyList.of(
          via("SIP/2.0/UDP 10.0.0.1;branch=z9hG4bK1"),
          via("SIP/2.0/UDP 10.0.0.2;branch=z9hG4bK2")
        ),
        cseq = CSeq(1, SipMethod.Options),
        contacts =
          List(nameAddr("<sip:a@10.0.0.9>"), nameAddr("<sip:b@10.0.0.9>")),
        others = List(
          ExtensionHeader(FieldName("X-Test").toOption.get, "yes")
        )
      )
    )
    val lines = SipCodec.encode(req).split("\r\n").toList
    assertEquals(lines.count(_.startsWith("Via: ")), 2)
    assertEquals(lines.count(_.startsWith("Contact: ")), 2)
    assertEquals(
      lines.dropWhile(!_.startsWith("Content-Length")).drop(1).head,
      "X-Test: yes"
    )
  }
}
