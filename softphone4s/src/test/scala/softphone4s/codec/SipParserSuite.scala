package softphone4s.codec

import fs2.Chunk
import munit.FunSuite
import softphone4s.model.*

import java.nio.charset.StandardCharsets.UTF_8

class SipParserSuite extends FunSuite {

  private def bytes(s: String): Chunk[Byte] = Chunk.array(s.getBytes(UTF_8))

  private def lines(ls: String*): String = ls.mkString("\r\n")

  private val invite = lines(
    "INVITE sip:bob@example.com SIP/2.0",
    "Via: SIP/2.0/UDP 10.0.0.1:5060;branch=z9hG4bK1",
    "From: <sip:alice@example.com>;tag=a1",
    "To: <sip:bob@example.com>",
    "Call-ID: call1@10.0.0.1",
    "CSeq: 1 INVITE"
  )

  private def withBody(head: String, body: String): String =
    head + "\r\n" + s"Content-Length: ${body.getBytes(UTF_8).length}\r\n\r\n" + body

  private def parsed(s: String): SipMessage =
    SipParser.parse(bytes(s)).fold(err => fail(err.message), identity)

  private def failure(s: String): ParseError =
    SipParser
      .parse(bytes(s))
      .fold(identity, m => fail(s"expected error, got $m"))

  // ---- Content-Length ----------------------------------------------------

  test("the body is sliced by Content-Length, not by scanning for CRLF") {
    val body = "v=0\r\n\r\no=- 1 1 IN IP4 10.0.0.1\r\n"
    assertEquals(parsed(withBody(invite, body)).body, body)
  }

  test("bytes beyond Content-Length are ignored") {
    val msg = withBody(invite, "abc") + "TRAILING GARBAGE"
    assertEquals(parsed(msg).body, "abc")
  }

  test("a body shorter than Content-Length is a typed error") {
    val msg = invite + "\r\nContent-Length: 10\r\n\r\nabc"
    assertEquals(failure(msg), ParseError.TruncatedBody(10, 3))
  }

  test("without Content-Length the body is the rest of the datagram") {
    assertEquals(parsed(invite + "\r\n\r\nhello").body, "hello")
  }

  test("Content-Length counts bytes, so multi-byte UTF-8 bodies round-trip") {
    val body = "héllo €"
    assertEquals(parsed(withBody(invite, body)).body, body)
  }

  test("an invalid Content-Length is a typed error") {
    List("-1", "abc", "1.5", "").foreach { v =>
      assertEquals(
        failure(invite + s"\r\nContent-Length: $v\r\n\r\n"),
        ParseError.InvalidContentLength(v)
      )
    }
  }

  test("the compact form l: sets Content-Length") {
    assertEquals(parsed(invite + "\r\nl: 2\r\n\r\nhiXX").body, "hi")
  }

  // ---- Tolerance ---------------------------------------------------------

  test("blank lines before the start line are ignored") {
    assertEquals(
      parsed("\r\n\r\n" + invite + "\r\n\r\n").headers.cseq.seq.value,
      1
    )
  }

  test("bare LF line endings are accepted") {
    val msg = invite.replace("\r\n", "\n") + "\nContent-Length: 2\n\nhi"
    assertEquals(parsed(msg).body, "hi")
  }

  test("a message with no terminating blank line has no body") {
    assertEquals(parsed(invite).body, "")
  }

  test("header names are case-insensitive and tabs may follow the colon") {
    val msg = invite.replace("Call-ID: ", "cAlL-iD:\t")
    assertEquals(parsed(msg).headers.callId.value, "call1@10.0.0.1")
  }

  test("several Vias in one line and across lines are all kept, in order") {
    val msg = invite.replace(
      "Via: SIP/2.0/UDP 10.0.0.1:5060;branch=z9hG4bK1",
      "Via: SIP/2.0/UDP 10.0.0.1;branch=z9hG4bK1,SIP/2.0/UDP 10.0.0.2;branch=z9hG4bK2\r\nv: SIP/2.0/UDP 10.0.0.3;branch=z9hG4bK3"
    )
    assertEquals(
      parsed(msg).headers.vias.toList.map(_.host.toString),
      List("10.0.0.1", "10.0.0.2", "10.0.0.3")
    )
  }

  test("a folded header joins its continuation lines") {
    val msg =
      invite + "\r\nSubject: I know you're there,\r\n\t pick up the phone\r\n\r\n"
    val other = parsed(msg).headers.others.find(_.name.matches("subject"))
    assertEquals(
      other.map(_.value),
      Some("I know you're there, pick up the phone")
    )
  }

  test("UTF-8 display names survive") {
    val msg = invite.replace("From: <", "From: \"Zoë Ünal\" <")
    assertEquals(parsed(msg).headers.from.displayName, Some("Zoë Ünal"))
  }

  // ---- Errors ------------------------------------------------------------

  test("empty and blank input is Empty") {
    assertEquals(SipParser.parse(Chunk.empty), Left(ParseError.Empty))
    assertEquals(SipParser.parse(bytes("\r\n\r\n")), Left(ParseError.Empty))
  }

  test("PUBLISH is a method") {
    assertEquals(
      parsed(
        invite
          .replace("INVITE sip", "PUBLISH sip")
          .replace("CSeq: 1 INVITE", "CSeq: 1 PUBLISH")
      ) match {
        case r: SipRequest => r.method
        case other         => fail(s"not a request: $other")
      },
      SipMethod.Publish
    )
  }

  test("an unknown method is a typed error") {
    assertEquals(
      failure(invite.replace("INVITE sip", "FROBNICATE sip")),
      ParseError.UnknownMethod("FROBNICATE")
    )
  }

  test("a request-URI that is not a SIP URI is a typed error") {
    failure(invite.replace("sip:bob@example.com SIP", "tel:+1555 SIP")) match {
      case ParseError.BadRequestUri(uri, _) => assertEquals(uri, "tel:+1555")
      case other                            => fail(s"unexpected: $other")
    }
  }

  test("a wrong SIP version or missing parts is a bad start line") {
    List(
      "INVITE sip:bob@example.com SIP/3.0",
      "INVITE sip:bob@example.com",
      "BOGUS"
    ).foreach { line =>
      val msg = invite.replace("INVITE sip:bob@example.com SIP/2.0", line)
      assertEquals(failure(msg), ParseError.BadStartLine(line))
    }
  }

  test("status codes outside 100-699 are rejected") {
    val response =
      invite.replace("INVITE sip:bob@example.com SIP/2.0", "SIP/2.0 99 Nope")
    assertEquals(failure(response), ParseError.BadStatusCode("99"))
    assertEquals(
      failure(response.replace("99", "700")),
      ParseError.BadStatusCode("700")
    )
  }

  test("a status line without a reason phrase is accepted") {
    val response =
      invite.replace("INVITE sip:bob@example.com SIP/2.0", "SIP/2.0 200")
    parsed(response) match {
      case res: SipResponse => assertEquals(res.reason, "")
      case other            => fail(s"expected a response, got $other")
    }
  }

  test("messages over the size limit are rejected before parsing") {
    assertEquals(
      SipParser.parse(bytes(invite + "\r\n\r\n"), maxSize = 10),
      Left(ParseError.MessageTooLarge(invite.length + 4, 10))
    )
  }

  test("random binary input never throws") {
    val random = new scala.util.Random(42)
    (1 to 500).foreach { _ =>
      val junk = new Array[Byte](random.nextInt(300))
      random.nextBytes(junk)
      assert(SipParser.parse(Chunk.array(junk)).isLeft)
    }
  }

  test("parsing a slice of a larger array respects its offset and length") {
    val msg   = bytes(withBody(invite, "hi")).toArray
    val whole = ("JUNKJUNK".getBytes(UTF_8) ++ msg ++ "JUNK".getBytes(UTF_8))
    val chunk = Chunk.array(whole, 8, msg.length)
    assertEquals(SipParser.parse(chunk).map(_.body), Right("hi"))
  }

  // ---- Framing -----------------------------------------------------------

  private val framed = withBody(invite, "hello")

  test("a complete message is one Frame of its full length") {
    assertEquals(
      SipParser.frame(bytes(framed)),
      Framing.Frame(framed.getBytes(UTF_8).length)
    )
  }

  test("bytes after a message are not part of its Frame") {
    val two = framed + framed
    val len = framed.getBytes(UTF_8).length
    assertEquals(SipParser.frame(bytes(two)), Framing.Frame(len))
    val rest = bytes(two).drop(len)
    assertEquals(SipParser.frame(rest), Framing.Frame(len))
  }

  test("partial headers or a partial body need more bytes") {
    val all = framed.getBytes(UTF_8)
    List(1, 20, all.length - 6, all.length - 1).foreach { n =>
      assertEquals(
        SipParser.frame(Chunk.array(all.take(n))),
        Framing.NeedMore,
        s"after $n bytes"
      )
    }
  }

  test("framing a message cut at every byte never throws and ends in a Frame") {
    val all     = framed.getBytes(UTF_8)
    val results =
      (1 to all.length).map(n => SipParser.frame(Chunk.array(all.take(n))))
    assertEquals(results.last, Framing.Frame(all.length))
    assert(results.init.forall(_ == Framing.NeedMore))
  }

  test("blank lines are keep-alives, consumed but not messages") {
    assertEquals(SipParser.frame(bytes("\r\n\r\n")), Framing.KeepAlive(4))
    assertEquals(SipParser.frame(bytes("\r\n" + framed)), Framing.KeepAlive(2))
  }

  test("a stream message without Content-Length is invalid") {
    assertEquals(
      SipParser.frame(bytes(invite + "\r\n\r\n")),
      Framing.Invalid(ParseError.MissingContentLength)
    )
  }

  test("a bad or oversized Content-Length is invalid") {
    assertEquals(
      SipParser.frame(bytes(invite + "\r\nContent-Length: -5\r\n\r\n")),
      Framing.Invalid(ParseError.InvalidContentLength("-5"))
    )
    SipParser.frame(
      bytes(invite + "\r\nContent-Length: 2000000000\r\n\r\n")
    ) match {
      case Framing.Invalid(ParseError.MessageTooLarge(_, _)) => ()
      case other => fail(s"unexpected: $other")
    }
  }

  test("endless headers are rejected once past the size limit") {
    val junk = Chunk.array(("X-A: " + "a" * 100).getBytes(UTF_8))
    assertEquals(SipParser.frame(junk), Framing.NeedMore)
    SipParser.frame(junk, maxSize = 50) match {
      case Framing.Invalid(ParseError.MessageTooLarge(_, 50)) => ()
      case other => fail(s"unexpected: $other")
    }
  }

  test("a Frame parses back to the original message") {
    SipParser.frame(bytes(framed + "NEXT")) match {
      case Framing.Frame(len) =>
        assertEquals(
          SipParser.parse(bytes(framed + "NEXT").take(len)).map(_.body),
          Right("hello")
        )
      case other => fail(s"expected a Frame, got $other")
    }
  }

  // ---- Typed scalar headers ---------------------------------------------

  test("Max-Forwards and Content-Type are parsed into their types") {
    val msg =
      invite + "\r\nMax-Forwards: 12\r\nContent-Type: application/sdp\r\n\r\n"
    val h = parsed(msg).headers
    assertEquals(h.maxForwards.value, 12)
    assertEquals(h.contentType, Some(MediaType.Sdp))
  }

  test("a missing Max-Forwards defaults to 70") {
    assertEquals(
      parsed(invite + "\r\n\r\n").headers.maxForwards,
      MaxForwards.Default
    )
  }

  test("invalid scalar headers are reported together as header errors") {
    val msg = invite + "\r\nMax-Forwards: 999\r\nContent-Type: nonsense\r\n\r\n"
    failure(msg) match {
      case ParseError.BadHeaders(errors) =>
        assertEquals(errors.size, 2)
      case other => fail(s"unexpected: $other")
    }
  }

  test("a CSeq beyond 2^31-1 is rejected") {
    val msg = invite.replace("CSeq: 1 INVITE", "CSeq: 4294967295 INVITE")
    assert(SipParser.parse(bytes(msg)).isLeft)
  }

  test("repeated challenge headers are all kept, unknown schemes included") {
    val msg = invite.replace(
      "INVITE sip:bob@example.com SIP/2.0",
      "SIP/2.0 401 Unauthorized"
    ) +
      "\r\nWWW-Authenticate: Digest realm=\"r\", nonce=\"n\"\r\nWWW-Authenticate: Negotiate x\r\n\r\n"
    assertEquals(parsed(msg).headers.wwwAuthenticate.size, 2)
  }

  test("Expires and Min-Expires are read into their type") {
    val h =
      parsed(invite + "\r\nExpires: 3600\r\nMin-Expires: 60\r\n\r\n").headers
    assertEquals(h.expires.map(_.value), Some(3600))
    assertEquals(h.minExpires.map(_.value), Some(60))
    assertEquals(parsed(invite + "\r\n\r\n").headers.expires, None)
  }

  test("a malformed Expires is a header error") {
    assert(SipParser.parse(bytes(invite + "\r\nExpires: soon\r\n\r\n")).isLeft)
  }
}
