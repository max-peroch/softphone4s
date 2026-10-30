package softphone4s.codec

import fs2.Chunk
import munit.FunSuite
import softphone4s.model.*

import java.nio.charset.StandardCharsets.UTF_8

/** Messages modelled on the SIP torture tests of RFC 4475 §3.1: unusual but
  * valid ones must parse, malformed ones must fail with a typed error. They
  * exercise the same cases but are written for this suite, not copied.
  *
  * Cases that need methods or transports beyond the ones we model (`intmeth`,
  * the `UNKNOWN` transport of `transports`) are asserted as rejected, and
  * become valid as those are added.
  */
class Rfc4475Suite extends FunSuite {

  private def msg(lines: String*): Chunk[Byte] =
    Chunk.array(lines.mkString("\r\n").getBytes(UTF_8))

  private def ok(c: Chunk[Byte]): SipMessage =
    SipParser.parse(c).fold(err => fail(err.message), identity)

  private def error(c: Chunk[Byte]): ParseError =
    SipParser.parse(c).fold(identity, m => fail(s"expected an error, got $m"))

  private def request(m: SipMessage): SipRequest = m match {
    case r: SipRequest => r
    case other         => fail(s"expected a request, got $other")
  }

  private def response(m: SipMessage): SipResponse = m match {
    case r: SipResponse => r
    case other          => fail(s"expected a response, got $other")
  }

  private val baseHeaders = List(
    "Via: SIP/2.0/UDP host.example.com;branch=z9hG4bK776",
    "From: <sip:alice@example.com>;tag=a1",
    "To: <sip:bob@example.com>",
    "Call-ID: torture@host",
    "CSeq: 1 INVITE"
  )

  private def invite(
      uri: String = "sip:bob@example.com",
      extra: List[String] = Nil,
      body: String = ""
  ): Chunk[Byte] =
    msg(
      (s"INVITE $uri SIP/2.0" :: baseHeaders) ++ extra ++
        List(s"Content-Length: ${body.getBytes(UTF_8).length}", "", body)*
    )

  // ---- 3.1.1 valid messages ------------------------------------------------

  test("wsinv: compact forms, odd spacing, folding and a long Via list") {
    val parsed = request(
      ok(
        msg(
          "INVITE sip:vivekg@chair.example.com;unknownparam SIP/2.0",
          "TO :",
          " <sip:vivekg@chair.example.com> ;   tag    = 1918181833n",
          "from   : \"J Rosenberg\"       <sip:jdrosen@example.com>",
          "  ;",
          "  tag = 98asjd8",
          "MaX-fOrWaRdS: 0068",
          "Call-ID: wsinv.ndaksdj@192.0.2.1",
          "cseq: 0009",
          "  INVITE",
          "Via  : SIP  /   2.0",
          " /UDP",
          "    192.0.2.2;branch=390skdjuw",
          "s :",
          "NewFangledHeader:   newfangled value",
          " more newfangled value",
          "v: SIP / 2.0  / TCP     spindle.example.com   ;",
          "  branch  =  z9hG4bK9ikj8  ,",
          " SIP  /    2.0   / UDP  192.168.255.111   ; branch=",
          " z9hG4bK30239",
          "m:\"Quoted string \\\"\\\"\" <sip:jdrosen@example.com> ; newparam =",
          "      newvalue ;",
          "  secondparam ; q = 0.33",
          "Content-Length   : 4",
          "",
          "v=0\n"
        )
      )
    )
    val h = parsed.headers
    assertEquals(h.vias.size, 3)
    assertEquals(
      h.vias.toList.map(_.transport),
      List(Transport.Udp, Transport.Tcp, Transport.Udp)
    )
    assertEquals(h.from.tag.map(_.value), Some("98asjd8"))
    assertEquals(h.to.tag.map(_.value), Some("1918181833n"))
    assertEquals(h.maxForwards.value, 68)
    assertEquals(h.cseq.seq.value, 9)
    assertEquals(h.contacts.head.displayName, Some("Quoted string \"\""))
    assertEquals(h.contacts.head.params.size, 3)
    assert(h.others.exists(_.name.matches("NewFangledHeader")))
    assertEquals(parsed.body, "v=0\n")
  }

  test("esc01: escaped characters in the request-URI user part are kept") {
    val r = request(
      ok(
        invite(uri =
          "sip:sip%3Auser%40example.com@company.com;other-param=summit"
        )
      )
    )
    assertEquals(
      r.requestUri.user.map(_.value),
      Some("sip%3Auser%40example.com")
    )
    assertEquals(r.requestUri.host.toString, "company.com")
    assertEquals(
      r.requestUri.render,
      "sip:sip%3Auser%40example.com@company.com;other-param=summit"
    )
  }

  test("escnull: an escaped NUL in the user part is accepted, not decoded") {
    val r = request(ok(invite(uri = "sip:null-%00-null@example.com")))
    assertEquals(r.requestUri.user.map(_.value), Some("null-%00-null"))
  }

  test("semiuri: a semicolon in the user part is part of the user") {
    val r =
      request(ok(invite(uri = "sip:user;par=u%40example.net@example.com")))
    assertEquals(
      r.requestUri.user.map(_.value),
      Some("user;par=u%40example.net")
    )
    assertEquals(r.requestUri.params.size, 0)
  }

  test("longreq: very long header values and many extension headers") {
    val longValue = "x" * 5000
    val extras    = (1 to 60).map(i => s"X-Header-$i: value $i").toList
    val parsed    = request(ok(invite(extra = s"X-Long: $longValue" :: extras)))
    assertEquals(parsed.headers.others.size, 61)
    assertEquals(parsed.headers.others.head.value.length, 5000)
  }

  test(
    "dblreq: a second request after the body is ignored by parse, bounded by frame"
  ) {
    val first  = invite(body = "abc")
    val second = invite(body = "def")
    val both   = first ++ second
    assertEquals(request(ok(both)).body, "abc")
    assertEquals(SipParser.frame(both), Framing.Frame(first.size))
  }

  test("transports: every transport we model is accepted in Via") {
    List("UDP", "TCP", "TLS", "SCTP", "WS", "WSS").foreach { t =>
      val m = ok(
        invite(extra =
          List(s"Via: SIP/2.0/$t other.example.com;branch=z9hG4bKx")
        )
      )
      assertEquals(m.headers.vias.size, 2, t)
    }
  }

  test("transports: an unknown Via transport is rejected with a header error") {
    error(
      invite(extra =
        List("Via: SIP/2.0/UNKNOWN other.example.com;branch=z9hG4bKx")
      )
    ) match {
      case ParseError.BadHeaders(_) => ()
      case other                    => fail(s"unexpected: $other")
    }
  }

  test("intmeth: a method outside the modelled set is a typed error") {
    assertEquals(
      error(
        msg("!interesting-Method0123456789_*+`.%indeed'~ sip:x@y SIP/2.0", "")
      ),
      ParseError.UnknownMethod("!INTERESTING-METHOD0123456789_*+`.%INDEED'~")
    )
  }

  test("noreason: a status line with no reason phrase") {
    val r = response(
      ok(
        msg(
          "SIP/2.0 100 ",
          baseHeaders.mkString("\r\n"),
          "Content-Length: 0",
          "",
          ""
        )
      )
    )
    assertEquals(r.statusCode, 100)
    assertEquals(r.reason, "")
  }

  test("unreason: a reason phrase in UTF-8") {
    val r = response(
      ok(
        msg(
          "SIP/2.0 200 = 2**3 * 5**2 но сто девяносто девять - простое",
          baseHeaders.mkString("\r\n"),
          "Content-Length: 0",
          "",
          ""
        )
      )
    )
    assertEquals(r.reason, "= 2**3 * 5**2 но сто девяносто девять - простое")
  }

  test("mpart01: a multipart body is passed through untouched") {
    val body =
      "--unique-boundary-1\r\nContent-Type: application/sdp\r\n\r\nv=0\r\n\r\n--unique-boundary-1--\r\n"
    val r = request(
      ok(
        invite(
          extra =
            List("Content-Type: multipart/mixed;boundary=unique-boundary-1"),
          body = body
        )
      )
    )
    assertEquals(r.body, body)
    assertEquals(r.headers.contentType.map(_.mainType), Some("multipart"))
    assertEquals(r.headers.contentType.map(_.params.size), Some(1))
  }

  test("lwsdisp: whitespace between a header name and its colon") {
    val r = request(
      ok(invite(extra = List("Subject : lunch", "X-Spaced   :   value  ")))
    )
    assertEquals(
      r.headers.others.map(h => h.name.value -> h.value),
      List("Subject" -> "lunch", "X-Spaced" -> "value")
    )
  }

  test("cparam: Contact parameters with quoted and bare values") {
    val r = request(
      ok(
        invite(extra =
          List(
            "Contact: <sip:a@10.0.0.1>;q=0.7;expires=3600;+sip.instance=\"<urn:uuid:00000000-0000-1000-8000-AABBCCDDEEFF>\""
          )
        )
      )
    )
    assertEquals(r.headers.contacts.head.params.size, 3)
  }

  // ---- 3.1.2 invalid messages ----------------------------------------------

  test("ncl: a negative Content-Length") {
    assertEquals(
      error(
        msg(
          (("INVITE sip:bob@example.com SIP/2.0" :: baseHeaders) :+ "l: -999" :+ "" :+ "")*
        )
      ),
      ParseError.InvalidContentLength("-999")
    )
  }

  test("clerr: Content-Length longer than the body") {
    assertEquals(
      error(
        msg(
          (("INVITE sip:bob@example.com SIP/2.0" :: baseHeaders) :+ "Content-Length: 9999" :+ "" :+ "short")*
        )
      ),
      ParseError.TruncatedBody(9999, 5)
    )
  }

  test("scalar02: out-of-range numbers in CSeq and Max-Forwards") {
    val headers = List(
      "Via: SIP/2.0/UDP host.example.com;branch=z9hG4bK776",
      "From: <sip:alice@example.com>;tag=a1",
      "To: <sip:bob@example.com>",
      "Call-ID: scalar02@host",
      "CSeq: 36893488147419103232 INVITE",
      "Max-Forwards: 300"
    )
    error(
      msg(
        ("INVITE sip:bob@example.com SIP/2.0" :: headers) ++ List(
          "Content-Length: 0",
          "",
          ""
        )*
      )
    ) match {
      case ParseError.BadHeaders(errors) => assertEquals(errors.size, 2)
      case other                         => fail(s"unexpected: $other")
    }
  }

  test("insuf: mandatory headers missing") {
    error(
      msg(
        "INVITE sip:bob@example.com SIP/2.0",
        "Via: SIP/2.0/UDP host;branch=z9hG4bK1",
        "Content-Length: 0",
        "",
        ""
      )
    ) match {
      case ParseError.BadHeaders(errors) =>
        assertEquals(errors.size, 4) // From, To, Call-ID, CSeq
      case other => fail(s"unexpected: $other")
    }
  }

  test("badaspec: a request-URI that is not a SIP URI") {
    List(
      "sip:user@example.com:abcd",
      "sip:",
      "http://example.com/",
      "sip:user@exa mple.com"
    ).foreach { uri =>
      error(
        msg(
          (s"INVITE $uri SIP/2.0" :: baseHeaders) ++ List(
            "Content-Length: 0",
            "",
            ""
          )*
        )
      ) match {
        case ParseError.BadRequestUri(_, _) | ParseError.BadStartLine(_) => ()
        case other => fail(s"unexpected for '$uri': $other")
      }
    }
  }

  test("badvers: an unsupported SIP version") {
    assertEquals(
      error(
        msg("INVITE sip:bob@example.com SIP/7.0", "Content-Length: 0", "", "")
      ),
      ParseError.BadStartLine("INVITE sip:bob@example.com SIP/7.0")
    )
  }

  test("badbranch: an empty branch parameter") {
    error(
      invite(extra = List("Via: SIP/2.0/UDP other.example.com;branch="))
    ) match {
      case ParseError.BadHeaders(_) => ()
      case other                    => fail(s"unexpected: $other")
    }
  }

  test("bigcode: a status code that is not three digits in range") {
    List("4294967301", "99", "1000", "abc").foreach { code =>
      assertEquals(
        error(
          msg(
            s"SIP/2.0 $code Weird",
            baseHeaders.mkString("\r\n"),
            "Content-Length: 0",
            "",
            ""
          )
        ),
        ParseError.BadStatusCode(code)
      )
    }
  }

  test("mismatch01: the request line and CSeq name different methods") {
    val parsed = error(
      msg(
        "OPTIONS sip:bob@example.com SIP/2.0",
        baseHeaders.mkString("\r\n"),
        "Content-Length: 0",
        "",
        ""
      )
    )
    assertEquals(
      parsed,
      ParseError.MethodMismatch(SipMethod.Options, SipMethod.Invite)
    )
  }

  test("a message that is only a start line and nothing else") {
    error(msg("INVITE sip:bob@example.com SIP/2.0")) match {
      case ParseError.BadHeaders(_) => ()
      case other                    => fail(s"unexpected: $other")
    }
  }
}
