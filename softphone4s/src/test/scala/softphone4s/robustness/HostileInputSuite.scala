package softphone4s.robustness

import cats.Id
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import munit.FunSuite
import softphone4s.codec.{RtcpCodec, RtpCodec, SdpCodec, SipCodec}
import softphone4s.model.*
import softphone4s.dialog.{Coverage, DialogInfo}
import softphone4s.presence.Presence
import softphone4s.sdp.SessionDescription
import softphone4s.stun.Stun
import softphone4s.subscription.MessageSummary
import softphone4s.transport.StreamFraming

import java.nio.charset.StandardCharsets.ISO_8859_1

/** Inputs made to cost a parser more than they cost to send: a huge length, a
  * huge count, nesting, a flood of the same thing. Each must be answered, with
  * a value and not an exception, within a few seconds, however long it is.
  */
class HostileInputSuite extends FunSuite {

  /** Time allowed for one: they are megabytes long, and a run of the whole
    * suite shares a machine with others.
    */
  private val Seconds = 3000L

  /** What failed in the test that is running: every case is tried, and all that
    * fail are reported together, when the test ends.
    */
  private val failures = scala.collection.mutable.ListBuffer.empty[String]

  private def bounded(name: String)(decode: => Any): Unit = {
    val outcome = Fuzz.survives(name, name, Seconds)(decode)
    val result  = org.scalacheck.Test.check(outcome)(identity)
    if !result.passed then
      failures += s"$name: ${result.status.toString.replaceAll("\\s+", " ").take(220)}"
  }

  /** A test of cases that are all tried, with every failure reported at the
    * end.
    */
  private def hostile(name: String)(body: => Unit): Unit =
    test(name) {
      failures.clear()
      body
      val failed = failures.toList
      failures.clear()
      if failed.nonEmpty then fail(failed.mkString("\n", "\n", ""))
    }

  private def rep(piece: String, times: Int): String = {
    val b = new java.lang.StringBuilder(piece.length * times)
    var i = 0
    while i < times do { b.append(piece); i += 1 }
    b.toString
  }

  private val head =
    "INVITE sip:bob@example.com SIP/2.0\r\nVia: SIP/2.0/UDP 10.0.0.1:5060;branch=z9hG4bKabc\r\nFrom: <sip:alice@example.com>;tag=1\r\nTo: <sip:bob@example.com>\r\nCall-ID: a@b\r\nCSeq: 1 INVITE\r\n"

  private def sip(
      extra: String,
      tail: String = "Content-Length: 0\r\n\r\n"
  ): String =
    head + extra + tail

  private def decodeSip(text: String): Any =
    SipCodec.decode(Chunk.array(text.getBytes(ISO_8859_1)))

  hostile("SIP: a hundred thousand headers") {
    bounded("100k headers")(decodeSip(sip(rep("X-Header: value\r\n", 100000))))
  }
  hostile("SIP: a hundred thousand Via headers") {
    bounded("100k Via")(
      decodeSip(
        sip(rep("Via: SIP/2.0/UDP 1.2.3.4;branch=z9hG4bKx\r\n", 100000))
      )
    )
  }
  hostile("SIP: one header line of four megabytes") {
    bounded("4 MB header")(
      decodeSip(sip("X-Long: " + rep("a", 4 * 1024 * 1024) + "\r\n"))
    )
  }
  hostile("SIP: a request line of a megabyte") {
    bounded("1 MB request line")(
      decodeSip(
        "INVITE sip:" + rep("a", 1024 * 1024) + "@h SIP/2.0\r\n" + head.drop(
          head.indexOf("\r\n") + 2
        ) + "Content-Length: 0\r\n\r\n"
      )
    )
  }
  hostile(
    "SIP: lengths that overflow, go negative, or promise more than there is"
  ) {
    List(
      "99999999999999999999",
      "-1",
      "2147483648",
      "4294967296",
      "18446744073709551616",
      "1000000",
      "0x10",
      "+5",
      "1e9",
      ""
    ).foreach(n =>
      bounded(s"Content-Length: $n")(
        decodeSip(head + s"Content-Length: $n\r\n\r\nabc")
      )
    )
  }
  hostile("SIP: a hundred thousand comma-separated values in one header") {
    bounded("100k commas Contact")(
      decodeSip(sip("Contact: " + rep("<sip:a@b>,", 100000) + "<sip:z@y>\r\n"))
    )
    bounded("100k commas Via")(
      decodeSip(
        sip(
          "Via: " + rep(
            "SIP/2.0/UDP 1.2.3.4;branch=z9hG4bKx,",
            50000
          ) + "SIP/2.0/UDP 5.6.7.8\r\n"
        )
      )
    )
  }
  hostile("SIP: quotes that never close, angle brackets that never close") {
    bounded("quotes")(
      decodeSip(sip("From: \"" + rep("\\\"", 500000) + " <sip:a@b>\r\n"))
    )
    bounded("open quote")(decodeSip(sip("To: " + rep("\"", 500000) + "\r\n")))
    bounded("angles")(decodeSip(sip("Contact: " + rep("<", 500000) + "\r\n")))
    bounded("parentheses")(
      decodeSip(sip("Warning: " + rep("(", 500000) + "\r\n"))
    )
  }
  hostile("SIP: a million parameters on one header") {
    bounded("1M params")(
      decodeSip(sip("Contact: <sip:a@b>" + rep(";p=v", 1000000) + "\r\n"))
    )
    bounded("1M uri params")(
      decodeSip(sip("Contact: <sip:a@b" + rep(";p=v", 1000000) + ">\r\n"))
    )
  }
  hostile("SIP: folded lines without end, and blank-line floods") {
    bounded("folds")(decodeSip(sip("Subject: x\r\n" + rep(" y\r\n", 500000))))
    bounded("crlf flood")(decodeSip(rep("\r\n", 2000000) + head))
    bounded("bare cr and lf")(
      decodeSip(head + rep("\r", 1000000) + rep("\n", 1000000))
    )
  }
  hostile("SIP: a body of ten megabytes, and one that says it is longer") {
    bounded("10 MB body")(
      decodeSip(
        head + "Content-Length: 10485760\r\n\r\n" + rep("0123456789", 1048576)
      )
    )
    bounded("short body")(
      decodeSip(head + "Content-Length: 10485760\r\n\r\nabc")
    )
  }

  hostile(
    "a stream is framed in time linear in what it carries, however it arrives"
  ) {
    def frames(pieces: Iterator[Chunk[Byte]]) =
      Stream
        .emits(pieces.toList)
        .flatMap(Stream.chunk)
        .through(StreamFraming.frames[Id]())
        .compile
        .toList
    def chunked(text: String, size: Int): Iterator[Chunk[Byte]] =
      text.getBytes(ISO_8859_1).grouped(size).map(Chunk.array(_))
    // keep-alives as fast as they can be sent, in the reads a socket gives
    bounded("20 MB of keep-alives in 64 KB reads")(
      frames(chunked(rep("\r\n\r\n", 5000000), 65536))
    )
    bounded("20 MB of blank lines in 64 KB reads")(
      frames(chunked(rep("\r\n", 10000000), 65536))
    )
    // a body of the largest size, slowly
    val big =
      head + "Content-Length: 1048576\r\n\r\n" + rep("0123456789abcdef", 65536)
    bounded("1 MiB body in 1 KiB reads")(frames(chunked(big, 1024)))
    bounded("1 MiB body in 64 byte reads")(frames(chunked(big, 64)))
    bounded("1 MiB body in 8 byte reads")(frames(chunked(big, 8)))
    // many small messages in one read
    bounded("65 536 messages in 64 KB reads")(
      frames(chunked(rep(sip(""), 65536), 65536))
    )
  }

  hostile(
    "a stream: headers that never end, bodies that promise the earth, endless blank lines"
  ) {
    def frames(text: String) =
      Stream
        .chunk(Chunk.array(text.getBytes(ISO_8859_1)))
        .through(StreamFraming.frames[Id]())
        .compile
        .toList
    bounded("stream no end")(frames(head + rep("X: y\r\n", 2000000)))
    bounded("stream length")(
      frames(head + "Content-Length: 4000000000\r\n\r\nabc")
    )
    bounded("stream keep-alives")(frames(rep("\r\n", 5000000) + sip("")))
    bounded("stream many messages")(frames(rep(sip(""), 20000)))
  }

  private val sdpHead =
    "v=0\r\no=- 1 1 IN IP4 10.0.0.1\r\ns=-\r\nc=IN IP4 10.0.0.1\r\nt=0 0\r\n"

  hostile(
    "SDP: a hundred thousand attributes, ten thousand streams, a million-character line"
  ) {
    bounded("attrs")(
      SessionDescription.parse(
        sdpHead + "m=audio 8000 RTP/AVP 0\r\n" + rep(
          "a=rtpmap:0 PCMU/8000\r\n",
          100000
        )
      )
    )
    bounded("streams")(
      SessionDescription.parse(
        sdpHead + rep("m=audio 8000 RTP/AVP 0 8 9 101\r\na=sendrecv\r\n", 20000)
      )
    )
    bounded("formats")(
      SessionDescription.parse(
        sdpHead + "m=audio 8000 RTP/AVP " + rep("0 ", 500000) + "\r\n"
      )
    )
    bounded("offer attrs")(
      SdpCodec.parseOffer(
        sdpHead + "m=audio 8000 RTP/AVP 0\r\n" + rep(
          "a=fmtp:0 x=y\r\n",
          200000
        ),
        softphone4s.sdp.Capabilities.Default
      )
    )
    bounded("long line")(
      SessionDescription.parse(sdpHead + "a=" + rep("x", 2000000) + "\r\n")
    )
  }

  hostile(
    "RTP and RTCP: counts and lengths that point far outside the datagram"
  ) {
    val oversize = Array.fill[Byte](1400)(0)
    oversize(0) =
      (0x80 | 0x0f).toByte // version 2, fifteen contributing sources
    bounded("rtp csrc")(RtpCodec.decode(Chunk.array(oversize)))
    oversize(0) = (0x90).toByte // an extension header of unbounded length
    oversize(12) = 0xff.toByte; oversize(13) = 0xff.toByte;
    oversize(14) = 0xff.toByte; oversize(15) = 0xff.toByte
    bounded("rtp extension")(RtpCodec.decode(Chunk.array(oversize)))
    // a compound packet of 100 000 four-byte goodbyes, each with a count of 31 sources
    val goodbyes = Array.tabulate[Byte](400000)(i =>
      i % 4 match {
        case 0 => (0x80 | 31).toByte; case 1 => 203.toByte; case 2 => 0;
        case _ => 0
      }
    )
    bounded("rtcp goodbyes")(RtcpCodec.decode(Chunk.array(goodbyes)))
    val sdes = Array.tabulate[Byte](60000)(i =>
      i % 4 match {
        case 0 => (0x80 | 31).toByte; case 1 => 202.toByte;
        case 2 => 0xff.toByte; case _        => 0xff.toByte
      }
    )
    bounded("rtcp sdes")(RtcpCodec.decode(Chunk.array(sdes)))
    val reports = Array.tabulate[Byte](65000)(i =>
      i % 4 match {
        case 0 => (0x80 | 31).toByte; case 1 => 201.toByte;
        case 2 => 0xff.toByte; case _        => 0xff.toByte
      }
    )
    bounded("rtcp report blocks")(RtcpCodec.decode(Chunk.array(reports)))
  }

  hostile("STUN: attributes without end, lengths that do not add up") {
    val header = Array[Byte](
      0x01,
      0x01,
      0xff.toByte,
      0xfc.toByte,
      0x21,
      0x12,
      0xa4.toByte,
      0x42
    ) ++ Array.fill[Byte](12)(1)
    val zeros = header ++ Array.fill[Byte](65532)(0)
    bounded("stun zero attributes")(Stun.decode(Chunk.array(zeros)))
    val ones = header ++ Array.tabulate[Byte](65532)(i =>
      if i % 4 == 3 then 0 else 0xff.toByte
    )
    bounded("stun long attributes")(Stun.decode(Chunk.array(ones)))
    val tiny =
      header ++ Array.tabulate[Byte](65532)(i => if i % 4 == 3 then 1 else 0)
    bounded("stun one-byte attributes")(Stun.decode(Chunk.array(tiny)))
  }

  hostile(
    "dialog-info: nesting, floods, entity bombs, thousands of dialogs and keeping track of them"
  ) {
    val ns  = """xmlns="urn:ietf:params:xml:ns:dialog-info""""
    val top = s"""<dialog-info $ns version="1" state="full" entity="e">"""
    bounded("dialog-info depth")(
      DialogInfo.parse(
        top + rep("<x>", 40000) + rep("</x>", 40000) + "</dialog-info>"
      )
    )
    bounded("dialog-info unclosed")(DialogInfo.parse(top + rep("<x>", 40000)))
    bounded("dialog-info attributes")(
      DialogInfo.parse(
        s"""<dialog-info $ns version="1" state="full" entity="e" """ + (1 to 8000)
          .map(i => s"a$i=\"v\"")
          .mkString(" ") + "/>"
      )
    )
    bounded("dialog-info entity bomb")(
      DialogInfo.parse(
        """<!DOCTYPE p [<!ENTITY a "aaaaaaaaaa"><!ENTITY b "&a;&a;&a;&a;&a;&a;&a;&a;&a;&a;"><!ENTITY c "&b;&b;&b;&b;&b;&b;&b;&b;&b;&b;">]><dialog-info xmlns="urn:ietf:params:xml:ns:dialog-info" version="1" state="full" entity="&c;"/>"""
      )
    )
    // as many dialogs as a document at the limit can hold
    def many(from: Int, count: Int, state: String) =
      (from until from + count)
        .map(i => s"""<dialog id="d$i"><state>$state</state></dialog>""")
        .mkString
    val full =
      s"""<dialog-info $ns version="1" state="full" entity="e">${many(
          0,
          2500,
          "early"
        )}</dialog-info>"""
    bounded("dialog-info thousands of dialogs")(DialogInfo.parse(full))
    val held    = DialogInfo.parse(full).toOption
    val partial = DialogInfo
      .parse(
        s"""<dialog-info $ns version="2" state="partial" entity="e">${many(
            1250,
            2500,
            "confirmed"
          )}</dialog-info>"""
      )
      .toOption
    bounded("dialog-info track, thousands against thousands")(
      (held, partial).tupled.map((h, p) =>
        (1 to 20).foldLeft(Option(h))((known, _) =>
          known.map(k =>
            DialogInfo
              .track(Some(k), p.copy(version = k.version + 1))
              .getOrElse(k)
          )
        )
      )
    )
    bounded("dialog-info track, one dialog many times")(
      (1 to 20000).foldLeft(Option.empty[DialogInfo])((known, n) =>
        DialogInfo
          .track(
            known,
            DialogInfo(
              "e",
              n.toLong,
              if known.isEmpty then Coverage.Full else Coverage.Partial,
              Nil
            )
          )
          .toOption
      )
    )
  }

  hostile(
    "bodies: deep nesting, floods of attributes, entity bombs, a million lines"
  ) {
    val ns = """xmlns="urn:ietf:params:xml:ns:pidf""""
    bounded("xml depth")(
      Presence.parse(
        s"""<presence $ns entity="e">""" + rep("<x>", 100000) + rep(
          "</x>",
          100000
        ) + "</presence>"
      )
    )
    bounded("xml unclosed")(
      Presence.parse(s"""<presence $ns entity="e">""" + rep("<x>", 100000))
    )
    bounded("xml attributes")(
      Presence.parse(
        s"""<presence $ns entity="e" """ + (1 to 50000)
          .map(i => s"a$i=\"v\"")
          .mkString(" ") + "/>"
      )
    )
    bounded("xml namespaces")(
      Presence.parse(
        s"""<presence $ns entity="e">""" + (1 to 20000)
          .map(i => s"""<n$i:x xmlns:n$i="urn:$i"/>""")
          .mkString + "</presence>"
      )
    )
    bounded("xml text")(
      Presence.parse(
        s"""<presence $ns entity="e"><note>""" + rep(
          "&amp;&#65;",
          100000
        ) + "</note></presence>"
      )
    )
    bounded("xml entity bomb")(
      Presence.parse(
        """<!DOCTYPE p [<!ENTITY a "aaaaaaaaaa"><!ENTITY b "&a;&a;&a;&a;&a;&a;&a;&a;&a;&a;"><!ENTITY c "&b;&b;&b;&b;&b;&b;&b;&b;&b;&b;">]><presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"><note>&c;</note></presence>"""
      )
    )
    bounded("message-summary lines")(
      MessageSummary.parse(
        "Messages-Waiting: yes\r\n" + rep(
          "Voice-Message: 1/2 (0/1)\r\n",
          40000
        )
      )
    )
    bounded("message-summary one line")(
      MessageSummary.parse(
        "Messages-Waiting: yes\r\nVoice-Message: " + rep(
          "1",
          1000000
        ) + "/2\r\n"
      )
    )
  }

  hostile("addresses and header values: very long, very nested") {
    bounded("uri host")(SipUri.parse("sip:a@" + rep("a.", 500000) + "com"))
    bounded("uri ipv6")(SipUri.parse("sip:a@[" + rep(":", 500000) + "]"))
    bounded("uri escapes")(SipUri.parse("sip:" + rep("%41", 500000) + "@h"))
    bounded("uri headers")(SipUri.parse("sip:a@h?" + rep("a=b&", 500000)))
    bounded("via")(Via.parse("SIP/2.0/UDP host" + rep(";p=v", 500000)))
    bounded("name-addr")(NameAddr.parse(rep("\"a\" ", 200000) + "<sip:a@b>"))
    bounded("media type")(MediaType.parse("text/plain" + rep(";a=b", 500000)))
    bounded("digest challenge")(
      AuthChallenge.parse(
        "Digest " + rep("realm=\"r\", ", 200000) + "nonce=\"n\""
      )
    )
    bounded("digest quoted")(
      AuthChallenge.parse(
        "Digest realm=\"" + rep("\\\"", 500000) + "\", nonce=\"n\""
      )
    )
  }
}
