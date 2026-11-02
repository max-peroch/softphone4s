package softphone4s.robustness

import cats.Id
import cats.data.NonEmptyList
import com.comcast.ip4s.*
import fs2.{Chunk, Stream}
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Test}
import softphone4s.codec.{RtcpCodec, RtpCodec, SdpCodec, SipCodec, SipGen}
import softphone4s.media.{LossFraction, RtcpPacket}
import softphone4s.model.*
import softphone4s.presence.Presence
import softphone4s.publication.EntityTag
import softphone4s.robustness.Fuzz.*
import softphone4s.sdp.{CryptoAttribute, RtpMap, SessionDescription}
import softphone4s.srtp.{
  MasterKey,
  SrtcpInbound,
  SrtcpOutbound,
  SrtpInbound,
  SrtpOutbound,
  SrtpSuite
}
import softphone4s.stun.Stun
import softphone4s.subscription.{
  EventHeader,
  MessageSummary,
  NotifyInfo,
  NotifyState
}
import softphone4s.transport.StreamFraming

import java.nio.charset.StandardCharsets.ISO_8859_1

/** Everything that decodes what comes off the network must answer any input
  * with a value and not with an exception, and soon. Each decoder is given
  * bytes at random, and real messages damaged in the ways that break parsers.
  */
class DecoderFuzzSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(4000)

  private def seeded[A](gen: Gen[A], count: Int): List[A] =
    Gen
      .listOfN(count, gen)
      .pureApply(Gen.Parameters.default.withSize(30), Seed(20261102L))

  /** A decoder of bytes: fed random bytes and damaged seeds. */
  private def bytesDecoder(name: String, seeds: List[Bytes])(
      decode: Bytes => Any
  ): Unit = {
    property(s"$name survives random bytes") {
      forAll(randomBytes)(b => survives(name, hex(b))(decode(b)))
    }
    property(s"$name survives real input, damaged") {
      forAll(mutated(seeds))(b => survives(name, hex(b))(decode(b)))
    }
  }

  /** A decoder of text: fed any text, protocol-looking text and damaged seeds.
    */
  private def textDecoder(name: String, seeds: List[String])(
      decode: String => Any
  ): Unit = {
    property(s"$name survives any text") {
      forAll(Gen.oneOf(anyText, protocolText))(s =>
        survives(name, s"'${s.take(200)}'")(decode(s))
      )
    }
    property(s"$name survives real input, damaged") {
      forAll(mutatedText(seeds))(s =>
        survives(name, s"'${s.take(200)}'")(decode(s))
      )
    }
  }

  private def chunk(b: Bytes): Chunk[Byte] = Chunk.array(b)

  // ---- SIP ------------------------------------------------------------------

  private val sipSeeds: List[Bytes] =
    seeded(SipGen.messageGen, 40).map(m => SipCodec.encode(m).toArray) ++ List(
      "INVITE sip:bob@example.com SIP/2.0\r\nVia: SIP/2.0/UDP 10.0.0.1:5060;branch=z9hG4bKabc;rport\r\nMax-Forwards: 70\r\nFrom: \"Alice\" <sip:alice@example.com>;tag=1\r\nTo: <sip:bob@example.com>\r\nCall-ID: a@b\r\nCSeq: 1 INVITE\r\nContact: <sip:alice@10.0.0.1:5060;transport=udp>\r\nContent-Type: application/sdp\r\nContent-Length: 4\r\n\r\nv=0\r\n",
      "SIP/2.0 401 Unauthorized\r\nVia: SIP/2.0/UDP 10.0.0.1:5060;branch=z9hG4bKabc;received=1.2.3.4;rport=5060\r\nFrom: <sip:a@b>;tag=1\r\nTo: <sip:a@b>;tag=2\r\nCall-ID: x\r\nCSeq: 2 REGISTER\r\nWWW-Authenticate: Digest realm=\"r\", nonce=\"n\", algorithm=SHA-256, qop=\"auth,auth-int\", opaque=\"o\"\r\nContent-Length: 0\r\n\r\n",
      "NOTIFY sip:a@10.0.0.1 SIP/2.0\r\nVia: SIP/2.0/UDP 1.1.1.1;branch=z9hG4bKn\r\nFrom: <sip:b@c>;tag=s\r\nTo: <sip:a@b>;tag=t\r\nCall-ID: c\r\nCSeq: 7 NOTIFY\r\nEvent: presence\r\nSubscription-State: active;expires=300\r\nContent-Type: application/pidf+xml\r\nContent-Length: 0\r\n\r\n",
      "v: SIP/2.0/UDP a;branch=z9hG4bK1\r\nf: <sip:a@b>;tag=1\r\nt: <sip:c@d>\r\ni: id\r\nCSeq: 1 BYE\r\nl: 0\r\n\r\n"
    ).map(_.getBytes(ISO_8859_1))

  bytesDecoder("SipCodec.decode (bytes)", sipSeeds)(b =>
    SipCodec.decode(chunk(b))
  )
  bytesDecoder("SipCodec.decode (text)", sipSeeds)(b =>
    SipCodec.decode(new String(b, ISO_8859_1))
  )
  bytesDecoder("a NOTIFY read as a notification", sipSeeds)(b =>
    SipCodec.decode(chunk(b)).foreach {
      case r: SipRequest => NotifyInfo.from(r)
      case _             => ()
    }
  )

  property("StreamFraming survives random and damaged streams, in any pieces") {
    val gen = for {
      stream <- Gen.oneOf(
        randomBytes,
        mutated(sipSeeds ++ sipSeeds.take(8).map(s => s ++ s))
      )
      cuts <- Gen.listOf(Gen.choose(1, 90))
    } yield (stream, cuts)
    forAll(gen) { (stream, cuts) =>
      survives("StreamFraming", hex(stream)) {
        val (pieces, _) =
          cuts.foldLeft((List.empty[Chunk[Byte]], chunk(stream))) {
            case ((done, rest), n) => (done :+ rest.take(n), rest.drop(n))
          }
        Stream
          .emits(pieces)
          .flatMap(Stream.chunk)
          .through(StreamFraming.frames[Id]())
          .compile
          .toList
      }
    }
  }

  // ---- Header values and addresses ----------------------------------------------

  private val uriSeeds = List(
    "sip:alice@example.com",
    "sips:bob@[2001:db8::1]:5061;transport=tls;lr",
    "sip:+15551234567@gw.example.com;user=phone?subject=hi&x=y",
    "sip:host:5060",
    "sip:a%40b@h;p=%2F",
    "sip:alice:secret@example.com",
    "sip:10.0.0.1;maddr=224.0.0.1;ttl=3"
  )
  textDecoder("SipUri.parse", uriSeeds)(SipUri.parse(_))
  textDecoder(
    "Via.parse",
    List(
      "SIP/2.0/UDP 10.0.0.1:5060;branch=z9hG4bK1;rport;received=1.2.3.4",
      "SIP/2.0/TLS [::1]:5061;branch=z9hG4bK2;alias",
      "SIP / 2.0 / TCP host;maddr=x;ttl=2"
    )
  )(Via.parse(_))
  textDecoder(
    "NameAddr.parse",
    List(
      "\"Alice A\" <sip:alice@example.com>;tag=abc",
      "<sip:b@c>;q=0.5;expires=60",
      "sip:d@e;tag=1",
      "Bob <sip:f@g>"
    )
  )(NameAddr.parse(_))
  textDecoder(
    "MediaType.parse",
    List(
      "application/sdp",
      "text/plain;charset=utf-8",
      "application/pidf+xml;x=\"a b\"",
      "multipart/mixed;boundary=\"b\""
    )
  )(MediaType.parse(_))
  textDecoder("Expires.parse", List("3600", "0", "99999999999", "-1", " 12 "))(
    Expires.parse(_)
  )
  textDecoder(
    "AuthChallenge.parse",
    List(
      "Digest realm=\"r\", nonce=\"n\", algorithm=MD5, qop=\"auth\"",
      "Basic realm=\"x\"",
      "Digest realm=\"r\", nonce=\"n\", stale=true, opaque=\"o\", algorithm=SHA-256-sess"
    )
  )(AuthChallenge.parse(_))
  textDecoder(
    "EventHeader.parse",
    List("presence", "message-summary;id=7", "dialog;call-id=x;from-tag=y")
  )(EventHeader.parse(_))
  textDecoder(
    "NotifyState.parse",
    List(
      "active;expires=300",
      "pending",
      "terminated;reason=timeout;retry-after=30",
      "terminated;reason=probation"
    )
  )(NotifyState.parse(_))
  textDecoder("EntityTag.parse", List("dx200xyz", "a.b-c", "x y"))(
    EntityTag.parse(_)
  )
  textDecoder(
    "Callee",
    List(
      "alice",
      "bob@example.com",
      "sip:carol@h",
      "*67#",
      "+15551234567",
      "a b"
    )
  )(Callee(_).foreach(_.toSipUri(host"example.com")))
  textDecoder(
    "User/Tag/Branch/CallId",
    List("alice", "a%20b", "z9hG4bK1", "id@host")
  )(s => (User(s), Tag(s), Branch(s), CallId(s)))

  // ---- SDP and bodies ------------------------------------------------------------

  private val sdpSeeds = List(
    "v=0\r\no=- 1 1 IN IP4 10.0.0.1\r\ns=-\r\nc=IN IP4 10.0.0.1\r\nt=0 0\r\nm=audio 8000 RTP/AVP 0 8 101\r\na=rtpmap:0 PCMU/8000\r\na=rtpmap:101 telephone-event/8000\r\na=fmtp:101 0-16\r\na=sendrecv\r\n",
    "v=0\r\no=- 2 2 IN IP6 2001:db8::1\r\ns=x\r\nc=IN IP6 2001:db8::1\r\nt=0 0\r\nm=audio 4000 RTP/SAVP 9\r\na=rtpmap:9 G722/8000\r\na=crypto:1 AES_CM_128_HMAC_SHA1_80 inline:WVNfX19zZW1jdGwgKCkgewkyMjA7fQp9CnVubGVzcyAoZ2V0YWxs|2^20|1:4\r\na=inactive\r\n",
    "v=0\r\nc=IN IP4 0.0.0.0\r\nm=audio 0 RTP/AVP 0\r\nm=video 5000 RTP/AVP 96\r\na=rtpmap:96 VP8/90000\r\n"
  )
  textDecoder("SessionDescription.parse", sdpSeeds)(SessionDescription.parse(_))
  textDecoder("SdpCodec.parseOffer", sdpSeeds)(
    SdpCodec.parseOffer(_, softphone4s.sdp.Capabilities.Default)
  )
  textDecoder(
    "CryptoAttribute.parse",
    List(
      "1 AES_CM_128_HMAC_SHA1_80 inline:WVNfX19zZW1jdGwgKCkgewkyMjA7fQp9CnVubGVzcyAoZ2V0YWxs",
      "2 AES_CM_128_HMAC_SHA1_32 inline:abc|2^31",
      "1 F8_128_HMAC_SHA1_80 inline:x"
    )
  )(CryptoAttribute.parse(_))
  textDecoder(
    "RtpMap.parse",
    List(
      "0 PCMU/8000",
      "101 telephone-event/8000",
      "111 opus/48000/2",
      "96 H264/90000"
    )
  )(RtpMap.parse(_))

  private val pidfSeeds = List(
    """<?xml version="1.0" encoding="UTF-8"?><presence xmlns="urn:ietf:params:xml:ns:pidf" entity="pres:a@b"><tuple id="t1"><status><basic>open</basic></status><contact>sip:a@b</contact><note>hi</note></tuple></presence>""",
    """<p:presence xmlns:p="urn:ietf:params:xml:ns:pidf" xmlns:r="urn:x" entity="e"><p:tuple id="x"><p:status><p:basic>closed</p:basic></p:status><r:a><r:b/></r:a></p:tuple><![CDATA[ <x> ]]></p:presence>""",
    """<!DOCTYPE presence [<!ENTITY a "aaaa">]><presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e">&a;&#x41;&amp;</presence>"""
  )
  textDecoder("Pidf.parse", pidfSeeds)(Presence.parse(_))
  textDecoder(
    "MessageSummary.parse",
    List(
      "Messages-Waiting: yes\r\nMessage-Account: sip:alice@vm.example.com\r\nVoice-Message: 2/8 (0/2)\r\n",
      "Messages-Waiting: no\r\nFax-Message: 1/0\r\nX: y\r\n"
    )
  )(MessageSummary.parse(_))

  // ---- Media -------------------------------------------------------------------------

  private val ssrc = Ssrc.fromBits(0x11223344)

  private val rtpSeeds: List[Bytes] =
    List(
      RtpCodec.encode(
        PayloadType(0).fold(e => sys.error(e), identity),
        1,
        160L,
        ssrc,
        Chunk.array(Array.fill[Byte](160)(0x55))
      ),
      RtpCodec.encode(
        PayloadType(101).fold(e => sys.error(e), identity),
        65535,
        4294967295L,
        ssrc,
        Chunk.array(Array[Byte](1, 2, 3, 4)),
        marker = true
      ),
      RtpCodec.encode(
        PayloadType(8).fold(e => sys.error(e), identity),
        0,
        0L,
        ssrc,
        Chunk.empty
      )
    ).map(_.toArray)
  bytesDecoder("RtpCodec.decode", rtpSeeds)(b => RtpCodec.decode(chunk(b)))
  bytesDecoder(
    "SoftphoneEvent.decode",
    List(Array[Byte](5, 0x0a, 0, 160.toByte), Array[Byte](0, -128, 3, -32))
  )(b => SoftphoneEvent.decode(chunk(b)))

  private val rtcpSeeds: List[Bytes] = {
    val block =
      RtcpPacket.ReportBlock(ssrc, LossFraction.None, 3, 1234L, 9L, 0L, 0L)
    List(
      RtcpCodec.encode(List(RtcpPacket.ReceiverReport(ssrc, List(block)))),
      RtcpCodec.encode(
        List(
          RtcpPacket.SenderReport(
            ssrc,
            RtcpPacket.SenderInfo(0x1234567890abcdefL, 160L, 10L, 1600L),
            List(block, block)
          ),
          RtcpPacket.SourceDescription(
            NonEmptyList.one(RtcpPacket.SourceName(ssrc, "softphone4s@host"))
          )
        )
      ),
      RtcpCodec.encode(
        List(RtcpPacket.Goodbye(NonEmptyList.one(ssrc), Some("bye")))
      )
    ).map(_.toArray)
  }
  bytesDecoder("RtcpCodec.decode", rtcpSeeds)(b => RtcpCodec.decode(chunk(b)))

  private val masterKey: MasterKey =
    MasterKey(
      Chunk.array(Array.tabulate[Byte](16)(i => (i + 1).toByte)),
      Chunk.array(Array.tabulate[Byte](14)(i => (i + 100).toByte))
    )
      .fold(e => sys.error(e.toString), identity)

  private val srtpSeeds: List[Bytes] =
    rtpSeeds.flatMap(p =>
      SrtpSuite.values.toList.flatMap(suite =>
        SrtpOutbound(suite, masterKey)
          .protect(chunk(p))
          .toOption
          .map(_._2.toArray)
      )
    )
  bytesDecoder("SrtpInbound.unprotect", srtpSeeds)(b =>
    SrtpSuite.values.foreach(suite =>
      SrtpInbound(suite, masterKey).unprotect(chunk(b))
    )
  )
  private val srtcpSeeds: List[Bytes] =
    rtcpSeeds.flatMap(p =>
      SrtcpOutbound(masterKey).protect(chunk(p)).toOption.map(_._2.toArray)
    )
  bytesDecoder("SrtcpInbound.unprotect", srtcpSeeds)(b =>
    SrtcpInbound(masterKey).unprotect(chunk(b))
  )

  private val stunSeeds: List[Bytes] = List(
    Stun
      .bindingRequest(
        Stun.TransactionId(Chunk.array(Array.tabulate[Byte](12)(_.toByte)))
      )
      .toArray,
    // a success response with an XOR-MAPPED-ADDRESS
    Array[Byte](0x01, 0x01, 0, 12, 0x21, 0x12, 0xa4.toByte, 0x42) ++ Array
      .tabulate[Byte](12)(_.toByte) ++
      Array[Byte](
        0,
        0x20,
        0,
        8,
        0,
        1,
        0x11,
        0x2b,
        0xe1.toByte,
        0x12,
        0xa5.toByte,
        0x43
      ),
    // an error response
    Array[Byte](0x01, 0x11, 0, 8, 0x21, 0x12, 0xa4.toByte, 0x42) ++ Array
      .tabulate[Byte](12)(_.toByte) ++
      Array[Byte](0, 0x09, 0, 4, 0, 0, 4, 1)
  )
  bytesDecoder("Stun.decode", stunSeeds)(b => Stun.decode(chunk(b)))
}
