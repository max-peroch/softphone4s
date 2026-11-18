package softphone4s.sdp

import cats.data.NonEmptyList
import com.comcast.ip4s.*
import fs2.Chunk
import munit.FunSuite
import softphone4s.model.MediaDirection
import softphone4s.srtp.{MasterKey, SrtpPolicy, SrtpSuite}

/** The encryption of a stream in the offer and the answer (RFC 4568 §5). */
class OfferAnswerSrtpSuite extends FunSuite {

  private val local = LocalMedia(ip"192.0.2.10", port"20000", 7, 8)

  private val Suite80 = SrtpSuite.AesCm128HmacSha1_80
  private val Suite32 = SrtpSuite.AesCm128HmacSha1_32

  private def key(n: Int): MasterKey =
    MasterKey(
      Chunk.array(Array.fill[Byte](16)(n.toByte)),
      Chunk.array(Array.fill[Byte](14)((n + 1).toByte))
    ).fold(e => fail(e.message), identity)

  /** Our keys: number 1 in the strong suite, 2 in the short-tag one. */
  private def ours(policy: SrtpPolicy) =
    LocalCrypto(
      policy,
      List(
        CryptoAttribute(1, Suite80, key(10)),
        CryptoAttribute(2, Suite32, key(20))
      )
    )

  /** The capabilities of a call that has `crypto`. */
  private def carrying(crypto: LocalCrypto): Capabilities =
    Capabilities.Default.copy(crypto = crypto)

  private def crlf(lines: String*): String = lines.mkString("", "\r\n", "\r\n")

  private def parsed(text: String): SessionDescription =
    SessionDescription.parse(text).fold(e => fail(e.message), identity)

  private def sdp(
      protocol: String,
      crypto: CryptoAttribute*
  ): SessionDescription =
    parsed(
      crlf(
        List(
          "v=0",
          "o=- 1 1 IN IP4 198.51.100.1",
          "s=-",
          "c=IN IP4 198.51.100.1",
          "t=0 0",
          s"m=audio 30000 $protocol 0 8",
          "a=rtpmap:0 PCMU/8000",
          "a=rtpmap:8 PCMA/8000"
        ) ++ crypto.map(_.attribute.render)*
      )
    )

  private def answer(offer: SessionDescription, policy: SrtpPolicy) =
    OfferAnswer.answer(offer, local, carrying(ours(policy)))

  private def answered(offer: SessionDescription, policy: SrtpPolicy) =
    answer(offer, policy).fold(r => fail(r.message), identity)

  private val theirs80 = CryptoAttribute(1, Suite80, key(30))
  private val theirs32 = CryptoAttribute(2, Suite32, key(40))

  private def cryptoLines(description: SessionDescription) =
    description.media.head.cryptos

  // ---- The offer ---------------------------------------------------------------------

  test("with encryption required we offer secure RTP with our keys") {
    val offer = OfferAnswer.offer(
      local,
      carrying(ours(SrtpPolicy.Required)),
      MediaDirection.SendRecv
    )
    val section = offer.media.head
    assertEquals(section.protocol, MediaProtocol.RtpSavp)
    assertEquals(
      section.cryptos.map(c => (c.tag, c.suite)),
      List((1, Suite80), (2, Suite32))
    )
    assertEquals(
      section.cryptos.map(_.master.bytes),
      List(key(10).bytes, key(20).bytes)
    )
  }

  test(
    "with encryption optional we offer plain RTP with our keys beside it, for the other side to choose"
  ) {
    val offer = OfferAnswer.offer(
      local,
      carrying(ours(SrtpPolicy.Optional)),
      MediaDirection.SendRecv
    )
    assertEquals(offer.media.head.protocol, MediaProtocol.RtpAvp)
    assertEquals(offer.media.head.cryptos.size, 2)
  }

  test(
    "with encryption off the offer is what it was, whatever keys are at hand"
  ) {
    val plain =
      OfferAnswer.offer(local, Capabilities.Default, MediaDirection.SendRecv)
    val off = OfferAnswer.offer(
      local,
      carrying(ours(SrtpPolicy.Off)),
      MediaDirection.SendRecv
    )
    assertEquals(off.render, plain.render)
    assert(!plain.render.contains("crypto"))
  }

  // ---- The answer --------------------------------------------------------------------

  test(
    "secure media is answered as secure, with our key under their number, and the keys are the plan's"
  ) {
    val done    = answered(sdp("RTP/SAVP", theirs80), SrtpPolicy.Required)
    val section = done.description.media.head
    assertEquals(section.protocol, MediaProtocol.RtpSavp)
    assertEquals(section.cryptos.map(c => (c.tag, c.suite)), List((1, Suite80)))
    assertEquals(section.cryptos.map(_.master.bytes), List(key(10).bytes))
    val srtp = done.plan.srtp.getOrElse(fail("not encrypted"))
    assertEquals(srtp.suite, Suite80)
    assertEquals(srtp.send.bytes, key(10).bytes)
    assertEquals(srtp.receive.bytes, key(30).bytes)
  }

  test(
    "the first of their keys in a suite we speak is taken, and answered under its number"
  ) {
    val done = answered(
      sdp("RTP/SAVP", theirs32.copy(tag = 5), theirs80.copy(tag = 6)),
      SrtpPolicy.Required
    )
    assertEquals(
      cryptoLines(done.description).map(c => (c.tag, c.suite)),
      List((5, Suite32))
    )
    assertEquals(done.plan.srtp.map(_.suite), Some(Suite32))
    assertEquals(done.plan.srtp.map(_.send.bytes), Some(key(20).bytes))
  }

  test(
    "keys in a suite we do not speak leave secure media with nothing to use, and it is refused"
  ) {
    val offer = parsed(
      sdp("RTP/SAVP").render.stripSuffix("\r\n") +
        "\r\na=crypto:1 AEAD_AES_128_GCM inline:d0RmdmcmVCspeEc3QGZiNWpVLFJhQX1cfHAwJSoj\r\n"
    )
    assertEquals(
      answer(offer, SrtpPolicy.Optional).left.toOption,
      Some(Reject.NoCommonCrypto)
    )
  }

  test("secure media is not accepted by a phone that does not encrypt") {
    assertEquals(
      answer(sdp("RTP/SAVP", theirs80), SrtpPolicy.Off).left.toOption,
      Some(Reject.NoAudio)
    )
  }

  test(
    "plain RTP with keys beside it is answered plain by a phone that does not encrypt, and the keys are ignored"
  ) {
    val done = answered(sdp("RTP/AVP", theirs80), SrtpPolicy.Off)
    assertEquals(done.plan.srtp, None)
    assertEquals(done.description.media.head.protocol, MediaProtocol.RtpAvp)
    assertEquals(cryptoLines(done.description), Nil)
  }

  test(
    "with encryption optional, plain RTP stays plain and plain RTP with keys becomes secure, in the profile it came in"
  ) {
    val plain = answered(sdp("RTP/AVP"), SrtpPolicy.Optional)
    assertEquals(plain.plan.srtp, None)
    assertEquals(cryptoLines(plain.description), Nil)
    val keyed = answered(sdp("RTP/AVP", theirs80), SrtpPolicy.Optional)
    assertEquals(keyed.plan.srtp.map(_.suite), Some(Suite80))
    assertEquals(keyed.description.media.head.protocol, MediaProtocol.RtpAvp)
    assertEquals(cryptoLines(keyed.description).size, 1)
  }

  test(
    "with encryption required, plain RTP is refused and plain RTP with keys is taken"
  ) {
    assertEquals(
      answer(sdp("RTP/AVP"), SrtpPolicy.Required).left.toOption,
      Some(Reject.NoCommonCrypto)
    )
    assertEquals(
      answered(sdp("RTP/AVP", theirs80), SrtpPolicy.Required).plan.srtp
        .map(_.suite),
      Some(Suite80)
    )
  }

  // ---- Accepting an answer -------------------------------------------------------------

  private def accepted(answerSdp: SessionDescription, policy: SrtpPolicy) =
    OfferAnswer.accept(
      answerSdp,
      carrying(ours(policy)),
      MediaDirection.SendRecv
    )

  test(
    "an answer that picks one of our keys by its number and suite gives the plan"
  ) {
    val plan = accepted(sdp("RTP/SAVP", theirs32), SrtpPolicy.Required)
      .fold(r => fail(r.message), identity)
    val srtp = plan.srtp.getOrElse(fail("not encrypted"))
    assertEquals(srtp.suite, Suite32)
    assertEquals(srtp.send.bytes, key(20).bytes)
    assertEquals(srtp.receive.bytes, key(40).bytes)
  }

  test(
    "an answer with a number we did not offer, or the wrong suite for it, is refused"
  ) {
    assertEquals(
      accepted(
        sdp("RTP/SAVP", theirs80.copy(tag = 9)),
        SrtpPolicy.Required
      ).left.toOption,
      Some(Reject.NoCommonCrypto)
    )
    assertEquals(
      accepted(
        sdp("RTP/SAVP", theirs80.copy(tag = 2)),
        SrtpPolicy.Required
      ).left.toOption,
      Some(Reject.NoCommonCrypto)
    )
  }

  test(
    "with encryption optional, an answer in plain RTP without keys makes the call plain"
  ) {
    val plan = accepted(sdp("RTP/AVP"), SrtpPolicy.Optional)
      .fold(r => fail(r.message), identity)
    assertEquals(plan.srtp, None)
  }

  test(
    "an answer that is secure but has no key is refused, optional or not, and plain is refused when required"
  ) {
    assertEquals(
      accepted(sdp("RTP/SAVP"), SrtpPolicy.Optional).left.toOption,
      Some(Reject.NoCommonCrypto)
    )
    assertEquals(
      accepted(sdp("RTP/AVP"), SrtpPolicy.Required).left.toOption,
      Some(Reject.NoCommonCrypto)
    )
  }

  test("an answer in secure RTP is not accepted by a phone that offered none") {
    assertEquals(
      accepted(sdp("RTP/SAVP", theirs80), SrtpPolicy.Off).left.toOption,
      Some(Reject.NoAudio)
    )
  }

  // ---- Both ends together ----------------------------------------------------------------

  test("each end sends with the key it made and receives with the other's") {
    val offerer = LocalCrypto(
      SrtpPolicy.Required,
      List(CryptoAttribute(1, Suite80, key(10)))
    )
    val answerer = LocalCrypto(
      SrtpPolicy.Required,
      List(CryptoAttribute(1, Suite80, key(50)))
    )
    val offer = OfferAnswer.offer(
      local,
      carrying(offerer),
      MediaDirection.SendRecv
    )
    // Over the wire: rendered and read again.
    val heard = parsed(offer.render)
    val reply = OfferAnswer
      .answer(heard, local, carrying(answerer))
      .fold(r => fail(r.message), identity)
    val offererPlan = OfferAnswer
      .accept(
        parsed(reply.description.render),
        carrying(offerer),
        MediaDirection.SendRecv
      )
      .fold(r => fail(r.message), identity)
    val a = offererPlan.srtp.getOrElse(fail("offerer not encrypted"))
    val b = reply.plan.srtp.getOrElse(fail("answerer not encrypted"))
    assertEquals(a.send.bytes, b.receive.bytes)
    assertEquals(a.receive.bytes, b.send.bytes)
    assertNotEquals(a.send.bytes, a.receive.bytes)
  }

  test("a plan does not print its keys") {
    val plan = answered(sdp("RTP/SAVP", theirs80), SrtpPolicy.Required).plan
    assertEquals(
      plan.srtp.map(_.toString),
      Some("SrtpPlan(AesCm128HmacSha1_80,MasterKey(***),MasterKey(***))")
    )
  }

  test("the encrypted stream still names its codec, as a plain one does") {
    val done = answered(sdp("RTP/SAVP", theirs80), SrtpPolicy.Required)
    assertEquals(done.plan.codec, Codec.Pcmu)
    assertEquals(NonEmptyList.one(done.plan.payloadType.toString).head, "0")
  }
}
