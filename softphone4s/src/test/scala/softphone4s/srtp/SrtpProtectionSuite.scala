package softphone4s.srtp

import fs2.Chunk
import munit.FunSuite
import softphone4s.SipLiterals.pt
import softphone4s.codec.RtpCodec
import softphone4s.model.Ssrc

class SrtpProtectionSuite extends FunSuite {

  private def hex(text: String): Chunk[Byte] =
    Chunk.array(text.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)

  private def show(bytes: Chunk[Byte]): String =
    bytes.toArray.map(b => f"${b & 0xff}%02x").mkString

  private def master(
      key: String = "E1F97A0D3E018BE0D64FA32C06DE4139",
      salt: String = "0EC675AD498AFEEBB6960B3AABE6"
  ) =
    MasterKey(hex(key), hex(salt)).fold(e => fail(e.message), identity)

  private val payload =
    Chunk.array(Array.tabulate[Byte](28)(i => (i + 1).toByte))

  private def rtp(
      sequence: Int,
      ssrc: Long = 0xcafebabeL,
      body: Chunk[Byte] = payload
  ): Chunk[Byte] =
    RtpCodec.encode(
      pt(15),
      sequence,
      0xdecafbadL,
      Ssrc.fromBits(ssrc.toInt),
      body
    )

  private val Suite80 = SrtpSuite.AesCm128HmacSha1_80
  private val Suite32 = SrtpSuite.AesCm128HmacSha1_32

  private def protect(out: SrtpOutbound, packet: Chunk[Byte]) =
    out.protect(packet).fold(e => fail(e.message), identity)

  private def open(in: SrtpInbound, packet: Chunk[Byte]) =
    in.unprotect(packet)

  // ---- RFC 3711 Appendix B ----------------------------------------------------------

  test(
    "the session keys are the ones RFC 3711 B.3 derives from its master key"
  ) {
    val keys = SessionKeys.srtp(master())
    assertEquals(show(keys.cipherKey), "c61e7a93744f39ee10734afe3ff7a087")
    assertEquals(show(keys.salt), "30cbbc08863d8c85d49db34a9ae1")
    assertEquals(
      show(keys.authKey),
      "cebe321f6ff7716b6fd4ab49af256a156d38baa4"
    )
  }

  test(
    "the keystream is the one RFC 3711 B.2 gives, at its start and at its end"
  ) {
    val key           = hex("2B7E151628AED2A6ABF7158809CF4F3C").toArray
    val iv            = hex("F0F1F2F3F4F5F6F7F8F9FAFBFCFD0000").toArray
    val ks            = Chunk.array(AesCounter.keystream(key, iv, 1044512))
    def block(n: Int) = show(ks.drop(n * 16).take(16))
    assertEquals(block(0), "e03ead0935c95e80e166b16dd92b4eb4")
    assertEquals(block(1), "d23513162b02d0f72a43a2fe4a5f97ab")
    assertEquals(block(2), "41e95b3bb0a2e8dd477901e4fca894c0")
    assertEquals(block(65279), "ec8cdf7398607cb0f2d21675ea9ea1e4")
    assertEquals(block(65280), "362b7c3c6773516318a077d7fc5073ae")
    assertEquals(block(65281), "6a2cc3787889374fbeb4c81b17ba6c44")
  }

  // ---- Whole packets, against an independent implementation ------------------------

  // Made by a second implementation of RFC 3711 on OpenSSL's AES (Python), which
  // agrees with the RFC's key derivation vectors above.
  private val Reference80 =
    "800f1234decafbadcafebabee4fc74e34934d47b2e0572b2323880b98e0280f6a05bb39348bd2c28e3b75c0f3cc9bd3ddcaa"
  private val Reference32 =
    "800f1234decafbadcafebabee4fc74e34934d47b2e0572b2323880b98e0280f6a05bb39348bd2c28e3b75c0f"
  private val ReferenceBeforeWrap =
    "800fffffdecafbadcafebabe59c73e532901add62e4bff331cb2b903f93d8b09116c9e62eef0f833ade66aba0e9ed1476264"
  private val ReferenceAfterWrap =
    "800f0000decafbadcafebabe8e455182323a138964d817315b5892e1306e24b140404b86c66fe4793076f67ebf9a73c486d7"

  test("a packet is protected as the independent implementation protects it") {
    val (_, sent) = protect(SrtpOutbound(Suite80, master()), rtp(0x1234))
    assertEquals(show(sent), Reference80)
  }

  test("with the shorter tag it is the same but for the last six bytes") {
    val (_, sent) = protect(SrtpOutbound(Suite32, master()), rtp(0x1234))
    assertEquals(show(sent), Reference32)
    assertEquals(sent.size, rtp(0x1234).size + 4)
  }

  test(
    "the rollover counter is in the IV and the tag: a packet after the wrap matches too"
  ) {
    val first      = protect(SrtpOutbound(Suite80, master()), rtp(0xffff))
    val (_, after) = protect(first._1, rtp(0))
    assertEquals(show(first._2), ReferenceBeforeWrap)
    assertEquals(show(after), ReferenceAfterWrap)
  }

  test(
    "what the independent implementation made is opened and gives the packet back"
  ) {
    val Right((_, opened)) =
      open(SrtpInbound(Suite80, master()), hex(Reference80)): @unchecked
    assertEquals(opened, rtp(0x1234))
  }

  // ---- Round trips ------------------------------------------------------------------

  test("only the payload is encrypted, and the tag follows it") {
    val (_, sent) = protect(SrtpOutbound(Suite80, master()), rtp(7))
    assertEquals(sent.take(12), rtp(7).take(12))
    assertNotEquals(sent.drop(12).take(28), payload)
    assertEquals(sent.size, rtp(7).size + 10)
  }

  test(
    "a run of packets across the wrap of the sequence number is opened in order"
  ) {
    val sequences = List(65533, 65534, 65535, 0, 1, 2)
    val (_, sent) = sequences.foldLeft(
      (SrtpOutbound(Suite80, master()), List.empty[Chunk[Byte]])
    ) { case ((out, done), n) =>
      val (next, packet) = protect(out, rtp(n))
      (next, done :+ packet)
    }
    val (_, opened) =
      sent.foldLeft((SrtpInbound(Suite80, master()), List.empty[Chunk[Byte]])) {
        case ((in, done), packet) =>
          val Right((next, plain)) = open(in, packet): @unchecked
          (next, done :+ plain)
      }
    assertEquals(opened, sequences.map(rtp(_)))
  }

  test("a packet that arrives across the wrap out of order is still opened") {
    val sequences = List(65534, 65535, 0, 1)
    val (_, sent) = sequences.foldLeft(
      (SrtpOutbound(Suite80, master()), Vector.empty[Chunk[Byte]])
    ) { case ((out, done), n) =>
      val (next, packet) = protect(out, rtp(n))
      (next, done :+ packet)
    }
    // 65534, then 0 and 1 (the counter has gone up), then 65535 (it has not).
    val order       = List(0, 2, 3, 1)
    val (_, opened) = order.foldLeft(
      (SrtpInbound(Suite80, master()), List.empty[Chunk[Byte]])
    ) { case ((in, done), k) =>
      val Right((next, plain)) = open(in, sent(k)): @unchecked
      (next, done :+ plain)
    }
    assertEquals(opened, order.map(k => rtp(sequences(k))))
  }

  test("a packet with contributing sources and an extension keeps its header") {
    // V=2, X=1, CC=1: one contributing source, and an extension of one word.
    val header =
      hex("910f1234decafbadcafebabe" + "11223344" + "bede0001" + "55667788")
    val packet    = header ++ payload
    val (_, sent) = protect(SrtpOutbound(Suite80, master()), packet)
    assertEquals(sent.take(header.size.toInt), header)
    assertNotEquals(
      sent.drop(header.size.toInt).take(payload.size.toInt),
      payload
    )
    val Right((_, opened)) =
      open(SrtpInbound(Suite80, master()), sent): @unchecked
    assertEquals(opened, packet)
  }

  // ---- What is refused ----------------------------------------------------------------

  private val sealed1 = protect(SrtpOutbound(Suite80, master()), rtp(1))._2

  private def flipped(packet: Chunk[Byte], at: Int): Chunk[Byte] = {
    val bytes = packet.toArray
    bytes(at) = (bytes(at) ^ 0x01).toByte
    Chunk.array(bytes)
  }

  test("a packet changed in its payload, its header or its tag is refused") {
    val in = SrtpInbound(Suite80, master())
    assertEquals(
      open(in, flipped(sealed1, 20)).left.toOption,
      Some(SrtpError.AuthenticationFailed)
    )
    assertEquals(
      open(in, flipped(sealed1, 3)).left.toOption,
      Some(SrtpError.AuthenticationFailed)
    )
    assertEquals(
      open(in, flipped(sealed1, sealed1.size.toInt - 1)).left.toOption,
      Some(SrtpError.AuthenticationFailed)
    )
  }

  test("a packet protected with another key is refused") {
    val other = master(key = "000102030405060708090A0B0C0D0E0F")
    assertEquals(
      open(SrtpInbound(Suite80, other), sealed1).left.toOption,
      Some(SrtpError.AuthenticationFailed)
    )
  }

  test("a packet opened twice is a replay") {
    val Right((in, _)) =
      open(SrtpInbound(Suite80, master()), sealed1): @unchecked
    assertEquals(open(in, sealed1).left.toOption, Some(SrtpError.Replayed))
  }

  test(
    "packets within the window may arrive in any order, once, and ones behind it are too old"
  ) {
    val sequences = (1 to 70).toList
    val (_, sent) = sequences.foldLeft(
      (SrtpOutbound(Suite80, master()), Vector.empty[Chunk[Byte]])
    ) { case ((out, done), n) =>
      val (next, packet) = protect(out, rtp(n))
      (next, done :+ packet)
    }
    // The newest first, then one 63 behind it (inside the window), then one 64
    // behind (outside).
    val Right((a, _)) =
      open(SrtpInbound(Suite80, master()), sent(69)): @unchecked
    val inside = open(a, sent(69 - 63))
    assert(inside.isRight, inside)
    assertEquals(open(a, sent(69 - 64)).left.toOption, Some(SrtpError.TooOld))
    val Right((b, _)) = inside: @unchecked
    assertEquals(open(b, sent(69 - 63)).left.toOption, Some(SrtpError.Replayed))
  }

  test(
    "a forged packet does not move the window, so the real ones still come"
  ) {
    val (out2, second) =
      protect(SrtpOutbound(Suite80, master()), rtp(1)) match {
        case (out, first) => (first, protect(out, rtp(2))._2)
      }
    val Right((in, _)) = open(SrtpInbound(Suite80, master()), out2): @unchecked
    // A packet far ahead with a tag that does not fit.
    val forged =
      flipped(protect(SrtpOutbound(Suite80, master()), rtp(5000))._2, 30)
    assertEquals(
      open(in, forged).left.toOption,
      Some(SrtpError.AuthenticationFailed)
    )
    assert(open(in, second).isRight)
  }

  test(
    "a packet from another source than the one the context counts is refused"
  ) {
    val Right((in, _)) =
      open(SrtpInbound(Suite80, master()), sealed1): @unchecked
    val other = protect(SrtpOutbound(Suite80, master()), rtp(2, ssrc = 5L))._2
    assertEquals(
      open(in, other).left.toOption,
      Some(SrtpError.SourceChanged(Ssrc.fromBits(5)))
    )
  }

  test("what is not RTP, or too short for its tag, is refused") {
    val in = SrtpInbound(Suite80, master())
    assertEquals(
      open(in, Chunk.array(Array[Byte](1, 2, 3))).left.toOption,
      Some(SrtpError.NotRtp)
    )
    assertEquals(
      open(in, Chunk.array(Array.fill[Byte](12)(0))).left.toOption,
      Some(SrtpError.NotRtp)
    )
    assertEquals(
      open(in, rtp(1, body = Chunk.empty)).left.toOption,
      Some(SrtpError.TooShort(12))
    )
    assertEquals(
      SrtpOutbound(Suite80, master())
        .protect(Chunk.array(Array[Byte](1)))
        .left
        .toOption,
      Some(SrtpError.NotRtp)
    )
  }

  // ---- Keys -----------------------------------------------------------------------------

  test("a master key is 16 bytes and a salt 14") {
    assertEquals(
      MasterKey(
        Chunk.array(new Array[Byte](15)),
        Chunk.array(new Array[Byte](14))
      ).left.toOption,
      Some(SrtpError.BadKeyLength(16, 15))
    )
    assertEquals(
      MasterKey(
        Chunk.array(new Array[Byte](16)),
        Chunk.array(new Array[Byte](13))
      ).left.toOption,
      Some(SrtpError.BadSaltLength(14, 13))
    )
  }

  test("a key and salt joined in 30 bytes, as SDES carries them, is split") {
    val joined =
      hex("E1F97A0D3E018BE0D64FA32C06DE4139" + "0EC675AD498AFEEBB6960B3AABE6")
    assertEquals(MasterKey.fromBytes(joined).map(_.bytes), Right(joined))
    assert(MasterKey.fromBytes(joined.take(29)).isLeft)
    assert(MasterKey.fromBytes(joined ++ Chunk.singleton(0.toByte)).isLeft)
  }

  test("the RTCP keys are different from the RTP keys") {
    val rtpKeys  = SessionKeys.srtp(master())
    val rtcpKeys = SessionKeys.srtcp(master())
    assertNotEquals(rtpKeys.cipherKey, rtcpKeys.cipherKey)
    assertNotEquals(rtpKeys.authKey, rtcpKeys.authKey)
    assertNotEquals(rtpKeys.salt, rtcpKeys.salt)
  }

  test("keys never print themselves") {
    assertEquals(master().toString, "MasterKey(***)")
    assertEquals(SessionKeys.srtp(master()).toString, "SessionKeys(***)")
  }

  test("a suite is found by the name SDP gives it") {
    assertEquals(SrtpSuite.fromName("AES_CM_128_HMAC_SHA1_80"), Some(Suite80))
    assertEquals(SrtpSuite.fromName("AES_CM_128_HMAC_SHA1_32"), Some(Suite32))
    assertEquals(SrtpSuite.fromName("AEAD_AES_128_GCM"), None)
  }
}
