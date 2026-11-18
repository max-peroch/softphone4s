package softphone4s.srtp

import fs2.Chunk
import munit.FunSuite
import softphone4s.model.Ssrc

class SrtcpProtectionSuite extends FunSuite {

  private def hex(text: String): Chunk[Byte] =
    Chunk.array(text.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)

  private def show(bytes: Chunk[Byte]): String =
    bytes.toArray.map(b => f"${b & 0xff}%02x").mkString

  private val master =
    MasterKey(
      hex("E1F97A0D3E018BE0D64FA32C06DE4139"),
      hex("0EC675AD498AFEEBB6960B3AABE6")
    ).fold(e => fail(e.message), identity)

  /** A receiver report's first eight bytes, from cafebabe, and 16 more. */
  private val rtcp = hex(
    "80c90001cafebabe" + "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
  )

  private def protect(out: SrtcpOutbound, packet: Chunk[Byte]) =
    out.protect(packet).fold(e => fail(e.message), identity)

  // Made by the independent implementation of RFC 3711 in Python on OpenSSL's AES,
  // which agrees with the RFC's key derivation vectors.
  private val ReferenceIndex0 =
    "80c90001cafebabeba96289207656176e51f8f0bf9b2bb3b8000000074149c80d0a95976b97b"
  private val ReferenceIndex1 =
    "80c90001cafebabe7a220a53eb89b4b1bdfa9141fe71a0ac800000011a6d36823e19f01a6608"
  private val ReferenceLast =
    "80c90001cafebabeaf0e7bef6ba9a54018d97efac44f7a2dffffffff2402f6014a268455e165"

  test("the RTCP keys are derived with labels 3, 4 and 5") {
    val keys = SessionKeys.srtcp(master)
    assertEquals(show(keys.cipherKey), "4c1aa45a81f73d61c800bbb00fbb1eaa")
    assertEquals(show(keys.salt), "9581c7ad87b3e530bf3e4454a8b3")
    assertEquals(show(keys.authKey), "8d54534feb49ae8e7993a6bd0b844fc323a93dfd")
  }

  test(
    "a packet is protected as the independent implementation protects it, and the index counts up"
  ) {
    val (next, first) = protect(SrtcpOutbound(master), rtcp)
    val (_, second)   = protect(next, rtcp)
    assertEquals(show(first), ReferenceIndex0)
    assertEquals(show(second), ReferenceIndex1)
  }

  test("what it made is opened and gives the packet back") {
    val Right((_, opened)) =
      SrtcpInbound(master).unprotect(hex(ReferenceIndex0)): @unchecked
    assertEquals(opened, rtcp)
  }

  test("the largest index, 2^31 - 1, is read") {
    val Right((_, opened)) =
      SrtcpInbound(master).unprotect(hex(ReferenceLast)): @unchecked
    assertEquals(opened, rtcp)
  }

  test(
    "the first eight bytes stay clear and the rest is encrypted, with the index and the tag after it"
  ) {
    val (_, sent) = protect(SrtcpOutbound(master), rtcp)
    assertEquals(sent.take(8), rtcp.take(8))
    assertNotEquals(sent.drop(8).take(16), rtcp.drop(8))
    assertEquals(sent.size, rtcp.size + 4 + 10)
    // The E flag is set, and the index is 0.
    assertEquals(show(sent.drop(24).take(4)), "80000000")
  }

  test(
    "a run of packets is opened in order, and out of order within the window"
  ) {
    val (_, sent) =
      (0 until 5).foldLeft((SrtcpOutbound(master), Vector.empty[Chunk[Byte]])) {
        case ((out, done), _) =>
          val (next, packet) = protect(out, rtcp)
          (next, done :+ packet)
      }
    val order  = List(0, 2, 1, 4, 3)
    val opened = order
      .foldLeft((SrtcpInbound(master), List.empty[Chunk[Byte]])) {
        case ((in, done), k) =>
          val Right((next, plain)) = in.unprotect(sent(k)): @unchecked
          (next, done :+ plain)
      }
      ._2
    assertEquals(opened, List.fill(5)(rtcp))
  }

  test(
    "a packet changed in its clear part, its encrypted part, its index or its tag is refused"
  ) {
    val (_, sent)                     = protect(SrtcpOutbound(master), rtcp)
    def flipped(at: Int): Chunk[Byte] = {
      val bytes = sent.toArray
      bytes(at) = (bytes(at) ^ 0x01).toByte
      Chunk.array(bytes)
    }
    List(1, 6, 12, 26, sent.size.toInt - 1).foreach { at =>
      assertEquals(
        SrtcpInbound(master).unprotect(flipped(at)).left.toOption,
        Some(SrtpError.AuthenticationFailed),
        s"byte $at"
      )
    }
  }

  test("a packet protected with another key is refused") {
    val other = MasterKey(hex("000102030405060708090A0B0C0D0E0F"), master.salt)
      .fold(e => fail(e.message), identity)
    val (_, sent) = protect(SrtcpOutbound(other), rtcp)
    assertEquals(
      SrtcpInbound(master).unprotect(sent).left.toOption,
      Some(SrtpError.AuthenticationFailed)
    )
  }

  test("RTP keys do not open RTCP: the two streams have keys of their own") {
    val rtpKeys  = SessionKeys.srtp(master)
    val rtcpKeys = SessionKeys.srtcp(master)
    assertNotEquals(rtpKeys.authKey, rtcpKeys.authKey)
    assertNotEquals(rtpKeys.cipherKey, rtcpKeys.cipherKey)
  }

  test(
    "a packet opened twice is a replay, and one behind the window is too old"
  ) {
    val (_, sent) = (0 until 70).foldLeft(
      (SrtcpOutbound(master), Vector.empty[Chunk[Byte]])
    ) { case ((out, done), _) =>
      val (next, packet) = protect(out, rtcp)
      (next, done :+ packet)
    }
    val Right((a, _)) = SrtcpInbound(master).unprotect(sent(69)): @unchecked
    assertEquals(a.unprotect(sent(69)).left.toOption, Some(SrtpError.Replayed))
    assertEquals(
      a.unprotect(sent(69 - 64)).left.toOption,
      Some(SrtpError.TooOld)
    )
    assert(a.unprotect(sent(69 - 63)).isRight)
  }

  test("a forged packet does not move the window") {
    val (next, first)  = protect(SrtcpOutbound(master), rtcp)
    val (_, second)    = protect(next, rtcp)
    val Right((in, _)) = SrtcpInbound(master).unprotect(first): @unchecked
    val forged         = {
      val bytes = hex(ReferenceLast).toArray
      bytes(12) = (bytes(12) ^ 0x01).toByte
      Chunk.array(bytes)
    }
    assertEquals(
      in.unprotect(forged).left.toOption,
      Some(SrtpError.AuthenticationFailed)
    )
    assert(in.unprotect(second).isRight)
  }

  test("a packet from another source than the one counted is refused") {
    val (_, sent)      = protect(SrtcpOutbound(master), rtcp)
    val Right((in, _)) = SrtcpInbound(master).unprotect(sent): @unchecked
    val other          = rtcp.take(4) ++ hex("00000005") ++ rtcp.drop(8)
    val (_, fromOther) = protect(SrtcpOutbound(master), other)
    assertEquals(
      in.unprotect(fromOther).left.toOption,
      Some(SrtpError.SourceChanged(Ssrc.fromBits(5)))
    )
  }

  test(
    "a packet whose E flag says it is not encrypted is authenticated and given as it is"
  ) {
    // Built by hand, as a sender that does not encrypt would.
    val keys   = SessionKeys.srtcp(master)
    val body   = rtcp.toArray ++ SrtcpPacket.word(SrtcpProtection.Plain, 3)
    val tagged =
      Chunk.array(body ++ SrtpPacket.mac(keys, body, SrtcpPacket.TagLength))
    val Right((_, opened)) = SrtcpInbound(master).unprotect(tagged): @unchecked
    assertEquals(opened, rtcp)
  }

  test(
    "what is not RTCP, or too short to hold an index and a tag, is refused"
  ) {
    val in = SrtcpInbound(master)
    assertEquals(
      in.unprotect(Chunk.array(Array[Byte](1, 2))).left.toOption,
      Some(SrtpError.NotRtp)
    )
    assertEquals(
      in.unprotect(rtcp.take(8) ++ hex("0102030405")).left.toOption,
      Some(SrtpError.TooShort(13))
    )
    assertEquals(
      SrtcpOutbound(master)
        .protect(Chunk.array(Array[Byte](0x40, 1, 2, 3, 4, 5, 6, 7)))
        .left
        .toOption,
      Some(SrtpError.NotRtp)
    )
  }

  test("a packet of nothing but the header is protected too") {
    val bare      = rtcp.take(8)
    val (_, sent) = protect(SrtcpOutbound(master), bare)
    assertEquals(sent.size, 8 + 4 + 10)
    val Right((_, opened)) = SrtcpInbound(master).unprotect(sent): @unchecked
    assertEquals(opened, bare)
  }
}
