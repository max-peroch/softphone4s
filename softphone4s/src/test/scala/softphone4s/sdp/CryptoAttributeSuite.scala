package softphone4s.sdp

import fs2.Chunk
import munit.FunSuite
import softphone4s.srtp.{MasterKey, SrtpSuite}

import java.util.Base64

class CryptoAttributeSuite extends FunSuite {

  private val Encoded = "d0RmdmcmVCspeEc3QGZiNWpVLFJhQX1cfHAwJSoj"

  private def key(n: Int): MasterKey =
    MasterKey(
      Chunk.array(Array.fill[Byte](16)(n.toByte)),
      Chunk.array(Array.fill[Byte](14)((n + 1).toByte))
    ).fold(e => fail(e.message), identity)

  test("a line is read: its number, its suite, and the key and salt inline") {
    val read = CryptoAttribute
      .parse(s"1 AES_CM_128_HMAC_SHA1_80 inline:$Encoded")
      .fold(e => fail(e.message), identity)
    assertEquals(read.tag, 1)
    assertEquals(read.suite, SrtpSuite.AesCm128HmacSha1_80)
    assertEquals(
      read.master.bytes.toArray.toList,
      Base64.getDecoder.decode(Encoded).toList
    )
  }

  test(
    "a lifetime is read and ignored, the other suite is known, and a window hint is allowed"
  ) {
    val line = s"7 AES_CM_128_HMAC_SHA1_32 inline:$Encoded|2^20 WSH=64"
    assertEquals(
      CryptoAttribute.parse(line).map(c => (c.tag, c.suite)),
      Right((7, SrtpSuite.AesCm128HmacSha1_32))
    )
    assert(
      CryptoAttribute
        .parse(s"1 AES_CM_128_HMAC_SHA1_80 inline:$Encoded|1000")
        .isRight
    )
  }

  test("a line written is read back as it was") {
    val written     = CryptoAttribute(3, SrtpSuite.AesCm128HmacSha1_80, key(5))
    val Some(value) = written.attribute.value: @unchecked
    assertEquals(written.attribute.name, "crypto")
    val read = CryptoAttribute.parse(value).fold(e => fail(e.message), identity)
    assertEquals((read.tag, read.suite), (3, SrtpSuite.AesCm128HmacSha1_80))
    assertEquals(read.master.bytes, written.master.bytes)
  }

  test(
    "a master key identifier is not supported, since it fills every packet"
  ) {
    assertEquals(
      CryptoAttribute.parse(
        s"1 AES_CM_128_HMAC_SHA1_80 inline:$Encoded|2^20|1:32"
      ),
      Left(CryptoError.UnsupportedMki)
    )
  }

  test("session parameters that change the keys are not supported") {
    assertEquals(
      CryptoAttribute.parse(s"1 AES_CM_128_HMAC_SHA1_80 inline:$Encoded KDR=3"),
      Left(CryptoError.UnsupportedParameter("KDR=3"))
    )
    assert(
      CryptoAttribute
        .parse(s"1 AES_CM_128_HMAC_SHA1_80 inline:$Encoded UNENCRYPTED_SRTCP")
        .isLeft
    )
  }

  test(
    "a suite we do not speak, or a key method other than inline, is refused"
  ) {
    assertEquals(
      CryptoAttribute.parse(s"1 AEAD_AES_128_GCM inline:$Encoded"),
      Left(CryptoError.UnknownSuite("AEAD_AES_128_GCM"))
    )
    assert(
      CryptoAttribute.parse("1 AES_CM_128_HMAC_SHA1_80 file:key.bin").isLeft
    )
    assert(
      CryptoAttribute
        .parse(s"1 AES_CM_128_HMAC_SHA1_80 inline:$Encoded;inline:$Encoded")
        .isLeft
    )
  }

  test("a key that is not base64, or not 30 bytes, is refused") {
    assert(
      CryptoAttribute
        .parse("1 AES_CM_128_HMAC_SHA1_80 inline:!!!not base64!!!")
        .isLeft
    )
    val short = Base64.getEncoder.encodeToString(new Array[Byte](29))
    assertEquals(
      CryptoAttribute
        .parse(s"1 AES_CM_128_HMAC_SHA1_80 inline:$short")
        .left
        .toOption
        .map(_.message.startsWith("bad key")),
      Some(true)
    )
    assert(
      CryptoAttribute
        .parse(s"1 AES_CM_128_HMAC_SHA1_80 inline:$Encoded|soon")
        .isLeft
    )
  }

  test(
    "a number that is not one to nine digits and more than nothing, or a line cut short, is refused"
  ) {
    assert(
      CryptoAttribute.parse(s"0 AES_CM_128_HMAC_SHA1_80 inline:$Encoded").isLeft
    )
    assert(
      CryptoAttribute.parse(s"x AES_CM_128_HMAC_SHA1_80 inline:$Encoded").isLeft
    )
    assert(
      CryptoAttribute
        .parse(s"1234567890 AES_CM_128_HMAC_SHA1_80 inline:$Encoded")
        .isLeft
    )
    assert(CryptoAttribute.parse("1 AES_CM_128_HMAC_SHA1_80").isLeft)
    assert(CryptoAttribute.parse("").isLeft)
  }

  test("a stream lists the lines it can use, in order, and drops the others") {
    val section = MediaDescription(
      MediaKind.Audio,
      com.comcast.ip4s.Port.fromInt(4000).get,
      MediaProtocol.RtpSavp,
      List("0"),
      None,
      List(
        Attribute.valued("crypto", s"1 AEAD_AES_128_GCM inline:$Encoded"),
        Attribute
          .valued("crypto", s"2 AES_CM_128_HMAC_SHA1_32 inline:$Encoded"),
        Attribute.valued(
          "crypto",
          s"3 AES_CM_128_HMAC_SHA1_80 inline:$Encoded|2^20|1:4"
        ),
        Attribute.valued("crypto", s"4 AES_CM_128_HMAC_SHA1_80 inline:$Encoded")
      )
    )
    assertEquals(section.cryptos.map(_.tag), List(2, 4))
  }

  test("a key is never printed, by the attribute or by what holds it") {
    val attribute =
      CryptoAttribute(1, SrtpSuite.AesCm128HmacSha1_80, key(5)).attribute
    assert(
      !attribute.toString.contains(attribute.value.get.takeRight(20)),
      attribute.toString
    )
    assertEquals(attribute.toString, "Attribute(crypto,Some(***))")
    // Still rendered for the wire.
    assert(
      attribute.render.startsWith("a=crypto:1 AES_CM_128_HMAC_SHA1_80 inline:")
    )
    assertEquals(
      Attribute.valued("rtpmap", "0 PCMU/8000").toString,
      "Attribute(rtpmap,Some(0 PCMU/8000))"
    )
  }
}
