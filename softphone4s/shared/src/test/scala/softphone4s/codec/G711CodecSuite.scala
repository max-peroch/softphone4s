package softphone4s.codec

import munit.FunSuite

class G711CodecSuite extends FunSuite {

  // ---- A-law (ITU-T G.711) -----------------------------------------------------------

  test("A-law: the codes of silence, and of the largest samples") {
    assertEquals(G711Codec.encodeAlaw(0) & 0xff, 0xd5)
    assertEquals(G711Codec.encodeAlaw(-1) & 0xff, 0x55)
    assertEquals(G711Codec.encodeAlaw(Short.MaxValue) & 0xff, 0xaa)
    assertEquals(G711Codec.encodeAlaw(Short.MinValue) & 0xff, 0x2a)
  }

  test("A-law: the values of the codes of silence") {
    assertEquals(G711Codec.decodeAlaw(0xd5.toByte).toInt, 8)
    assertEquals(G711Codec.decodeAlaw(0x55.toByte).toInt, -8)
    assertEquals(G711Codec.decodeAlaw(0xaa.toByte).toInt, 32256)
    assertEquals(G711Codec.decodeAlaw(0x2a.toByte).toInt, -32256)
  }

  test("A-law: every code decodes to a value that encodes back to the code") {
    (0 to 255).foreach { code =>
      val sample = G711Codec.decodeAlaw(code.toByte)
      assertEquals(
        G711Codec.encodeAlaw(sample) & 0xff,
        code,
        s"code 0x${code.toHexString} -> $sample"
      )
    }
  }

  test("A-law: a sample comes back within the step of its segment") {
    (Short.MinValue to Short.MaxValue by 7).foreach { s =>
      val back  = G711Codec.decodeAlaw(G711Codec.encodeAlaw(s.toShort)).toInt
      val error = math.abs(back - s)
      // A step is 1/16 of the segment, which is at most 1/16 of the value
      // above the first segments, and 16 below them.
      val allowed = math.max(16, math.abs(s) / 16 + 1)
      assert(error <= allowed, s"$s came back as $back")
    }
  }

  test("A-law: the sign is kept") {
    List(100, 1000, 10000, 30000).foreach { v =>
      assert(G711Codec.decodeAlaw(G711Codec.encodeAlaw(v.toShort)) > 0)
      assert(G711Codec.decodeAlaw(G711Codec.encodeAlaw((-v).toShort)) < 0)
    }
  }

  test("A-law: PCM bytes go to one byte per sample and back") {
    val pcm = Array[Byte](
      0,
      0,
      0x10,
      0x27,
      0xf0.toByte,
      0xd8.toByte
    ) // 0, 10000, -10000
    val alaw = G711Codec.pcmToAlaw(pcm)
    assertEquals(alaw.length, 3)
    val back = G711Codec.alawToPcm(alaw)
    assertEquals(back.length, pcm.length)
    val samples = back
      .grouped(2)
      .map(b => ((b(1) << 8) | (b(0) & 0xff)).toShort.toInt)
      .toList
    assert(math.abs(samples(1) - 10000) < 700, samples.toString)
    assert(math.abs(samples(2) + 10000) < 700, samples.toString)
  }

  // ---- μ-law is untouched ---------------------------------------------------------------

  test("μ-law: silence is 0xff and the largest sample 0x80") {
    assertEquals(G711Codec.encode(0) & 0xff, 0xff)
    assertEquals(G711Codec.encode(Short.MaxValue) & 0xff, 0x80)
  }
}
