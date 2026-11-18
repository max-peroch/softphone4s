package softphone4s.media

import fs2.Chunk
import munit.FunSuite

import scala.concurrent.duration.*

class ComfortNoiseSuite extends FunSuite {

  private def level(n: Int): ComfortNoise.Level =
    ComfortNoise.Level
      .parse(Chunk.array(Array(n.toByte)))
      .getOrElse(fail("no level"))

  private def samples(pcm: Chunk[Byte]): Vector[Int] =
    pcm.toArray
      .grouped(2)
      .map(b => ((b(1) << 8) | (b(0) & 0xff)).toShort.toInt)
      .toVector

  private def noiseAt(n: Int) =
    ComfortNoise.Noise.heard(level(n), Duration.Zero, None)

  private def rms(xs: Vector[Int]): Double =
    math.sqrt(xs.map(x => x.toDouble * x).sum / xs.size)

  test(
    "the level is the low seven bits of the first byte; the top bit is reserved"
  ) {
    assertEquals(
      ComfortNoise.Level.parse(Chunk.array(Array[Byte](0x2d))).map(_.minusDbov),
      Some(45)
    )
    assertEquals(
      ComfortNoise.Level
        .parse(Chunk.array(Array(0xad.toByte)))
        .map(_.minusDbov),
      Some(45)
    )
    assertEquals(ComfortNoise.Level.parse(Chunk.empty[Byte]), None)
  }

  test("more bytes than the level, the spectrum, do not change it") {
    assertEquals(
      ComfortNoise.Level
        .parse(Chunk.array(Array[Byte](30, 1, 2, 3)))
        .map(_.minusDbov),
      Some(30)
    )
  }

  test("each 20 dB lower is a tenth of the amplitude") {
    assertEqualsDouble(level(20).rms / level(40).rms, 10.0, 1e-9)
    assertEqualsDouble(level(0).rms, 32768.0, 1e-9)
  }

  test("a frame has the samples asked for, 16-bit each") {
    val (_, frame) = noiseAt(40).frame(160)
    assertEquals(frame.size, 320)
  }

  test("the noise has the loudness of its level, and no lean one way") {
    List(20, 40, 60).foreach { n =>
      val (_, frame) = noiseAt(n).frame(8000)
      val xs         = samples(frame)
      val expected   = level(n).rms
      assert(
        math.abs(rms(xs) - expected) < expected * 0.1,
        s"level $n: ${rms(xs)} against $expected"
      )
      assert(
        math.abs(xs.sum.toDouble / xs.size) < expected * 0.1,
        s"level $n leans"
      )
    }
  }

  test(
    "a level of 127 is no sound at all, and 0 is as loud as a sample goes without wrapping"
  ) {
    assertEquals(samples(noiseAt(127).frame(160)._2).forall(_ == 0), true)
    val loud = samples(noiseAt(0).frame(1000)._2)
    assert(loud.forall(x => x >= -32768 && x <= 32767))
    assert(loud.exists(_ > 20000) && loud.exists(_ < -20000))
  }

  test(
    "the generator moves on: the next frame is another, and the same start gives the same noise"
  ) {
    val noise       = noiseAt(40)
    val (next, one) = noise.frame(160)
    val (_, two)    = next.frame(160)
    assertNotEquals(one, two)
    assertEquals(noise.frame(160)._2, one)
    // Noise heard of again carries on from where the generator was.
    val again = ComfortNoise.Noise.heard(level(50), 5.seconds, Some(next))
    assertEquals(again.seed, next.seed)
  }

  test(
    "noise is current for 30 seconds after the last news of it, and then not"
  ) {
    val noise = ComfortNoise.Noise.heard(level(40), 10.seconds, None)
    assert(noise.isCurrent(10.seconds))
    assert(noise.isCurrent(39.seconds))
    assert(!noise.isCurrent(40.seconds))
    assert(!noise.isCurrent(5.minutes))
  }
}
