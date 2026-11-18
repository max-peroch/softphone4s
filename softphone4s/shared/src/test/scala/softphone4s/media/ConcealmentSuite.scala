package softphone4s.media

import fs2.Chunk
import munit.FunSuite

class ConcealmentSuite extends FunSuite {

  private def frameOf(sample: Int, samples: Int = 160): Chunk[Byte] =
    Chunk.array(
      Array.tabulate[Byte](samples * 2)(i =>
        if i % 2 == 0 then sample.toByte else (sample >> 8).toByte
      )
    )

  private def samples(pcm: Chunk[Byte]): List[Int] =
    pcm.toArray
      .grouped(2)
      .map(b => ((b(1) << 8) | (b(0) & 0xff)).toShort.toInt)
      .toList

  test("with nothing before it, a lost frame is silence of the length asked") {
    val (_, frame) = Concealment.Empty.fill(320)
    assertEquals(frame.size, 320)
    assert(samples(frame).forall(_ == 0))
  }

  test(
    "a lost frame repeats the one before, quieter each time, and then is silence"
  ) {
    val start         = Concealment.Empty.heard(frameOf(3000))
    val (one, first)  = start.fill(320)
    val (two, second) = one.fill(320)
    val (_, third)    = two.fill(320)
    assertEquals(samples(first).distinct, List(2000))
    assertEquals(samples(second).distinct, List(1000))
    assert(samples(third).forall(_ == 0))
  }

  test("the fade works on negative samples too") {
    val (_, frame) = Concealment.Empty.heard(frameOf(-3000)).fill(320)
    assertEquals(samples(frame).distinct, List(-2000))
  }

  test("speech heard again starts the fade over") {
    val faded = Concealment.Empty.heard(frameOf(3000)).fill(320)._1.fill(320)._1
    val (_, frame) = faded.heard(frameOf(900)).fill(320)
    assertEquals(samples(frame).distinct, List(600))
  }

  test("something other than speech leaves nothing to repeat") {
    val (_, frame) = Concealment.Empty.heard(frameOf(3000)).quiet.fill(320)
    assert(samples(frame).forall(_ == 0))
  }

  test("a frame of another length than the one asked for is not repeated") {
    val (_, frame) = Concealment.Empty.heard(frameOf(3000, 80)).fill(320)
    assertEquals(frame.size, 320)
    assert(samples(frame).forall(_ == 0))
  }
}
