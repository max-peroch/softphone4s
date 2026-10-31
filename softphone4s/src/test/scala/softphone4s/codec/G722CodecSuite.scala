package softphone4s.codec

import munit.FunSuite

import scala.util.Using

/** G.722 against the output of a reference implementation, bit for bit. */
class G722CodecSuite extends FunSuite {

  private def resource(name: String): Array[Byte] =
    Using.resource(getClass.getResourceAsStream(s"/g722/$name"))(
      _.readAllBytes()
    )

  private lazy val input   = resource("input.pcm")
  private lazy val encoded = resource("reference.g722")
  private lazy val decoded = resource("reference-decoded.pcm")

  private def samples(pcm: Array[Byte]): Vector[Int] =
    pcm
      .grouped(2)
      .map(b => ((b(1) << 8) | (b(0) & 0xff)).toShort.toInt)
      .toVector

  test("the vectors are what the test thinks: 3 200 samples, 1 600 bytes") {
    assertEquals(input.length, 6400)
    assertEquals(encoded.length, 1600)
    assertEquals(decoded.length, 6400)
  }

  test("our encoder gives the reference's bytes, all of them") {
    val ours = new G722Codec.Encoder().encode(input)
    assertEquals(ours.toList, encoded.toList)
  }

  test("our decoder gives the reference's samples, all of them") {
    val ours = new G722Codec.Decoder().decode(encoded)
    assertEquals(ours.toList, decoded.toList)
  }

  test(
    "a stream cut into packets gives what the whole gives: the state carries on"
  ) {
    val encoder = new G722Codec.Encoder
    val packets = input.grouped(640).map(encoder.encode).toList
    assertEquals(packets.flatten, encoded.toList)
    val decoder = new G722Codec.Decoder
    val played  = encoded.grouped(160).map(decoder.decode).toList
    assertEquals(played.flatten, decoded.toList)
  }

  test("two streams do not share state") {
    val first  = new G722Codec.Encoder
    val second = new G722Codec.Encoder
    first.encode(input.take(640))
    assertEquals(second.encode(input).toList, encoded.toList)
  }

  test("a packet is 640 bytes of PCM and 160 of payload") {
    assertEquals(
      new G722Codec.Encoder().encode(new Array[Byte](640)).length,
      160
    )
    assertEquals(
      new G722Codec.Decoder().decode(new Array[Byte](160)).length,
      640
    )
  }

  test("silence stays silent, give or take the rounding of the quantiser") {
    val back = new G722Codec.Decoder().decode(
      new G722Codec.Encoder().encode(new Array[Byte](6400))
    )
    assert(
      samples(back).forall(s => math.abs(s) < 16),
      samples(back).max.toString
    )
  }

  test("what is decoded follows what was encoded, 22 samples later") {
    val a      = samples(input)
    val b      = samples(decoded)
    val signal = (200 until 3000).map(i => a(i).toDouble * a(i)).sum
    val noise  = (200 until 3000).map(i => math.pow(a(i) - b(i + 22), 2)).sum
    val snr    = 10 * math.log10(signal / noise)
    assert(snr > 15, s"$snr dB")
  }
}
