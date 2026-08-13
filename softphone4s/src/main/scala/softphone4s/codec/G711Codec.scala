package softphone4s.codec

import fs2.{Chunk, Pipe, Stream}

// PCM format: 16-bit signed little-endian at 8kHz (javax.sound.sampled default)
object G711Codec {

  private val Bias            = 0x84 // 132
  private val MaxLinearSample = 32767

  // Exponent look-up: index = (sample + BIAS) >> 8 (0..255)
  private val expLut: Array[Byte] = Array[Byte](
    0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7
  )

  // Per-segment base value for decode (segment 0..7)
  private val decodeBase: Array[Int] =
    Array(0, 132, 396, 924, 1980, 4092, 8316, 16764)

  def encode(sample: Short): Byte = {
    var linearSample = sample.toInt
    val sign         = if linearSample < 0 then {
      linearSample = -linearSample - 1; 0x80
    } else 0
    linearSample += Bias
    if linearSample > MaxLinearSample then linearSample = MaxLinearSample
    val exponent = expLut(linearSample >> 7)
    val mantissa = (linearSample >> (exponent + 3)) & 0x0f
    (~(sign | (exponent << 4) | mantissa)).toByte
  }

  def decode(ulaw: Byte): Short = {
    val complement   = ~ulaw & 0xff
    val sign         = complement & 0x80
    val exponent     = (complement >> 4) & 0x07
    val mantissa     = complement & 0x0f
    var linearSample = decodeBase(exponent) + (mantissa << (exponent + 3))
    if sign != 0 then linearSample = -linearSample
    linearSample.toShort
  }

  def pcmToMulaw(pcm: Array[Byte]): Array[Byte] = {
    val out   = Array.ofDim[Byte](pcm.length / 2)
    var index = 0
    while index < out.length do {
      val lowByte  = pcm(index * 2) & 0xff
      val highByte = pcm(index * 2 + 1) & 0xff
      val sample   = ((highByte << 8) | lowByte).toShort
      out(index) = encode(sample)
      index += 1
    }
    out
  }

  def mulawToPcmPipe[F[_]]: Pipe[F, Byte, Byte] =
    _.chunks.flatMap(c => Stream.chunk(Chunk.array(mulawToPcm(c.toArray))))

  def pcmToMulawPipe[F[_]](frameBytes: Int): Pipe[F, Byte, Byte] =
    _.chunkN(frameBytes, allowFewer = false)
      .flatMap(c => Stream.chunk(Chunk.array(pcmToMulaw(c.toArray))))

  def mulawToPcm(mulaw: Array[Byte]): Array[Byte] = {
    val out   = Array.ofDim[Byte](mulaw.length * 2)
    var index = 0
    while index < mulaw.length do {
      val sample = decode(mulaw(index))
      out(index * 2) = (sample & 0xff).toByte
      out(index * 2 + 1) = ((sample >> 8) & 0xff).toByte
      index += 1
    }
    out
  }
}
