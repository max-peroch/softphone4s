package softphone4s.codec

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

  // ---- A-law (ITU-T G.711) ------------------------------------------------

  // Top of each segment of the 13-bit magnitude.
  private val alawSegmentEnd: Array[Int] =
    Array(0x1f, 0x3f, 0x7f, 0xff, 0x1ff, 0x3ff, 0x7ff, 0xfff)

  def encodeAlaw(sample: Short): Byte = {
    var magnitude = sample.toInt >> 3 // 16 bits to the 13 A-law has
    val mask      =
      if magnitude >= 0 then 0xd5
      else {
        magnitude = -magnitude - 1
        0x55
      }
    var segment = 0
    while segment < 8 && magnitude > alawSegmentEnd(segment) do segment += 1
    if segment >= 8 then (0x7f ^ mask).toByte
    else {
      val shift    = if segment < 2 then 1 else segment
      val mantissa = (magnitude >> shift) & 0x0f
      (((segment << 4) | mantissa) ^ mask).toByte
    }
  }

  def decodeAlaw(alaw: Byte): Short = {
    val a       = (alaw & 0xff) ^ 0x55
    val segment = (a & 0x70) >> 4
    var value   = (a & 0x0f) << 4
    segment match {
      case 0 => value += 8
      case 1 => value += 0x108
      case _ => value = (value + 0x108) << (segment - 1)
    }
    (if (a & 0x80) != 0 then value else -value).toShort
  }

  def pcmToAlaw(pcm: Array[Byte]): Array[Byte] = {
    val out   = Array.ofDim[Byte](pcm.length / 2)
    var index = 0
    while index < out.length do {
      val sample =
        (((pcm(index * 2 + 1) & 0xff) << 8) | (pcm(index * 2) & 0xff)).toShort
      out(index) = encodeAlaw(sample)
      index += 1
    }
    out
  }

  def alawToPcm(alaw: Array[Byte]): Array[Byte] = {
    val out   = Array.ofDim[Byte](alaw.length * 2)
    var index = 0
    while index < alaw.length do {
      val sample = decodeAlaw(alaw(index))
      out(index * 2) = (sample & 0xff).toByte
      out(index * 2 + 1) = ((sample >> 8) & 0xff).toByte
      index += 1
    }
    out
  }
}
