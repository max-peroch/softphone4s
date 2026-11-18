package softphone4s.codec

/** ITU-T G.722 at 64 kbit/s: sub-band ADPCM that turns 16 kHz speech into one
  * byte for every two samples. The signal is split in two bands by a 24-tap
  * quadrature mirror filter; the lower (0 to 4 kHz) is coded in six bits, the
  * upper in two, each with its own adaptive predictor and step size.
  *
  * The codec has memory: the filter, the predictors and the step sizes carry
  * from one sample to the next, so an [[Encoder]] or a [[Decoder]] belongs to
  * one stream and is used by one fiber. That state is held in arrays and
  * updated in place, which is what keeps a codec that runs fifty times a second
  * for every call off the garbage collector; nothing outside this file sees it.
  *
  * The arithmetic is the standard's, in 32-bit integers, and agrees with it bit
  * for bit. The tables are the standard's.
  */
object G722Codec {

  // ---- The standard's tables ------------------------------------------------------

  private val QmfCoefficients: Array[Int] =
    Array(3, -11, 12, 32, -210, 951, 3876, -805, 362, -156, 53, -11)

  /** Decision levels of the lower band's quantiser, for six bits. */
  private val Q6: Array[Int] = Array(0, 35, 72, 110, 150, 190, 233, 276, 323,
    370, 422, 473, 530, 587, 650, 714, 786, 858, 940, 1023, 1121, 1219, 1339,
    1458, 1612, 1765, 1980, 2195, 2557, 2919, 0, 0)

  /** The code of a negative and of a positive difference at each level. */
  private val Iln: Array[Int] = Array(0, 63, 62, 31, 30, 29, 28, 27, 26, 25, 24,
    23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 0)
  private val Ilp: Array[Int] =
    Array(0, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45,
      44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 0)

  /** How the lower band's step size moves with the level just coded. */
  private val Wl: Array[Int]   = Array(-60, -30, 58, 172, 334, 538, 1198, 3042)
  private val Rl42: Array[Int] =
    Array(0, 7, 6, 5, 4, 3, 2, 1, 7, 6, 5, 4, 3, 2, 1, 0)

  /** The step size for each fraction of the logarithmic scale. */
  private val Ilb: Array[Int] = Array(2048, 2093, 2139, 2186, 2233, 2282, 2332,
    2383, 2435, 2489, 2543, 2599, 2656, 2714, 2774, 2834, 2896, 2960, 3025,
    3091, 3158, 3228, 3298, 3371, 3444, 3520, 3597, 3676, 3756, 3838, 3922,
    4008)

  /** Reconstruction levels of the lower band: with four bits (the ones the
    * predictor adapts on) and with six (the ones the output is made from).
    */
  private val Qm4: Array[Int] = Array(0, -20456, -12896, -8968, -6288, -4240,
    -2584, -1200, 20456, 12896, 8968, 6288, 4240, 2584, 1200, 0)
  private val Qm6: Array[Int] = Array(-136, -136, -136, -136, -24808, -21904,
    -19008, -16704, -14984, -13512, -12280, -11192, -10232, -9360, -8576, -7856,
    -7192, -6576, -6000, -5456, -4944, -4464, -4008, -3576, -3168, -2776, -2400,
    -2032, -1688, -1360, -1040, -728, 24808, 21904, 19008, 16704, 14984, 13512,
    12280, 11192, 10232, 9360, 8576, 7856, 7192, 6576, 6000, 5456, 4944, 4464,
    4008, 3576, 3168, 2776, 2400, 2032, 1688, 1360, 1040, 728, 432, 136, -432,
    -136)

  /** The upper band, which has two bits: the code of a negative and of a
    * positive difference, the reconstruction levels, and the step size moves.
    */
  private val Ihn: Array[Int] = Array(0, 1, 0)
  private val Ihp: Array[Int] = Array(0, 3, 2)
  private val Qm2: Array[Int] = Array(-7408, -1616, 7408, 1616)
  private val Wh: Array[Int]  = Array(0, -214, 798)
  private val Rh2: Array[Int] = Array(2, 1, 2, 1)

  // ---- One sub-band --------------------------------------------------------------

  private def saturate(amplitude: Int): Int =
    if amplitude > Short.MaxValue then Short.MaxValue
    else if amplitude < Short.MinValue then Short.MinValue
    else amplitude

  /** The step size `det` that a logarithmic scale factor `nb` stands for; the
    * upper band's runs two octaves further than the lower's (`offset`).
    */
  private def stepSize(nb: Int, offset: Int): Int = {
    val fraction = Ilb((nb >> 6) & 31)
    val shift    = offset - (nb >> 11)
    (if shift < 0 then fraction << -shift else fraction >> shift) << 2
  }

  /** The predictor and the step size of one band (blocks 3 and 4 of the
    * standard). `s` is what the predictor expects the next sample to be.
    *
    * It is mutable on purpose: it is updated for every sample, 16 000 times a
    * second in each band, and a new value for each would load the garbage
    * collector on the audio path. That is safe because a band belongs to one
    * encoder or one decoder, which belongs to one session, and a session is
    * used by one pipe, one frame after another; nothing else holds or shares
    * it.
    */
  private final class Band(initialStep: Int) {
    var s: Int   = 0
    var sp: Int  = 0
    var sz: Int  = 0
    var nb: Int  = 0
    var det: Int = initialStep

    // Index 0 is the newest; the poles use two past values, the zeros six.
    private val r  = new Array[Int](3)
    private val a  = new Array[Int](3)
    private val ap = new Array[Int](3)
    private val p  = new Array[Int](3)
    private val d  = new Array[Int](7)
    private val b  = new Array[Int](7)
    private val bp = new Array[Int](7)
    private val sg = new Array[Int](7)

    /** Takes in the quantised difference `diff` that was just coded and works
      * out the next prediction.
      */
    def update(diff: Int): Unit = {
      // RECONS and PARREC
      d(0) = diff
      r(0) = saturate(s + diff)
      p(0) = saturate(sz + diff)

      // UPPOL2
      var i = 0
      while i < 3 do { sg(i) = p(i) >> 15; i += 1 }
      val scaled = saturate(a(1) << 2)
      var second = if sg(0) == sg(1) then -scaled else scaled
      if second > 32767 then second = 32767
      var pole2 = (second >> 7) + (if sg(0) == sg(2) then 128 else -128)
      pole2 += (a(2) * 32512) >> 15
      ap(2) = math.max(-12288, math.min(12288, pole2))

      // UPPOL1
      sg(0) = p(0) >> 15
      sg(1) = p(1) >> 15
      val step  = if sg(0) == sg(1) then 192 else -192
      val decay = (a(1) * 32640) >> 15
      ap(1) = saturate(step + decay)
      val limit = saturate(15360 - ap(2))
      if ap(1) > limit then ap(1) = limit
      else if ap(1) < -limit then ap(1) = -limit

      // UPZERO
      val push = if diff == 0 then 0 else 128
      sg(0) = diff >> 15
      i = 1
      while i < 7 do {
        sg(i) = d(i) >> 15
        val toward = if sg(i) == sg(0) then push else -push
        bp(i) = saturate(toward + ((b(i) * 32640) >> 15))
        i += 1
      }

      // DELAYA: age the histories, adopting the new coefficients
      i = 6
      while i > 0 do { d(i) = d(i - 1); b(i) = bp(i); i -= 1 }
      i = 2
      while i > 0 do {
        r(i) = r(i - 1)
        p(i) = p(i - 1)
        a(i) = ap(i)
        i -= 1
      }

      // FILTEP: the poles' part of the prediction
      val pole1Part = (a(1) * saturate(r(1) + r(1))) >> 15
      val pole2Part = (a(2) * saturate(r(2) + r(2))) >> 15
      sp = saturate(pole1Part + pole2Part)

      // FILTEZ: the zeros' part
      var zeros = 0
      i = 6
      while i > 0 do {
        zeros += (b(i) * saturate(d(i) + d(i))) >> 15
        i -= 1
      }
      sz = saturate(zeros)

      // PREDIC
      s = saturate(sp + sz)
    }

    /** The lower band's scale factor moves with the level just coded. */
    def adaptLow(level: Int): Unit = {
      nb = math.max(0, math.min(18432, ((nb * 127) >> 7) + Wl(Rl42(level))))
      det = stepSize(nb, 8)
    }

    /** The upper band's, with the two-bit code just seen. */
    def adaptHigh(code: Int): Unit = {
      nb = math.max(0, math.min(22528, ((nb * 127) >> 7) + Wh(Rh2(code))))
      det = stepSize(nb, 10)
    }
  }

  /** The band-splitting filter's window: the last 24 samples. */
  private def shiftIn(window: Array[Int], first: Int, second: Int): Unit = {
    var i = 0
    while i < 22 do { window(i) = window(i + 2); i += 1 }
    window(22) = first
    window(23) = second
  }

  private def sample(bytes: Array[Byte], index: Int): Int =
    (bytes(index * 2 + 1) << 8) | (bytes(index * 2) & 0xff)

  // ---- The encoder -----------------------------------------------------------------

  /** Encodes one stream: 16-bit little-endian PCM at 16 kHz, two samples to a
    * byte.
    */
  final class Encoder {
    private val window = new Array[Int](24)
    private val low    = new Band(32)
    private val high   = new Band(8)

    /** The bytes for `pcm`, which holds a whole number of sample pairs. */
    def encode(pcm: Array[Byte]): Array[Byte] = {
      val out = new Array[Byte](pcm.length / 4)
      var n   = 0
      while n < out.length do {
        shiftIn(window, sample(pcm, 2 * n), sample(pcm, 2 * n + 1))

        // The quadrature mirror filter, keeping every other output.
        var even = 0
        var odd  = 0
        var i    = 0
        while i < 12 do {
          odd += window(2 * i) * QmfCoefficients(i)
          even += window(2 * i + 1) * QmfCoefficients(11 - i)
          i += 1
        }
        val lowSample  = (even + odd) >> 14
        val highSample = (even - odd) >> 14

        out(n) = ((codeHigh(highSample) << 6) | codeLow(lowSample)).toByte
        n += 1
      }
      out
    }

    /** Quantises the lower band's difference to six bits (QUANTL), and adapts.
      */
    private def codeLow(x: Int): Int = {
      val error     = saturate(x - low.s)
      val magnitude = if error >= 0 then error else -(error + 1)
      var level     = 1
      while level < 30 && magnitude >= ((Q6(level) * low.det) >> 12) do
        level += 1
      val code = if error < 0 then Iln(level) else Ilp(level)
      val four = code >> 2
      val diff = (low.det * Qm4(four)) >> 15
      low.adaptLow(four)
      low.update(diff)
      code
    }

    /** The same for the upper band, to two bits (QUANTH). */
    private def codeHigh(x: Int): Int = {
      val error     = saturate(x - high.s)
      val magnitude = if error >= 0 then error else -(error + 1)
      val level     = if magnitude >= ((564 * high.det) >> 12) then 2 else 1
      val code      = if error < 0 then Ihn(level) else Ihp(level)
      val diff      = (high.det * Qm2(code)) >> 15
      high.adaptHigh(code)
      high.update(diff)
      code
    }
  }

  // ---- The decoder -----------------------------------------------------------------

  /** Decodes one stream: a byte to two samples of 16-bit little-endian PCM at
    * 16 kHz.
    */
  final class Decoder {
    private val window = new Array[Int](24)
    private val low    = new Band(32)
    private val high   = new Band(8)

    def decode(codes: Array[Byte]): Array[Byte] = {
      val out = new Array[Byte](codes.length * 4)
      var n   = 0
      while n < codes.length do {
        val code     = codes(n) & 0xff
        val lowCode  = code & 0x3f
        val highCode = (code >> 6) & 0x03
        val four     = lowCode >> 2

        // The lower band is played from six bits and adapted on four.
        val lowSignal = math.max(
          -16384,
          math.min(16383, low.s + ((low.det * Qm6(lowCode)) >> 15))
        )
        val lowDiff = (low.det * Qm4(four)) >> 15
        low.adaptLow(four)
        low.update(lowDiff)

        val highDiff   = (high.det * Qm2(highCode)) >> 15
        val highSignal =
          math.max(-16384, math.min(16383, highDiff + high.s))
        high.adaptHigh(highCode)
        high.update(highDiff)

        // The receive filter joins the bands and makes two samples.
        shiftIn(window, lowSignal + highSignal, lowSignal - highSignal)
        var first  = 0
        var second = 0
        var i      = 0
        while i < 12 do {
          second += window(2 * i) * QmfCoefficients(i)
          first += window(2 * i + 1) * QmfCoefficients(11 - i)
          i += 1
        }
        put(out, 2 * n, saturate(first >> 11))
        put(out, 2 * n + 1, saturate(second >> 11))
        n += 1
      }
      out
    }

    private def put(out: Array[Byte], index: Int, value: Int): Unit = {
      out(index * 2) = value.toByte
      out(index * 2 + 1) = (value >> 8).toByte
    }
  }
}
