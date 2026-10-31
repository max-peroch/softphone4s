package softphone4s.media

import fs2.Chunk

import scala.concurrent.duration.*

/** Comfort noise (RFC 3389): when the other side stops sending speech to save
  * bandwidth it sends the level of the background noise instead, and we play
  * noise of that level so that the silence does not sound like a dead line.
  *
  * Only the noise level is used. The spectrum the packet may also carry is not,
  * so the noise is white.
  */
object ComfortNoise {

  /** How long noise is made after the last news of it. A sender sends the level
    * when silence begins and again when it changes, which can be minutes apart,
    * so this is long; it ends the noise of a party that has gone quiet in
    * another way.
    */
  val Hold: FiniteDuration = 30.seconds

  /** The level of the noise, in -dBov: 0 is as loud as a signal can be, 127 is
    * none at all.
    */
  final case class Level private (minusDbov: Int) {

    /** The root mean square of the noise, in the units of 16-bit samples. */
    def rms: Double = 32768.0 * math.pow(10.0, -minusDbov / 20.0)
  }

  object Level {

    /** The level in the first byte of a comfort noise payload: its low seven
      * bits (RFC 3389 §3); the top one is reserved.
      */
    def parse(payload: Chunk[Byte]): Option[Level] =
      payload.head.map(b => Level(b & 0x7f))
  }

  /** Noise being made: its level, when we last heard of it, and the state of
    * the generator that makes it.
    */
  final case class Noise(level: Level, heardAt: FiniteDuration, seed: Int) {

    /** Whether it is still to be played at `now`. */
    def isCurrent(now: FiniteDuration): Boolean = now - heardAt < Hold

    /** A frame of `samples` samples of 16-bit little-endian PCM, and the noise
      * with its generator moved on. The samples are uniform, with the level's
      * root mean square (a uniform variable of amplitude `a` has `a / sqrt 3`).
      */
    def frame(samples: Int): (Noise, Chunk[Byte]) = {
      val amplitude = level.rms * math.sqrt(3.0)
      val out       = new Array[Byte](samples * 2)
      var state     = seed
      var i         = 0
      while i < samples do {
        // xorshift32: a few shifts and exclusive ors, never leaves zero.
        state ^= state << 13
        state ^= state >>> 17
        state ^= state << 5
        val unit   = state.toDouble / Int.MaxValue.toDouble // -1 to 1
        val sample =
          math.max(-32768.0, math.min(32767.0, unit * amplitude)).toInt
        out(2 * i) = sample.toByte
        out(2 * i + 1) = (sample >> 8).toByte
        i += 1
      }
      (copy(seed = state), Chunk.array(out))
    }
  }

  object Noise {

    private val InitialSeed = 0x2545f491

    /** News of noise at `level` at `now`, carrying on the generator of the
      * noise before it, if there was some.
      */
    def heard(level: Level, now: FiniteDuration, before: Option[Noise]): Noise =
      Noise(level, now, before.fold(InitialSeed)(_.seed))
  }
}
