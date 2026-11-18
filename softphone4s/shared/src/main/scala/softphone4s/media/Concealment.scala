package softphone4s.media

import fs2.Chunk

import java.nio.{ByteBuffer, ByteOrder}

/** What to play for a packet that is lost: the frame before it again, a little
  * quieter each time, and then silence. That hides one lost packet, and does
  * not hold a tone for the length of a burst of them.
  *
  * It is made of PCM, so it serves any codec; a codec that can do better with
  * what it knows of its own signal can be given the chance later.
  *
  * @param last
  *   the last frame played, if it was speech.
  * @param losses
  *   how many frames have been concealed in a row.
  */
final case class Concealment(last: Option[Chunk[Byte]], losses: Int) {

  /** A frame of speech was played. */
  def heard(pcm: Chunk[Byte]): Concealment = Concealment(Some(pcm), 0)

  /** Something other than speech was played: there is nothing to repeat. */
  def quiet: Concealment = Concealment.Empty

  /** The frame of `frameBytes` bytes to play for a packet that did not come,
    * and the concealment after it.
    */
  def fill(frameBytes: Int): (Concealment, Chunk[Byte]) = {
    val step  = losses + 1
    val frame = last match {
      case Some(pcm)
          if pcm.size == frameBytes && step < Concealment.FadeFrames =>
        Concealment.scaled(
          pcm,
          Concealment.FadeFrames - step,
          Concealment.FadeFrames
        )
      case _ => Chunk.array(new Array[Byte](frameBytes))
    }
    (copy(losses = step), frame)
  }
}

object Concealment {

  val Empty: Concealment = Concealment(None, 0)

  /** In how many frames the repeated sound fades to nothing: 60 ms. */
  val FadeFrames: Int = 3

  /** `pcm`, 16-bit little-endian samples, times `numerator / denominator`. A
    * byte left over after the last whole sample stays zero.
    */
  private[media] def scaled(
      pcm: Chunk[Byte],
      numerator: Int,
      denominator: Int
  ): Chunk[Byte] = {
    val in  = ByteBuffer.wrap(pcm.toArray).order(ByteOrder.LITTLE_ENDIAN)
    val out = ByteBuffer.allocate(pcm.size).order(ByteOrder.LITTLE_ENDIAN)
    Iterator
      .range(0, pcm.size / 2)
      .foreach(i =>
        out.putShort(
          i * 2,
          (in.getShort(i * 2) * numerator / denominator).toShort
        )
      )
    Chunk.array(out.array())
  }
}
