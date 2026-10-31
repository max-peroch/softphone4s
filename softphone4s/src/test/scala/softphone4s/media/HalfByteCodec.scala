package softphone4s.media

import cats.effect.Sync
import fs2.{Chunk, Pipe}
import softphone4s.sdp.Codec

/** A codec of the kind a program would add: it keeps the high byte of every
  * sample, so a 20 ms packet of 320 bytes of PCM is 160 bytes of payload. It
  * has no static payload type, so it is given a dynamic one in an offer, and it
  * has memory of nothing.
  */
object HalfByteCodec extends AudioCodec {

  val token: String      = "x-half"
  val description: Codec = Codec("x-half", 8000, 1, None, Some("mode=1"))
  val pcmRate: Int       = 8000

  def encoder[F[_]: Sync]: Pipe[F, Byte, Chunk[Byte]] =
    AudioCodec.encoding(
      pcmPacketBytes,
      AudioCodec.stateless(pcm => pcm.grouped(2).map(_(1)).toArray)
    )

  def decoder[F[_]: Sync]: Pipe[F, Chunk[Byte], Byte] =
    AudioCodec.decoding(
      AudioCodec.stateless(payload => payload.flatMap(b => Array[Byte](0, b)))
    )
}
