package softphone4s.media

import cats.effect.{Resource, Sync}
import fs2.{Chunk, Pipe}
import softphone4s.codec.G711Codec
import softphone4s.sdp.Codec

/** G.711 (RFC 3551): 8-bit companded samples, 8 000 a second. */
object G711 {

  private final class Companded(
      val token: String,
      val description: Codec,
      toPayload: AudioCodec.Session,
      toPcm: AudioCodec.Session
  ) extends AudioCodec {
    val pcmRate: Int = 8000

    def encoder[F[_]: Sync]: Pipe[F, Byte, Chunk[Byte]] =
      AudioCodec.encoding(pcmPacketBytes, AudioCodec.stateless(toPayload))

    def decoderSession[F[_]: Sync]: Resource[F, AudioCodec.Session] =
      AudioCodec.stateless(toPcm)
  }

  /** μ-law, static payload type 0. */
  val Pcmu: AudioCodec =
    Companded("pcmu", Codec.Pcmu, G711Codec.pcmToMulaw, G711Codec.mulawToPcm)

  /** A-law, static payload type 8. */
  val Pcma: AudioCodec =
    Companded("pcma", Codec.Pcma, G711Codec.pcmToAlaw, G711Codec.alawToPcm)
}
