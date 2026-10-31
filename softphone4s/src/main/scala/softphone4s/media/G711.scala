package softphone4s.media

import fs2.{Chunk, Pipe, Stream}
import softphone4s.codec.G711Codec
import softphone4s.sdp.Codec

/** G.711 (RFC 3551): 8-bit companded samples, 8 000 a second. */
object G711 {

  private val PacketSamples = 160

  private final class Companded(
      val description: Codec,
      toPayload: Array[Byte] => Array[Byte],
      toPcm: Array[Byte] => Array[Byte]
  ) extends AudioCodec {
    val pcmRate: Int                       = 8000
    val payloadPacketBytes: Int            = PacketSamples
    def encoder[F[_]]: Pipe[F, Byte, Byte] =
      AudioCodec.stateless(pcmPacketBytes, toPayload)
    def decoder[F[_]]: Pipe[F, Byte, Byte] =
      _.chunks.flatMap(c => Stream.chunk(Chunk.array(toPcm(c.toArray))))
  }

  /** μ-law, static payload type 0. */
  val Pcmu: AudioCodec =
    Companded(Codec.Pcmu, G711Codec.pcmToMulaw, G711Codec.mulawToPcm)

  /** A-law, static payload type 8. */
  val Pcma: AudioCodec =
    Companded(Codec.Pcma, G711Codec.pcmToAlaw, G711Codec.alawToPcm)
}
