package softphone4s.media

import cats.effect.{Resource, Sync}
import fs2.{Chunk, Pipe}
import softphone4s.codec.G722Codec
import softphone4s.sdp.Codec

/** G.722 (ITU-T), wideband speech at 64 kbit/s: 16 kHz PCM, one byte for every
  * two samples, so 640 bytes of PCM and 160 of payload in 20 ms.
  */
object G722 {

  private final class Implementation extends AudioCodec {
    val token: String      = "g722"
    val description: Codec = Codec.G722
    val pcmRate: Int       = 16000

    def encoder[F[_]: Sync]: Pipe[F, Byte, Chunk[Byte]] =
      AudioCodec.encoding(
        pcmPacketBytes,
        AudioCodec.stateful {
          val encoder = new G722Codec.Encoder
          encoder.encode
        }
      )

    def decoderSession[F[_]: Sync]: Resource[F, AudioCodec.Session] =
      AudioCodec.stateful {
        val decoder = new G722Codec.Decoder
        decoder.decode
      }
  }

  val Wideband: AudioCodec = new Implementation
}
