package softphone4s.media

import fs2.Chunk
import softphone4s.model.{PayloadType, Ssrc}

/** What the jitter buffer needs of an RTP packet. Audio, comfort noise and
  * telephone events all go through it, since they share one run of sequence
  * numbers; the payload type says which a packet is.
  */
final case class JitterPacket(
    sequence: Int,
    timestamp: Long,
    ssrc: Ssrc,
    marker: Boolean,
    payloadType: PayloadType,
    payload: Chunk[Byte]
)
