package softphone4s.media

import fs2.Chunk

/** What the jitter buffer needs of an RTP packet. */
final case class JitterPacket(
    sequence: Int,
    timestamp: Long,
    ssrc: Long,
    marker: Boolean,
    payload: Chunk[Byte]
)
