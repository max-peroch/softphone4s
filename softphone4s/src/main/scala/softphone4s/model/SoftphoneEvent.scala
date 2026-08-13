package softphone4s.model

import fs2.Chunk

/** RFC 4733 telephone-event RTP payload.
  *
  * Byte layout (4 bytes total): 0 event (8 bits) — 0-9 = digits, 10 = *, 11 =
  * #, 12-15 = A-D 1 E R vol — E=end-bit (bit 7), R=reserved (bit 6), volume (6
  * bits) 2-3 duration — 16-bit sample count at the event clock rate
  */
case class SoftphoneEvent(
    event: Int,
    endBit: Boolean,
    volume: Int,
    duration: Int
)

object SoftphoneEvent {

  private val MinPayloadSize = 4

  def decode(payload: Chunk[Byte]): Option[SoftphoneEvent] =
    if payload.size < MinPayloadSize then None
    else {
      val bytes    = payload.toArray
      val event    = bytes(0) & 0xff
      val endBit   = (bytes(1) & 0x80) != 0
      val volume   = bytes(1) & 0x3f
      val duration = ((bytes(2) & 0xff) << 8) | (bytes(3) & 0xff)
      Some(SoftphoneEvent(event, endBit, volume, duration))
    }
}
