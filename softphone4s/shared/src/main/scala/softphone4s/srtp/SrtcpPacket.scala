package softphone4s.srtp

import fs2.Chunk
import softphone4s.model.Ssrc

/** What SRTCP (RFC 3711 §3.4) lays out around an RTCP packet. */
private[srtp] object SrtcpPacket {

  /** The first RTCP header and the SSRC of its sender stay in the clear. */
  val ClearLength: Int = 8

  /** The E flag and the 31-bit index. */
  val IndexLength: Int = 4

  /** SRTCP always authenticates with 80 bits, whatever the suite says of SRTP.
    */
  val TagLength: Int = 10

  /** The source an RTCP packet is from, if it is one. */
  def source(packet: Chunk[Byte]): Either[SrtpError, Ssrc] =
    if packet.size < ClearLength || (packet(0) >> 6 & 0x03) != 2 then
      Left(SrtpError.NotRtp)
    else
      Right(
        Ssrc.fromBits(
          ((packet(4) & 0xff) << 24) | ((packet(5) & 0xff) << 16) |
            ((packet(6) & 0xff) << 8) | (packet(7) & 0xff)
        )
      )

  /** The index word as it goes on the wire. */
  def word(protection: SrtcpProtection, index: Long): Array[Byte] = {
    val flag = protection match {
      case SrtcpProtection.Encrypted => 0x80000000L
      case SrtcpProtection.Plain     => 0L
    }
    val value = flag | index
    Array[Byte](
      (value >>> 24).toByte,
      (value >>> 16).toByte,
      (value >>> 8).toByte,
      value.toByte
    )
  }
}
