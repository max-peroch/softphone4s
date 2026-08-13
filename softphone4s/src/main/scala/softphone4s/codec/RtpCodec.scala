package softphone4s.codec

import fs2.Chunk

/** RFC 3550 RTP packet encode/decode — pure. */
object RtpCodec {

  val HeaderLength = 12 // fixed header, no CSRC, no extension

  case class RtpHeader(
      version: Int,
      payloadType: Int,
      marker: Boolean,
      sequenceNumber: Int,
      timestamp: Long,
      ssrc: Long
  )

  def encode(
      payloadType: Int,
      sequenceNumber: Int,
      timestamp: Long,
      ssrc: Long,
      payload: Chunk[Byte],
      marker: Boolean = false
  ): Chunk[Byte] = {
    val packet = Array.ofDim[Byte](HeaderLength + payload.size)
    packet(0) = 0x80.toByte
    packet(1) = ((if marker then 0x80 else 0x00) | (payloadType & 0x7f)).toByte
    packet(2) = ((sequenceNumber >> 8) & 0xff).toByte
    packet(3) = (sequenceNumber & 0xff).toByte
    packet(4) = ((timestamp >> 24) & 0xff).toByte
    packet(5) = ((timestamp >> 16) & 0xff).toByte
    packet(6) = ((timestamp >> 8) & 0xff).toByte
    packet(7) = (timestamp & 0xff).toByte
    packet(8) = ((ssrc >> 24) & 0xff).toByte
    packet(9) = ((ssrc >> 16) & 0xff).toByte
    packet(10) = ((ssrc >> 8) & 0xff).toByte
    packet(11) = (ssrc & 0xff).toByte
    payload.copyToArray(packet, HeaderLength)
    Chunk.array(packet)
  }

  def decode(bytes: Chunk[Byte]): Option[(RtpHeader, Chunk[Byte])] =
    if bytes.size < HeaderLength then None
    else
      parseRtpHeader(bytes).flatMap { (header, headerLength) =>
        if bytes.size < headerLength then None
        else Some(header, bytes.drop(headerLength))
      }

  private def parseRtpHeader(bytes: Chunk[Byte]): Option[(RtpHeader, Int)] = {
    val version = (bytes(0) >> 6) & 0x03
    if version != 2 then None
    else {
      val csrcCount      = bytes(0) & 0x0f
      val marker         = (bytes(1) & 0x80) != 0
      val payloadType    = bytes(1) & 0x7f
      val sequenceNumber = ((bytes(2) & 0xff) << 8) | (bytes(3) & 0xff)
      val timestamp = ((bytes(4) & 0xffL) << 24) | ((bytes(5) & 0xffL) << 16) |
        ((bytes(6) & 0xffL) << 8) | (bytes(7) & 0xffL)
      val ssrc = ((bytes(8) & 0xffL) << 24) | ((bytes(9) & 0xffL) << 16) |
        ((bytes(10) & 0xffL) << 8) | (bytes(11) & 0xffL)
      val headerLength = HeaderLength + csrcCount * 4
      Some(
        RtpHeader(
          version,
          payloadType,
          marker,
          sequenceNumber,
          timestamp,
          ssrc
        ),
        headerLength
      )
    }
  }
}
