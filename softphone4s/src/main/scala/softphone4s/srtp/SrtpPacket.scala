package softphone4s.srtp

import fs2.Chunk
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import softphone4s.model.Ssrc

/** What SRTP does to a packet in either direction, as pure functions of the
  * packet, its index and the keys (RFC 3711 §4).
  */
private[srtp] object SrtpPacket {

  /** The parts of an RTP header that SRTP needs.
    *
    * @param length
    *   where the payload begins: the fixed header, the contributing sources and
    *   the extension.
    */
  final case class Header(length: Int, sequence: Int, ssrc: Ssrc)

  def header(packet: Chunk[Byte]): Either[SrtpError, Header] =
    if packet.size < 12 || (packet(0) >> 6 & 0x03) != 2 then
      Left(SrtpError.NotRtp)
    else {
      val sources  = packet(0) & 0x0f
      val fixed    = 12 + 4 * sources
      val extended = (packet(0) & 0x10) != 0
      if packet.size < fixed + (if extended then 4 else 0) then
        Left(SrtpError.NotRtp)
      else {
        val length =
          if !extended then fixed
          else
            fixed + 4 + 4 * (((packet(fixed + 2) & 0xff) << 8) |
              (packet(fixed + 3) & 0xff))
        Either.cond(
          length <= packet.size,
          Header(
            length,
            ((packet(2) & 0xff) << 8) | (packet(3) & 0xff),
            Ssrc.fromBits(
              ((packet(8) & 0xff) << 24) | ((packet(9) & 0xff) << 16) |
                ((packet(10) & 0xff) << 8) | (packet(11) & 0xff)
            )
          ),
          SrtpError.NotRtp
        )
      }
    }

  /** The IV of §4.1.1: `(salt * 2^16) XOR (SSRC * 2^64) XOR (index * 2^16)`. */
  private def iv(salt: Chunk[Byte], ssrc: Ssrc, index: Long): Array[Byte] = {
    val block = new Array[Byte](16)
    salt.copyToArray(block, 0)
    Iterator
      .range(0, 4)
      .foreach(i =>
        block(4 + i) = (block(4 + i) ^ (ssrc.value >>> (24 - 8 * i))).toByte
      )
    Iterator
      .range(0, 6)
      .foreach(i =>
        block(8 + i) = (block(8 + i) ^ (index >>> (40 - 8 * i))).toByte
      )
    block
  }

  /** `data` with its payload, from `from` on, encrypted or decrypted, which is
    * the same.
    */
  def crypt(
      keys: SessionKeys,
      ssrc: Ssrc,
      index: Long,
      data: Array[Byte],
      from: Int
  ): Array[Byte] = {
    val length    = data.length - from
    val keystream = AesCounter.keystream(
      keys.cipherKey.toArray,
      iv(keys.salt, ssrc, index),
      length
    )
    val out = data.clone()
    Iterator.range(0, length).foreach { i =>
      out(from + i) = (out(from + i) ^ keystream(i)).toByte
    }
    out
  }

  /** HMAC-SHA1 over `authenticated` and the rollover counter, cut to `length`
    * bytes (§4.2).
    */
  def tag(
      keys: SessionKeys,
      authenticated: Array[Byte],
      roc: Long,
      length: Int
  ): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA1")
    mac.init(new SecretKeySpec(keys.authKey.toArray, "HmacSHA1"))
    mac.update(authenticated)
    mac.update(
      Array[Byte](
        (roc >>> 24).toByte,
        (roc >>> 16).toByte,
        (roc >>> 8).toByte,
        roc.toByte
      )
    )
    mac.doFinal().take(length)
  }
}
