package softphone4s.srtp

import fs2.Chunk

/** The sending end of an SRTCP stream: the keys, and the index of the next
  * packet, which counts up in 31 bits (§3.4). Packets are protected in the
  * order they are sent.
  */
final class SrtcpOutbound private (keys: SessionKeys, index: Long) {

  /** `rtcp`, an RTCP packet or compound packet, encrypted after its first eight
    * bytes, with the index and the tag after it, and the context for the next.
    */
  def protect(
      rtcp: Chunk[Byte]
  ): Either[SrtpError, (SrtcpOutbound, Chunk[Byte])] =
    SrtcpPacket.source(rtcp).map { ssrc =>
      val encrypted = SrtpPacket.crypt(
        keys,
        ssrc,
        index,
        rtcp.toArray,
        SrtcpPacket.ClearLength
      )
      val authenticated =
        encrypted ++ SrtcpPacket.word(SrtcpProtection.Encrypted, index)
      val tag = SrtpPacket.mac(keys, authenticated, SrtcpPacket.TagLength)
      (
        new SrtcpOutbound(keys, (index + 1) & 0x7fffffffL),
        Chunk.array(authenticated) ++ Chunk.array(tag)
      )
    }
}

object SrtcpOutbound {

  /** A stream that has sent nothing, with the RTCP keys that `master` gives. */
  def apply(master: MasterKey): SrtcpOutbound =
    new SrtcpOutbound(SessionKeys.srtcp(master), 0L)
}
