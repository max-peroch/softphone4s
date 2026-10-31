package softphone4s.srtp

import fs2.Chunk

/** The sending end of an SRTP stream of one source: the keys, and the rollover
  * counter, which goes up each time the sequence number wraps. Packets are
  * protected in the order they are sent.
  */
final class SrtpOutbound private (
    suite: SrtpSuite,
    keys: SessionKeys,
    roc: Long,
    lastSequence: Option[Int]
) {

  /** `packet`, an RTP packet, encrypted and with its tag, and the context for
    * the next.
    */
  def protect(
      packet: Chunk[Byte]
  ): Either[SrtpError, (SrtpOutbound, Chunk[Byte])] =
    SrtpPacket.header(packet).map { header =>
      val counter =
        if lastSequence.exists(_ > header.sequence) then (roc + 1) & 0xffffffffL
        else roc
      val index     = (counter << 16) | header.sequence
      val encrypted =
        SrtpPacket.crypt(
          keys,
          header.ssrc,
          index,
          packet.toArray,
          header.length
        )
      val tag = SrtpPacket.tag(keys, encrypted, counter, suite.tagLength)
      (
        new SrtpOutbound(suite, keys, counter, Some(header.sequence)),
        Chunk.array(encrypted) ++ Chunk.array(tag)
      )
    }
}

object SrtpOutbound {

  /** A stream that has sent nothing, with the keys that `master` gives. */
  def apply(suite: SrtpSuite, master: MasterKey): SrtpOutbound =
    new SrtpOutbound(suite, SessionKeys.srtp(master), 0L, None)
}
