package softphone4s.srtp

import fs2.Chunk
import softphone4s.model.Ssrc

import java.security.MessageDigest

/** The receiving end of an SRTCP stream of one source: the keys, and the replay
  * window over the 31-bit index the packets carry (§3.4).
  */
final class SrtcpInbound private (
    keys: SessionKeys,
    source: Option[Ssrc],
    window: ReplayWindow
) {

  /** `packet`, authenticated and decrypted, and the context for the next. A
    * packet is counted once it has been authenticated, never before. One whose
    * E flag says it was not encrypted is authenticated and given as it is.
    */
  def unprotect(
      packet: Chunk[Byte]
  ): Either[SrtpError, (SrtcpInbound, Chunk[Byte])] =
    for {
      ssrc <- SrtcpPacket.source(packet)
      _    <- Either.cond(
        packet.size >= SrtcpPacket.ClearLength + SrtcpPacket.IndexLength +
          SrtcpPacket.TagLength,
        (),
        SrtpError.TooShort(packet.size)
      )
      _ <- source
        .filter(_ != ssrc)
        .fold(Right(()))(_ => Left(SrtpError.SourceChanged(ssrc)))
      authenticated = packet.take(packet.size - SrtcpPacket.TagLength).toArray
      wordAt        = authenticated.length - SrtcpPacket.IndexLength
      word          = ((authenticated(wordAt) & 0xff).toLong << 24) |
        ((authenticated(wordAt + 1) & 0xff) << 16) |
        ((authenticated(wordAt + 2) & 0xff) << 8) |
        (authenticated(wordAt + 3) & 0xff)
      index     = word & 0x7fffffffL
      encrypted = (word & 0x80000000L) != 0
      _ <- window.admits(index)
      _ <- Either.cond(
        MessageDigest.isEqual(
          SrtpPacket.mac(keys, authenticated, SrtcpPacket.TagLength),
          packet.drop(packet.size - SrtcpPacket.TagLength).toArray
        ),
        (),
        SrtpError.AuthenticationFailed
      )
    } yield {
      val rtcp  = authenticated.take(wordAt)
      val plain =
        if encrypted then
          SrtpPacket.crypt(keys, ssrc, index, rtcp, SrtcpPacket.ClearLength)
        else rtcp
      (
        new SrtcpInbound(keys, Some(ssrc), window.counted(index)),
        Chunk.array(plain)
      )
    }
}

object SrtcpInbound {

  /** A stream that has received nothing, with the RTCP keys that `master`
    * gives.
    */
  def apply(master: MasterKey): SrtcpInbound =
    new SrtcpInbound(SessionKeys.srtcp(master), None, ReplayWindow.Empty)
}
