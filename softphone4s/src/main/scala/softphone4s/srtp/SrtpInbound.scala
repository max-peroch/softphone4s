package softphone4s.srtp

import fs2.Chunk
import softphone4s.model.Ssrc

import java.security.MessageDigest

/** The receiving end of an SRTP stream of one source: the keys and the replay
  * window, whose highest index has the rollover counter in its high bits
  * (§3.3.1).
  */
final class SrtpInbound private (
    suite: SrtpSuite,
    keys: SessionKeys,
    source: Option[Ssrc],
    window: ReplayWindow
) {

  /** `packet`, authenticated and decrypted, and the context for the next. A
    * packet is counted once it has been authenticated, never before, so that
    * one forged cannot move the window.
    */
  def unprotect(
      packet: Chunk[Byte]
  ): Either[SrtpError, (SrtpInbound, Chunk[Byte])] =
    for {
      header <- SrtpPacket.header(packet)
      _      <- Either.cond(
        packet.size >= header.length + suite.tagLength,
        (),
        SrtpError.TooShort(packet.size)
      )
      _ <- source
        .filter(_ != header.ssrc)
        .fold(Right(()))(_ => Left(SrtpError.SourceChanged(header.ssrc)))
      index = estimate(header.sequence)
      _ <- window.admits(index)
      authenticated = packet.take(packet.size - suite.tagLength).toArray
      expected      = SrtpPacket.tag(
        keys,
        authenticated,
        index >>> 16,
        suite.tagLength
      )
      _ <- Either.cond(
        MessageDigest.isEqual(
          expected,
          packet.drop(packet.size - suite.tagLength).toArray
        ),
        (),
        SrtpError.AuthenticationFailed
      )
    } yield {
      val opened =
        SrtpPacket.crypt(keys, header.ssrc, index, authenticated, header.length)
      (counted(header.ssrc, index), Chunk.array(opened))
    }

  /** The index a packet with `sequence` has, going by the highest so far: the
    * rollover counter it was sent with is the one that puts it nearest
    * (§3.3.1).
    */
  private def estimate(sequence: Int): Long =
    window.highest.fold(sequence.toLong) { top =>
      val roc     = top >>> 16
      val last    = (top & 0xffff).toInt
      val counter =
        if last < 32768 then
          if sequence - last > 32768 then (roc - 1) & 0xffffffffL else roc
        else if last - 32768 > sequence then (roc + 1) & 0xffffffffL
        else roc
      (counter << 16) | sequence
    }

  private def counted(ssrc: Ssrc, index: Long): SrtpInbound =
    new SrtpInbound(suite, keys, Some(ssrc), window.counted(index))
}

object SrtpInbound {

  /** How many indexes the replay window covers. */
  val WindowSize: Int = ReplayWindow.Size

  /** A stream that has received nothing, with the keys that `master` gives. */
  def apply(suite: SrtpSuite, master: MasterKey): SrtpInbound =
    new SrtpInbound(suite, SessionKeys.srtp(master), None, ReplayWindow.Empty)
}
