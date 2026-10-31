package softphone4s.srtp

import fs2.Chunk
import softphone4s.model.Ssrc

import java.security.MessageDigest

/** The receiving end of an SRTP stream of one source: the keys, the highest
  * index opened (whose high bits are the rollover counter, §3.3.1) and the
  * replay window of the 64 indexes up to it (§3.3.2).
  */
final class SrtpInbound private (
    suite: SrtpSuite,
    keys: SessionKeys,
    source: Option[Ssrc],
    highest: Option[Long],
    window: Long
) {
  import SrtpInbound.*

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
      _ <- unseen(index)
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
    highest.fold(sequence.toLong) { top =>
      val roc     = top >>> 16
      val last    = (top & 0xffff).toInt
      val counter =
        if last < 32768 then
          if sequence - last > 32768 then (roc - 1) & 0xffffffffL else roc
        else if last - 32768 > sequence then (roc + 1) & 0xffffffffL
        else roc
      (counter << 16) | sequence
    }

  /** Whether the packet of this index may be opened: not one that is in the
    * window already, and not one behind it.
    */
  private def unseen(index: Long): Either[SrtpError, Unit] =
    highest match {
      case Some(top) if index <= top =>
        val behind = top - index
        if behind >= WindowSize then Left(SrtpError.TooOld)
        else if ((window >>> behind) & 1L) != 0 then Left(SrtpError.Replayed)
        else Right(())
      case _ => Right(())
    }

  private def counted(ssrc: Ssrc, index: Long): SrtpInbound =
    highest match {
      case Some(top) if index <= top =>
        new SrtpInbound(
          suite,
          keys,
          Some(ssrc),
          highest,
          window | (1L << (top - index))
        )
      case Some(top) =>
        val shift = index - top
        new SrtpInbound(
          suite,
          keys,
          Some(ssrc),
          Some(index),
          if shift >= WindowSize then 1L else (window << shift) | 1L
        )
      case None => new SrtpInbound(suite, keys, Some(ssrc), Some(index), 1L)
    }
}

object SrtpInbound {

  /** How many indexes the replay window covers. */
  val WindowSize: Int = 64

  /** A stream that has received nothing, with the keys that `master` gives. */
  def apply(suite: SrtpSuite, master: MasterKey): SrtpInbound =
    new SrtpInbound(suite, SessionKeys.srtp(master), None, None, 0L)
}
