package softphone4s.codec

import fs2.Chunk
import cats.data.NonEmptyList
import softphone4s.media.{LossFraction, RtcpError, RtcpPacket}
import softphone4s.model.Ssrc
import softphone4s.media.RtcpPacket.{ReportBlock, SenderInfo}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

/** RFC 3550 RTCP compound packets, encoded and decoded — pure. */
object RtcpCodec {

  private val SenderReportType   = 200
  private val ReceiverReportType = 201
  private val DescriptionType    = 202
  private val GoodbyeType        = 203
  private val CanonicalName      = 1

  /** The compound packet of `packets`, one after the other. More than
    * [[softphone4s.media.RtcpPacket.MaxCount]] blocks or sources in one are cut
    * to that many, and a name or reason to 255 bytes, which are what the wire
    * can say.
    */
  def encode(packets: List[RtcpPacket]): Chunk[Byte] =
    Chunk.array(packets.flatMap(single).toArray)

  private def single(packet: RtcpPacket): Array[Byte] = packet match {
    case RtcpPacket.SenderReport(ssrc, info, blocks) =>
      val kept = blocks.take(RtcpPacket.MaxCount)
      framed(
        kept.size,
        SenderReportType,
        words(
          24 + kept.size * 24,
          b => {
            b.putInt(ssrc.bits)
            b.putInt((info.ntp >>> 32).toInt)
            b.putInt(info.ntp.toInt)
            b.putInt(info.rtpTimestamp.toInt)
            b.putInt(info.packets.toInt)
            b.putInt(info.octets.toInt)
            kept.foreach(put(b, _))
          }
        )
      )
    case RtcpPacket.ReceiverReport(ssrc, blocks) =>
      val kept = blocks.take(RtcpPacket.MaxCount)
      framed(
        kept.size,
        ReceiverReportType,
        words(
          4 + kept.size * 24,
          b => {
            b.putInt(ssrc.bits)
            kept.foreach(put(b, _))
          }
        )
      )
    case RtcpPacket.SourceDescription(names) =>
      val kept = names.toList.take(RtcpPacket.MaxCount)
      val body = kept.map { case RtcpPacket.SourceName(ssrc, name) =>
        val text  = clipped(name)
        val chunk = 4 + 2 + text.length + 1 // the item, and the end of items
        words(
          chunk,
          b => {
            b.putInt(ssrc.bits)
            b.put(CanonicalName.toByte)
            b.put(text.length.toByte)
            b.put(text)
          }
        )
      }
      framed(kept.size, DescriptionType, body.flatten.toArray)
    case RtcpPacket.Goodbye(ssrcs, reason) =>
      val kept = ssrcs.toList.take(RtcpPacket.MaxCount)
      val text = reason.map(clipped)
      framed(
        kept.size,
        GoodbyeType,
        words(
          kept.size * 4 + text.fold(0)(1 + _.length),
          b => {
            kept.foreach(s => b.putInt(s.bits))
            text.foreach { t =>
              b.put(t.length.toByte)
              b.put(t)
            }
          }
        )
      )
  }

  /** `text` as UTF-8 of at most 255 bytes, cut between characters and not in
    * the middle of one.
    */
  private def clipped(text: String): Array[Byte] = {
    val bytes = text.getBytes(StandardCharsets.UTF_8)
    if bytes.length <= 255 then bytes
    else {
      // A continuation byte is 10xxxxxx: back up to the byte that begins one.
      val cut = Iterator
        .iterate(255)(_ - 1)
        .find(i => i == 0 || (bytes(i) & 0xc0) != 0x80)
        .getOrElse(0)
      bytes.take(cut)
    }
  }

  private def put(b: ByteBuffer, block: ReportBlock): Unit = {
    b.putInt(block.ssrc.bits)
    val lost = math.max(-0x800000, math.min(0x7fffff, block.cumulativeLost))
    b.putInt(
      (block.fractionLost.value << 24) | (lost & 0xffffff)
    )
    b.putInt(block.highestSequence.toInt)
    b.putInt(block.jitter.toInt)
    b.putInt(block.lastSender.toInt)
    b.putInt(block.delaySinceLastSender.toInt)
  }

  /** `fill` writes `size` bytes, to which zeros are added up to a whole number
    * of 32-bit words. Every item list and string ends with at least one zero
    * byte, which SDES needs and the others do not mind.
    */
  private def words(size: Int, fill: ByteBuffer => Unit): Array[Byte] = {
    val padded = (size + 3) / 4 * 4
    val b      = ByteBuffer.allocate(padded)
    fill(b)
    b.array()
  }

  private def framed(
      count: Int,
      packetType: Int,
      body: Array[Byte]
  ): Array[Byte] = {
    val b = ByteBuffer.allocate(4 + body.length)
    b.put((0x80 | count).toByte)
    b.put(packetType.toByte)
    b.putShort((body.length / 4).toShort)
    b.put(body)
    b.array()
  }

  /** The packets of a compound packet, in order. Those of a type we do not use,
    * and sets of no sources, are skipped.
    */
  def decode(bytes: Chunk[Byte]): Either[RtcpError, List[RtcpPacket]] = {
    val data = bytes.toArray

    @tailrec
    def go(
        offset: Int,
        done: List[RtcpPacket]
    ): Either[RtcpError, List[RtcpPacket]] =
      if offset == data.length then Right(done.reverse)
      else
        packetAt(data, offset) match {
          case Left(error)            => Left(error)
          case Right((packet, after)) => go(after, packet.toList ::: done)
        }

    go(0, Nil)
  }

  /** The packet that begins at `offset`, and where the next one begins. */
  private def packetAt(
      data: Array[Byte],
      offset: Int
  ): Either[RtcpError, (Option[RtcpPacket], Int)] =
    if data.length - offset < 4 then
      Left(RtcpError.Truncated(4, data.length - offset))
    else {
      val first   = data(offset) & 0xff
      val version = first >> 6
      val padded  = (first & 0x20) != 0
      val count   = first & 0x1f
      val kind    = data(offset + 1) & 0xff
      val length  =
        (((data(offset + 2) & 0xff) << 8) | (data(offset + 3) & 0xff)) * 4
      val end = offset + 4 + length
      if version != 2 then Left(RtcpError.BadVersion(version))
      else if end > data.length then
        Left(RtcpError.Truncated(end - offset, data.length - offset))
      else {
        val pad = if padded && length > 0 then data(end - 1) & 0xff else 0
        if padded && (pad == 0 || pad > length) then
          Left(RtcpError.BadPadding(kind))
        else
          body(kind, count, ByteBuffer.wrap(data, offset + 4, length - pad))
            .map(_ -> end)
      }
    }

  private def body(
      kind: Int,
      count: Int,
      b: ByteBuffer
  ): Either[RtcpError, Option[RtcpPacket]] = {
    def unsigned(): Long = b.getInt() & 0xffffffffL
    def blocks(): Either[RtcpError, List[ReportBlock]] =
      if b.remaining() < count * 24 then
        Left(RtcpError.BadLength(kind, b.remaining() / 4))
      else
        Right(List.fill(count) {
          val ssrc = Ssrc.fromBits(b.getInt())
          val word = b.getInt()
          val lost = (word << 8) >> 8 // sign-extended from 24 bits
          ReportBlock(
            ssrc,
            LossFraction.fromBits(word >>> 24),
            lost,
            unsigned(),
            unsigned(),
            unsigned(),
            unsigned()
          )
        })

    kind match {
      case SenderReportType =>
        if b.remaining() < 24 then
          Left(RtcpError.BadLength(kind, b.remaining() / 4))
        else {
          val ssrc = Ssrc.fromBits(b.getInt())
          val ntp  = (unsigned() << 32) | unsigned()
          val info = SenderInfo(ntp, unsigned(), unsigned(), unsigned())
          blocks().map(bs => Some(RtcpPacket.SenderReport(ssrc, info, bs)))
        }
      case ReceiverReportType =>
        if b.remaining() < 4 then
          Left(RtcpError.BadLength(kind, b.remaining() / 4))
        else {
          val ssrc = Ssrc.fromBits(b.getInt())
          blocks().map(bs => Some(RtcpPacket.ReceiverReport(ssrc, bs)))
        }
      case DescriptionType => description(count, b)
      case GoodbyeType     =>
        if b.remaining() < count * 4 then
          Left(RtcpError.BadLength(kind, b.remaining() / 4))
        else {
          val ssrcs  = List.fill(count)(Ssrc.fromBits(b.getInt()))
          val reason =
            if b.remaining() > 0 then {
              val n = b.get() & 0xff
              if n <= b.remaining() then Some(text(b, n)) else None
            } else None
          Right(
            NonEmptyList
              .fromList(ssrcs)
              .map(RtcpPacket.Goodbye(_, reason))
          )
        }
      case _ => Right(None)
    }
  }

  private def text(b: ByteBuffer, n: Int): String = {
    val bytes = new Array[Byte](n)
    b.get(bytes)
    new String(bytes, StandardCharsets.UTF_8)
  }

  /** The canonical names among the items; other items are skipped. */
  private def description(
      count: Int,
      b: ByteBuffer
  ): Either[RtcpError, Option[RtcpPacket]] = {
    type Names = List[RtcpPacket.SourceName]

    // The items of one source, up to the zero that ends them.
    @tailrec
    def items(ssrc: Ssrc, names: Names): Either[RtcpError, Names] =
      if b.remaining() < 1 then Right(names)
      else {
        val kind = b.get() & 0xff
        if kind == 0 then {
          // The end of the items, then zeros up to the next word.
          while b.position() % 4 != 0 && b.remaining() > 0 do b.get()
          Right(names)
        } else if b.remaining() < 1 then
          Left(RtcpError.BadLength(DescriptionType, 0))
        else {
          val n = b.get() & 0xff
          if n > b.remaining() then
            Left(RtcpError.BadLength(DescriptionType, b.remaining() / 4))
          else if kind == CanonicalName then
            items(ssrc, RtcpPacket.SourceName(ssrc, text(b, n)) :: names)
          else {
            b.position(b.position() + n)
            items(ssrc, names)
          }
        }
      }

    @tailrec
    def sources(left: Int, names: Names): Either[RtcpError, Names] =
      if left == 0 then Right(names)
      else if b.remaining() < 4 then
        Left(RtcpError.BadLength(DescriptionType, b.remaining() / 4))
      else
        items(Ssrc.fromBits(b.getInt()), names) match {
          case Left(error) => Left(error)
          case Right(more) => sources(left - 1, more)
        }

    sources(count, Nil).map(names =>
      NonEmptyList
        .fromList(names.reverse)
        .map(RtcpPacket.SourceDescription(_))
    )
  }
}
