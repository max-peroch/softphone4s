package softphone4s.codec

import fs2.Chunk
import softphone4s.media.RtcpError
import softphone4s.media.RtcpPacket
import softphone4s.media.RtcpPacket.{ReportBlock, SenderInfo}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.collection.mutable.ListBuffer

/** RFC 3550 RTCP compound packets, encoded and decoded — pure. */
object RtcpCodec {

  private val SenderReportType   = 200
  private val ReceiverReportType = 201
  private val DescriptionType    = 202
  private val GoodbyeType        = 203
  private val CanonicalName      = 1

  /** The compound packet of `packets`, one after the other. More than
    * [[RtcpPacket.MaxCount]] blocks or sources in one are cut to that many, and
    * a name or reason to 255 bytes, which are what the wire can say.
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
            b.putInt(ssrc.toInt)
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
            b.putInt(ssrc.toInt)
            kept.foreach(put(b, _))
          }
        )
      )
    case RtcpPacket.SourceDescription(names) =>
      val kept = names.take(RtcpPacket.MaxCount)
      val body = kept.map { (ssrc, name) =>
        val text  = name.getBytes(StandardCharsets.UTF_8).take(255)
        val chunk = 4 + 2 + text.length + 1 // the item, and the end of items
        words(
          chunk,
          b => {
            b.putInt(ssrc.toInt)
            b.put(CanonicalName.toByte)
            b.put(text.length.toByte)
            b.put(text)
          }
        )
      }
      framed(kept.size, DescriptionType, body.flatten.toArray)
    case RtcpPacket.Goodbye(ssrcs, reason) =>
      val kept = ssrcs.take(RtcpPacket.MaxCount)
      val text = reason.map(_.getBytes(StandardCharsets.UTF_8).take(255))
      framed(
        kept.size,
        GoodbyeType,
        words(
          kept.size * 4 + text.fold(0)(1 + _.length),
          b => {
            kept.foreach(s => b.putInt(s.toInt))
            text.foreach { t =>
              b.put(t.length.toByte)
              b.put(t)
            }
          }
        )
      )
    case RtcpPacket.Other(_) => Array.emptyByteArray
  }

  private def put(b: ByteBuffer, block: ReportBlock): Unit = {
    b.putInt(block.ssrc.toInt)
    val lost = math.max(-0x800000, math.min(0x7fffff, block.cumulativeLost))
    b.putInt(
      (math.max(0, math.min(255, block.fractionLost)) << 24) | (lost & 0xffffff)
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

  /** The packets of a compound packet, in order. Those of a type we do not use
    * are [[RtcpPacket.Other]].
    */
  def decode(bytes: Chunk[Byte]): Either[RtcpError, List[RtcpPacket]] = {
    val data = bytes.toArray

    def go(
        offset: Int,
        done: ListBuffer[RtcpPacket]
    ): Either[RtcpError, List[RtcpPacket]] =
      if offset == data.length then Right(done.toList)
      else if data.length - offset < 4 then
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
              .flatMap { packet =>
                done += packet
                go(end, done)
              }
        }
      }

    go(0, ListBuffer.empty)
  }

  private def body(
      kind: Int,
      count: Int,
      b: ByteBuffer
  ): Either[RtcpError, RtcpPacket] = {
    def unsigned(): Long = b.getInt() & 0xffffffffL
    def blocks(): Either[RtcpError, List[ReportBlock]] =
      if b.remaining() < count * 24 then
        Left(RtcpError.BadLength(kind, b.remaining() / 4))
      else
        Right(List.fill(count) {
          val ssrc = unsigned()
          val word = b.getInt()
          val lost = (word << 8) >> 8 // sign-extended from 24 bits
          ReportBlock(
            ssrc,
            (word >>> 24) & 0xff,
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
          val ssrc = unsigned()
          val ntp  = (unsigned() << 32) | unsigned()
          val info = SenderInfo(ntp, unsigned(), unsigned(), unsigned())
          blocks().map(RtcpPacket.SenderReport(ssrc, info, _))
        }
      case ReceiverReportType =>
        if b.remaining() < 4 then
          Left(RtcpError.BadLength(kind, b.remaining() / 4))
        else {
          val ssrc = unsigned()
          blocks().map(RtcpPacket.ReceiverReport(ssrc, _))
        }
      case DescriptionType => description(count, b)
      case GoodbyeType     =>
        if b.remaining() < count * 4 then
          Left(RtcpError.BadLength(kind, b.remaining() / 4))
        else {
          val ssrcs  = List.fill(count)(unsigned())
          val reason =
            if b.remaining() > 0 then {
              val n = b.get() & 0xff
              if n <= b.remaining() then Some(text(b, n)) else None
            } else None
          Right(RtcpPacket.Goodbye(ssrcs, reason))
        }
      case other => Right(RtcpPacket.Other(other))
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
  ): Either[RtcpError, RtcpPacket] = {
    val names                     = ListBuffer.empty[(Long, String)]
    var chunks                    = 0
    var broken: Option[RtcpError] = None
    while chunks < count && broken.isEmpty do {
      if b.remaining() < 4 then
        broken = Some(RtcpError.BadLength(DescriptionType, b.remaining() / 4))
      else {
        val ssrc  = b.getInt() & 0xffffffffL
        var items = true
        while items && broken.isEmpty do {
          if b.remaining() < 1 then items = false
          else {
            val kind = b.get() & 0xff
            if kind == 0 then {
              // The end of the items, then zeros up to the next word.
              while b.position() % 4 != 0 && b.remaining() > 0 do b.get()
              items = false
            } else if b.remaining() < 1 then
              broken = Some(RtcpError.BadLength(DescriptionType, 0))
            else {
              val n = b.get() & 0xff
              if n > b.remaining() then
                broken = Some(
                  RtcpError.BadLength(DescriptionType, b.remaining() / 4)
                )
              else if kind == CanonicalName then names += ssrc -> text(b, n)
              else b.position(b.position() + n)
            }
          }
        }
        chunks += 1
      }
    }
    broken.toLeft(RtcpPacket.SourceDescription(names.toList))
  }
}
