package softphone4s.transport

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.DatagramSocket
import fs2.{Chunk, Stream}
import softphone4s.codec.RtpCodec
import softphone4s.media.JitterPacket
import softphone4s.model.{DtmfDigit, PayloadType, SoftphoneEvent, Ssrc}

object RtpTransport {

  /** The packets of a stream arriving on `socket`: those of payload type
    * `audioType`, the telephone events (RFC 4733) on `eventType` and the
    * comfort noise (RFC 3389) on `comfortType`, if there are any. They share
    * one run of sequence numbers, so all three go on to the jitter buffer,
    * which would otherwise take the ones that are not audio for packets lost.
    * `heardFrom` is told where each came from, and its SSRC.
    *
    * `open` turns what came on the wire into the RTP packet in it, or into
    * nothing if it is to be dropped: it is how SRTP is taken off, and a packet
    * that does not authenticate goes no further.
    *
    * A telephone event is also acted on as it arrives, not when its turn comes
    * to be played: DTMF end-bit packets are deduped by (timestamp, event) and
    * delivered via `onDtmf`.
    */
  def receiveBytes[F[_]: Async](
      socket: DatagramSocket[F],
      lastDtmfKey: Ref[F, (Long, Int)],
      onDtmf: DtmfDigit => F[Unit],
      heardFrom: (SocketAddress[IpAddress], Ssrc) => F[Unit],
      audioType: PayloadType,
      eventType: Option[PayloadType],
      comfortType: Option[PayloadType],
      open: Chunk[Byte] => F[Option[Chunk[Byte]]]
  ): Stream[F, JitterPacket] =
    socket.reads
      .evalMapFilter { datagram =>
        open(datagram.bytes).map(_.map(bytes => datagram.copy(bytes = bytes)))
      }
      .flatMap { datagram =>
        RtpCodec.decode(datagram.bytes) match {
          case None                    => Stream.empty
          case Some((header, payload)) =>
            val pt     = header.payloadType
            val packet = JitterPacket(
              header.sequenceNumber,
              header.timestamp,
              header.ssrc,
              header.marker,
              pt,
              payload
            )
            val heard = Stream.exec(heardFrom(datagram.remote, header.ssrc))
            if pt == audioType || comfortType.contains(pt) then
              heard ++ Stream.emit(packet)
            else if eventType.contains(pt) then
              heard ++
                Stream.exec(
                  handleSoftphoneEvent(header, payload, lastDtmfKey, onDtmf)
                ) ++ Stream.emit(packet)
            else Stream.empty
        }
      }

  private def handleSoftphoneEvent[F[_]: Async](
      header: RtpCodec.RtpHeader,
      payload: fs2.Chunk[Byte],
      lastDtmfKey: Ref[F, (Long, Int)],
      onDtmf: DtmfDigit => F[Unit]
  ): F[Unit] =
    SoftphoneEvent.decode(payload) match {
      case Some(event) if event.endBit =>
        val currentDtmfKey = (header.timestamp, event.event)
        lastDtmfKey.getAndSet(currentDtmfKey).flatMap { previousDtmfKey =>
          if previousDtmfKey == currentDtmfKey then Async[F].unit
          else DtmfDigit.fromCode(event.event).traverse_(onDtmf)
        }
      case _ => Async[F].unit
    }
}
