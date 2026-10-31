package softphone4s.transport

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.DatagramSocket
import fs2.{Chunk, Stream}
import softphone4s.codec.RtpCodec
import softphone4s.model.{DtmfDigit, PayloadType, SoftphoneEvent}

object RtpTransport {

  /** The payloads of the audio packets arriving on `socket`, a chunk for each
    * packet: those of payload type `audioType`, and the telephone events (RFC
    * 4733) on `eventType` and the comfort noise (RFC 3389) on `comfortType`, if
    * there are any. `heardFrom` is told where each of them came from, and its
    * SSRC; `onNoise` is given the payload of each comfort noise packet, which
    * makes no audio of its own.
    *
    * DTMF end-bit packets are deduped by (timestamp, event) and delivered via
    * `onDtmf`; they produce no payload in the returned stream.
    */
  def receiveBytes[F[_]: Async](
      socket: DatagramSocket[F],
      lastDtmfKey: Ref[F, (Long, Int)],
      onDtmf: DtmfDigit => F[Unit],
      heardFrom: (SocketAddress[IpAddress], Long) => F[Unit],
      audioType: PayloadType,
      eventType: Option[PayloadType],
      comfortType: Option[PayloadType],
      onNoise: Chunk[Byte] => F[Unit]
  ): Stream[F, Chunk[Byte]] =
    socket.reads.flatMap { datagram =>
      RtpCodec.decode(datagram.bytes) match {
        case None                    => Stream.empty
        case Some((header, payload)) =>
          val pt = header.payloadType
          if pt == audioType then
            Stream.exec(heardFrom(datagram.remote, header.ssrc)) ++
              Stream.emit(payload)
          else if eventType.contains(pt) then
            Stream.exec(heardFrom(datagram.remote, header.ssrc)) ++
              Stream
                .eval(
                  handleSoftphoneEvent(header, payload, lastDtmfKey, onDtmf)
                )
                .drain
          else if comfortType.contains(pt) then
            Stream.exec(heardFrom(datagram.remote, header.ssrc)) ++
              Stream.exec(onNoise(payload))
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
