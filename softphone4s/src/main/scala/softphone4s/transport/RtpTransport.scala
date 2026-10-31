package softphone4s.transport

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{Datagram, DatagramSocket}
import fs2.{Chunk, Pipe, Stream}
import softphone4s.codec.RtpCodec
import softphone4s.model.{DtmfDigit, SoftphoneEvent}

object RtpTransport {

  /** The payloads of the audio packets arriving on `socket`, a chunk for each
    * packet: those of payload type `audioType`, and the telephone events (RFC
    * 4733) on `eventType`, if there are any. `heardFrom` is told where each of
    * them came from, and its SSRC.
    *
    * DTMF end-bit packets are deduped by (timestamp, event) and delivered via
    * `onDtmf`; they produce no payload in the returned stream.
    */
  def receiveBytes[F[_]: Async](
      socket: DatagramSocket[F],
      lastDtmfKey: Ref[F, (Long, Int)],
      onDtmf: DtmfDigit => F[Unit],
      heardFrom: (SocketAddress[IpAddress], Long) => F[Unit],
      audioType: Int,
      eventType: Option[Int]
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
          else Stream.empty
      }
    }

  /** Packs payloads into RTP packets of payload type `payloadType`, one payload
    * to a packet, each `timestampStep` after the one before, and writes them to
    * `socket`, to wherever `target` says at the time: it changes when the
    * remote party moves its media, and is `None` while the call is on hold,
    * when packets are dropped (their sequence numbers are skipped, as for
    * silence).
    */
  def sendPipe[F[_]: Async](
      socket: DatagramSocket[F],
      target: F[Option[SocketAddress[IpAddress]]],
      timestampStep: Int,
      payloadType: Int
  ): Pipe[F, Chunk[Byte], Unit] = upstream =>
    Stream
      .eval(Async[F].delay(scala.util.Random.nextLong() & 0xffffffffL))
      .flatMap { ssrc =>
        upstream.zipWithIndex
          .evalMap { case (chunk, packetIndex) =>
            target.map(_.map { remoteAddr =>
              Datagram(
                remoteAddr,
                RtpCodec.encode(
                  payloadType = payloadType,
                  sequenceNumber = (packetIndex & 0xffff).toInt,
                  timestamp = (packetIndex * timestampStep) & 0xffffffffL,
                  ssrc = ssrc,
                  payload = chunk,
                  marker = packetIndex == 0
                )
              )
            })
          }
          .evalMap(_.traverse_(socket.write))
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
