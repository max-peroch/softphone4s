package softphone4s.transport

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{Datagram, DatagramSocket}
import fs2.{Pipe, Stream}
import softphone4s.codec.RtpCodec
import softphone4s.model.{DtmfDigit, SoftphoneEvent}

object RtpTransport {

  private val PayloadTypePcmu           = 0   // G.711 μ-law (RFC 3551)
  private val PayloadTypeTelephoneEvent = 101 // RFC 4733 DTMF telephone-event

  /** Number of PCMU samples per 20ms packet at 8kHz. */
  val PacketSamples = 160

  /** Decoded PCMU bytes from inbound RTP packets on `socket`.
    *
    * DTMF RFC 4733 end-bit packets are deduped by (timestamp, event) and
    * delivered via `onDtmf`; they produce no bytes in the returned stream.
    */
  def receiveBytes[F[_]: Async](
      socket: DatagramSocket[F],
      lastDtmfKey: Ref[F, (Long, Int)],
      onDtmf: DtmfDigit => F[Unit]
  ): Stream[F, Byte] =
    socket.reads.flatMap { datagram =>
      RtpCodec.decode(datagram.bytes) match {
        case None                    => Stream.empty
        case Some((header, payload)) =>
          header.payloadType match {
            case PayloadTypePcmu =>
              Stream.chunk(payload)
            case PayloadTypeTelephoneEvent =>
              Stream
                .eval(
                  handleSoftphoneEvent(header, payload, lastDtmfKey, onDtmf)
                )
                .drain
            case _ => Stream.empty
          }
      }
    }

  /** Encodes incoming PCMU bytes as RTP and writes them to `socket`. */
  def sendPipe[F[_]: Async](
      socket: DatagramSocket[F],
      remoteAddr: SocketAddress[IpAddress]
  ): Pipe[F, Byte, Unit] = upstream =>
    Stream
      .eval(Async[F].delay(scala.util.Random.nextLong() & 0xffffffffL))
      .flatMap { ssrc =>
        upstream
          .chunkN(PacketSamples, allowFewer = false)
          .zipWithIndex
          .map { case (chunk, packetIndex) =>
            Datagram(
              remoteAddr,
              RtpCodec.encode(
                payloadType = PayloadTypePcmu,
                sequenceNumber = (packetIndex & 0xffff).toInt,
                timestamp = (packetIndex * PacketSamples) & 0xffffffffL,
                ssrc = ssrc,
                payload = chunk,
                marker = packetIndex == 0
              )
            )
          }
          .evalMap(socket.write)
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
