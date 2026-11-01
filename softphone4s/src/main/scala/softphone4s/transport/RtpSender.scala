package softphone4s.transport

import cats.effect.std.Semaphore
import cats.effect.{Async, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.io.net.{Datagram, DatagramSocket}
import fs2.{Chunk, Pipe}
import softphone4s.codec.RtpCodec
import softphone4s.media.RtcpPacket.SenderInfo
import softphone4s.media.{NtpTime, RtpReception}
import softphone4s.srtp.SrtpOutbound
import softphone4s.model.{DtmfDigit, PayloadType, SoftphoneEvent, Ssrc}

import scala.concurrent.duration.*

/** The sending end of one RTP stream: audio and telephone events (RFC 4733) go
  * out under one SSRC and one run of sequence numbers, as the receiver expects
  * of a stream, and while a digit is being sent the audio is held back (§2.5).
  *
  * The audio carries the clock: a frame stands for 20 ms whether or not it is
  * sent, so a frame that is dropped, held or replaced by a digit leaves a gap
  * in the timestamps and not in the sequence.
  */
final class RtpSender[F[_]: Async] private (
    socket: DatagramSocket[F],
    target: F[Option[SocketAddress[IpAddress]]],
    audioType: PayloadType,
    eventType: Option[PayloadType],
    timestampStep: Int,
    ssrc: Ssrc,
    sequence: Ref[F, Int],
    clock: Ref[F, Long],
    talking: Ref[F, Boolean],
    sendingDigit: Ref[F, Boolean],
    oneDigitAtATime: Semaphore[F],
    counts: Ref[F, RtpSender.Sent],
    anchor: Ref[F, Option[RtpSender.Anchor]],
    srtp: Ref[F, Option[SrtpOutbound]],
    inOrder: Semaphore[F]
) {

  /** The source these packets are sent as. */
  def source: Ssrc = ssrc

  /** What a sender report says of this stream now: when it is, on the wall
    * clock and on the stream's, and how much has been sent. None before
    * anything has been, when there is nothing to report as a sender.
    */
  def senderInfo: F[Option[SenderInfo]] =
    (Async[F].realTime, Async[F].monotonic, counts.get, anchor.get).mapN {
      (wall, now, sent, last) =>
        last.filter(_ => sent.packets > 0).map { frame =>
          val elapsed = RtpReception.ticks(now - frame.at, timestampStep * 50)
          SenderInfo(
            NtpTime.fromEpoch(wall),
            (frame.timestamp + elapsed) & 0xffffffffL,
            sent.packets & 0xffffffffL,
            sent.octets & 0xffffffffL
          )
        }
    }

  /** Sends each chunk, a payload of 20 ms of audio, as an RTP packet. An empty
    * one, one that comes while a digit is being sent, and any while there is
    * nowhere to send to (a call on hold) are not sent; the first one after such
    * a gap carries the marker bit (RFC 3551 §4.1).
    */
  def audio: Pipe[F, Chunk[Byte], Unit] = _.evalMap(sendAudio)

  private def sendAudio(payload: Chunk[Byte]): F[Unit] =
    for {
      timestamp <- clock.getAndUpdate(_ + timestampStep)
      now       <- Async[F].monotonic
      _         <- anchor.set(Some(RtpSender.Anchor(timestamp, now)))
      previous  <- talking.get
      to        <- target
      // Whether a digit is being sent is asked in the same step as the sending,
      // so that no audio gets in between the packets of a digit by having asked
      // a moment before.
      sent <- inOrder.permit.surround(
        sendingDigit.get.flatMap(held =>
          to
            .filter(_ => payload.nonEmpty && !held)
            .traverse(
              transmitNow(_, audioType, timestamp, !previous, payload)
            )
        )
      )
      _ <- talking.set(sent.isDefined)
    } yield ()

  /** Sends `digit` as telephone events lasting `duration`: a packet every 20 ms
    * with the time so far, the first marked, then the last three times over
    * with the end bit, so that losing one does not lose the digit. All of them
    * carry the timestamp the digit began at. False if there is no one to send
    * it to, or telephone events were not agreed; one digit is sent at a time.
    */
  def telephoneEvent(digit: DtmfDigit, duration: FiniteDuration): F[Boolean] =
    eventType.fold(Async[F].pure(false))(payloadType =>
      oneDigitAtATime.permit.surround(
        target.flatMap {
          case None    => Async[F].pure(false)
          case Some(_) =>
            Resource
              .make(sendingDigit.set(true))(_ => sendingDigit.set(false))
              .surround(
                clock.get.flatMap(start =>
                  sendDigit(payloadType, start, digit, duration)
                )
              )
              .as(true)
        }
      )
    )

  private def sendDigit(
      payloadType: PayloadType,
      start: Long,
      digit: DtmfDigit,
      duration: FiniteDuration
  ): F[Unit] = {
    val total = (duration.toMillis * EventTicksPerMilli).toInt
    val steps = math.max(1, (total + Interval - 1) / Interval)

    def packet(elapsed: Int, end: Boolean, marker: Boolean): F[Unit] =
      target.flatMap(
        _.traverse_(
          transmit(
            _,
            payloadType,
            start,
            marker,
            SoftphoneEvent.encode(
              SoftphoneEvent(digit.code, end, Volume, elapsed)
            )
          )
        )
      )

    val running = (1 until steps).toList.traverse_(k =>
      packet(k * Interval, end = false, marker = k == 1) >>
        Async[F].sleep(PacketTime)
    )
    val ending = (0 until EndRepeats).toList.traverse_(i =>
      packet(total, end = true, marker = steps == 1 && i == 0) >>
        Async[F].sleep(PacketTime).whenA(i < EndRepeats - 1)
    )
    running >> ending
  }

  private def transmit(
      to: SocketAddress[IpAddress],
      payloadType: PayloadType,
      timestamp: Long,
      marker: Boolean,
      payload: Chunk[Byte]
  ): F[Unit] =
    inOrder.permit.surround(
      transmitNow(to, payloadType, timestamp, marker, payload)
    )

  /** The number, the encryption (whose rollover counter goes up as the number
    * wraps) and the sending are one step, so that packets are protected in the
    * order they are numbered, whichever of audio and digits sends them. The
    * caller holds `inOrder`.
    */
  private def transmitNow(
      to: SocketAddress[IpAddress],
      payloadType: PayloadType,
      timestamp: Long,
      marker: Boolean,
      payload: Chunk[Byte]
  ): F[Unit] =
    sequence
      .getAndUpdate(s => (s + 1) & 0xffff)
      .flatMap(number =>
        secured(
          RtpCodec.encode(
            payloadType = payloadType,
            sequenceNumber = number,
            timestamp = timestamp & 0xffffffffL,
            ssrc = ssrc,
            payload = payload,
            marker = marker
          )
        ).flatMap(
          _.traverse_(bytes =>
            socket.write(Datagram(to, bytes)) >>
              counts.update(_.add(payload.size))
          )
        )
      )

  /** `packet` as it goes on the wire: encrypted, if the stream is. None if it
    * could not be, which an RTP packet of ours never cannot.
    */
  private def secured(packet: Chunk[Byte]): F[Option[Chunk[Byte]]] =
    srtp.modify {
      case None      => (None, Some(packet))
      case Some(out) =>
        out.protect(packet) match {
          case Right((next, protectedPacket)) =>
            (Some(next), Some(protectedPacket))
          case Left(_) => (Some(out), None)
        }
    }

  // Telephone events count at 8 000 a second, and a packet is 20 ms of them.
  private val EventTicksPerMilli = 8
  private val PacketTime         = 20.millis
  private val Interval           = 160
  private val EndRepeats         = 3
  private val Volume             = 10 // -10 dBm0
}

object RtpSender {

  /** What has been sent: RTP packets, and the bytes of their payloads. */
  private[transport] final case class Sent(packets: Long, octets: Long) {
    def add(payloadBytes: Int): Sent = Sent(packets + 1, octets + payloadBytes)
  }

  /** The timestamp of an audio frame and when it was made, from which the
    * stream's clock is read at any later time.
    */
  private[transport] final case class Anchor(
      timestamp: Long,
      at: FiniteDuration
  )

  /** A sender for a stream of `audioType`, with telephone events on `eventType`
    * if they were agreed, writing to `socket` whatever `target` says at the
    * time: it changes when the remote party moves its media, and is `None`
    * while the call is on hold.
    */
  def make[F[_]: Async](
      socket: DatagramSocket[F],
      target: F[Option[SocketAddress[IpAddress]]],
      audioType: PayloadType,
      eventType: Option[PayloadType],
      timestampStep: Int,
      srtp: Option[SrtpOutbound] = None
  ): F[RtpSender[F]] =
    (
      Async[F].delay(Ssrc.fromBits(scala.util.Random.nextInt())),
      Ref.of[F, Int](0),
      Ref.of[F, Long](0L),
      Ref.of[F, Boolean](false),
      Ref.of[F, Boolean](false),
      Semaphore[F](1),
      Ref.of[F, Sent](Sent(0, 0)),
      Ref.of[F, Option[Anchor]](None),
      Ref.of[F, Option[SrtpOutbound]](srtp),
      Semaphore[F](1)
    ).mapN(
      new RtpSender(
        socket,
        target,
        audioType,
        eventType,
        timestampStep,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _
      )
    )
}
