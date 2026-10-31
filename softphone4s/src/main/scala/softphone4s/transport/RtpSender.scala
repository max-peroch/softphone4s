package softphone4s.transport

import cats.effect.std.Semaphore
import cats.effect.{Async, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.io.net.{Datagram, DatagramSocket}
import fs2.{Chunk, Pipe}
import softphone4s.codec.RtpCodec
import softphone4s.model.{DtmfDigit, PayloadType, SoftphoneEvent}

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
    ssrc: Long,
    sequence: Ref[F, Int],
    clock: Ref[F, Long],
    talking: Ref[F, Boolean],
    sendingDigit: Ref[F, Boolean],
    oneDigitAtATime: Semaphore[F]
) {

  /** Sends each chunk, a payload of 20 ms of audio, as an RTP packet. An empty
    * one, one that comes while a digit is being sent, and any while there is
    * nowhere to send to (a call on hold) are not sent; the first one after such
    * a gap carries the marker bit (RFC 3551 §4.1).
    */
  def audio: Pipe[F, Chunk[Byte], Unit] = _.evalMap(sendAudio)

  private def sendAudio(payload: Chunk[Byte]): F[Unit] =
    for {
      timestamp <- clock.getAndUpdate(_ + timestampStep)
      held      <- sendingDigit.get
      previous  <- talking.get
      to        <- target
      sent      <- to
        .filter(_ => payload.nonEmpty && !held)
        .traverse(transmit(_, audioType, timestamp, !previous, payload))
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
    sequence
      .getAndUpdate(s => (s + 1) & 0xffff)
      .flatMap(number =>
        socket.write(
          Datagram(
            to,
            RtpCodec.encode(
              payloadType = payloadType,
              sequenceNumber = number,
              timestamp = timestamp & 0xffffffffL,
              ssrc = ssrc,
              payload = payload,
              marker = marker
            )
          )
        )
      )

  // Telephone events count at 8 000 a second, and a packet is 20 ms of them.
  private val EventTicksPerMilli = 8
  private val PacketTime         = 20.millis
  private val Interval           = 160
  private val EndRepeats         = 3
  private val Volume             = 10 // -10 dBm0
}

object RtpSender {

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
      timestampStep: Int
  ): F[RtpSender[F]] =
    (
      Async[F].delay(scala.util.Random.nextLong() & 0xffffffffL),
      Ref.of[F, Int](0),
      Ref.of[F, Long](0L),
      Ref.of[F, Boolean](false),
      Ref.of[F, Boolean](false),
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
        _
      )
    )
}
