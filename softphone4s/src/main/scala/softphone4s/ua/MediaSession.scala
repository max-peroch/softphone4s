package softphone4s.ua

import cats.effect.*
import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.concurrent.Channel
import fs2.{Chunk, Pipe, Stream}
import fs2.io.net.Network
import softphone4s.config.RtpLatching
import softphone4s.media.{AudioCodecs, ComfortNoise}
import softphone4s.model.{DtmfDigit, MediaDirection, SoftphoneError}
import softphone4s.sdp.{Capabilities, Codec, MediaPlan}

import scala.concurrent.duration.*
import softphone4s.transport.{RtpLatch, RtpSender, RtpTransport}

private[softphone4s] class MediaSession[F[_]: Async: Network] private (
    bindAddress: IpAddress,
    localRtpPort: Port,
    codecs: AudioCodecs,
    audioChannel: Channel[F, Chunk[Byte]],
    sendPipeReady: Deferred[F, Pipe[F, Byte, Unit]],
    sender: Deferred[F, RtpSender[F]],
    mediaStop: Deferred[F, Unit],
    target: Ref[F, Option[SocketAddress[IpAddress]]],
    agreed: Ref[F, Option[MediaPlan]],
    latching: RtpLatching,
    latch: Ref[F, RtpLatch],
    comfort: Ref[F, Option[ComfortNoise.Noise]],
    remoteSends: Ref[F, Boolean]
) {

  val source: Stream[F, Byte] =
    audioChannel.stream.flatMap(Stream.chunk)

  val sink: Pipe[F, Byte, Unit] = upstream =>
    Stream.eval(sendPipeReady.get).flatMap { pipe =>
      upstream.through(pipe).interruptWhen(mediaStop.get.attempt)
    }

  /** The codec media is running in, once it has started. A change of session
    * keeps to it.
    */
  def codecInUse: F[Option[Codec]] = agreed.get.map(_.map(_.codec))

  /** `base` as a change of session is to see it: once media is running, only
    * the codec in use, since switching codec in mid-call is not supported and a
    * session change without it is refused.
    */
  def restrict(base: Capabilities): F[Capabilities] =
    agreed.get.map(_.fold(base)(plan => base.only(plan.codec)))

  /** Points outgoing media at the remote party's (possibly new) address, or
    * stops it while `direction` says they will not receive (call on hold).
    */
  def retarget(
      remoteIp: IpAddress,
      remotePort: Port,
      direction: MediaDirection
  ): F[Unit] =
    latch.set(RtpLatch.empty) >>
      remoteSends.set(direction.sends) >>
      // A party that sends nothing makes no noise either.
      comfort.set(None).whenA(!direction.sends) >>
      target.set(
        Option.when(direction.receives)(SocketAddress(remoteIp, remotePort))
      )

  /** Media arrived from `source`: if it keeps coming from somewhere other than
    * the target, send there (see [[RtpLatch]]). The target is changed only if
    * it is still the one the decision was made against.
    */
  private def heardFrom(source: SocketAddress[IpAddress], ssrc: Long): F[Unit] =
    Async[F].whenA(latching == RtpLatching.Follow)(
      target.get.flatMap { current =>
        latch
          .modify(RtpLatch.hear(_, current, source, ssrc))
          .flatMap(
            _.traverse_(moved =>
              target.update(t => if t == current then Some(moved) else t)
            )
          )
      }
    )

  /** News of the other side's background noise: its level (RFC 3389), from now
    * on until speech comes or it is stale.
    */
  private def noiseHeard(payload: Chunk[Byte]): F[Unit] =
    ComfortNoise.Level
      .parse(payload)
      .traverse_(level =>
        Async[F].monotonic.flatMap(now =>
          comfort.update(before =>
            Some(ComfortNoise.Noise.heard(level, now, before))
          )
        )
      )

  /** A frame of noise of `samples` samples to play now, if some is wanted: the
    * other side told us the level, has not spoken since, is still sending and
    * the news is not stale.
    */
  private def playNoise(samples: Int): F[Unit] =
    (Async[F].monotonic, remoteSends.get).flatMapN { (now, sends) =>
      comfort
        .modify {
          case Some(noise) if sends && noise.isCurrent(now) =>
            val (next, frame) = noise.frame(samples)
            (Some(next), Some(frame))
          case _ => (None, None)
        }
        .flatMap(_.traverse_(audioChannel.send(_).void))
    }

  /** Starts media as `plan` says, sending only if the other side will receive.
    * Fails with [[SoftphoneError.CodecUnavailable]] if the codec it settled on
    * is not one we have, which a plan made from our own capabilities cannot be.
    *
    * Media already running, because the call began with early media, is not
    * started again: it is pointed where the new plan says, and the answer is
    * whether the codec stayed the same. If it did not, the early one is kept.
    */
  def start(
      plan: MediaPlan,
      supervisor: Supervisor[F],
      onDtmf: DtmfDigit => F[Unit]
  ): F[MediaStart] =
    agreed.get.flatMap {
      case Some(running) =>
        agreed.set(
          Some(
            running.copy(
              remote = plan.remote,
              remoteDirection = plan.remoteDirection
            )
          )
        ) >>
          retarget(plan.remote.host, plan.remote.port, plan.remoteDirection)
            .as(
              if running.codec == plan.codec && running.payloadType == plan.payloadType
              then MediaStart.Followed
              else MediaStart.CodecChanged
            )
      case None =>
        codecs
          .implementing(plan.codec)
          .liftTo[F](SoftphoneError.CodecUnavailable(plan.codec.encoding))
          .flatMap { codec =>
            agreed.set(Some(plan)) >>
              retarget(
                plan.remote.host,
                plan.remote.port,
                plan.remoteDirection
              ) >>
              run(codec, plan, supervisor, onDtmf).as(MediaStart.Started)
          }
    }

  private def run(
      codec: softphone4s.media.AudioCodec,
      plan: MediaPlan,
      supervisor: Supervisor[F],
      onDtmf: DtmfDigit => F[Unit]
  ): F[Unit] =
    supervisor
      .supervise(
        Ref[F].of((-1L, -1)).flatMap { lastDtmfKey =>
          Network[F]
            .bindDatagramSocket(address =
              SocketAddress(bindAddress, localRtpPort)
            )
            .use { socket =>
              val receiveF =
                RtpTransport
                  .receiveBytes(
                    socket,
                    lastDtmfKey,
                    onDtmf,
                    heardFrom,
                    plan.payloadType,
                    plan.telephoneEventType,
                    plan.comfortNoiseType,
                    noiseHeard
                  )
                  // Speech again: the noise that stood in for it ends.
                  .evalTap(_ => comfort.set(None))
                  .through(codec.decoder[F])
                  .chunks
                  .evalMap(audioChannel.send(_).void)
                  .compile
                  .drain

              // While the other side is quiet and has told us how, a frame of
              // noise for every 20 ms.
              val noiseF =
                Stream
                  .awakeEvery[F](MediaSession.FramePeriod)
                  .evalMap(_ => playNoise(codec.pcmPacketBytes / 2))
                  .compile
                  .drain

              RtpSender
                .make(
                  socket,
                  target.get,
                  plan.payloadType,
                  plan.telephoneEventType,
                  codec.timestampStep
                )
                .flatMap { rtp =>
                  val pcmToRtp: Pipe[F, Byte, Unit] =
                    _.through(codec.encoder[F]).through(rtp.audio)
                  sender.complete(rtp) >>
                    sendPipeReady.complete(pcmToRtp) >>
                    receiveF.both(noiseF).void.race(mediaStop.get.void).void
                }
            } >> audioChannel.close.void
        }
      )
      .void

  /** Sends `digit` in the media stream as telephone events (RFC 4733), taking
    * `duration`. False, and nothing sent, if media is not running, the other
    * side did not agree to receive them, or the call is on hold.
    */
  def sendDtmf(digit: DtmfDigit, duration: FiniteDuration): F[Boolean] =
    sender.tryGet.flatMap(
      _.fold(Async[F].pure(false))(_.telephoneEvent(digit, duration))
    )

  def stop(): F[Unit] = mediaStop.complete(()).void
}

private[softphone4s] object MediaSession {

  /** The time one frame of audio stands for. */
  private val FramePeriod: FiniteDuration = 20.millis

  def create[F[_]: Async: Network](
      bindAddress: IpAddress,
      localRtpPort: Port,
      codecs: AudioCodecs,
      latching: RtpLatching
  ): F[MediaSession[F]] =
    for {
      audioChannel  <- Channel.bounded[F, Chunk[Byte]](64)
      sendPipeReady <- Deferred[F, Pipe[F, Byte, Unit]]
      sender        <- Deferred[F, RtpSender[F]]
      mediaStop     <- Deferred[F, Unit]
      target        <- Ref[F].of(Option.empty[SocketAddress[IpAddress]])
      agreed        <- Ref[F].of(Option.empty[MediaPlan])
      latch         <- Ref[F].of(RtpLatch.empty)
      comfort       <- Ref[F].of(Option.empty[ComfortNoise.Noise])
      remoteSends   <- Ref[F].of(true)
    } yield new MediaSession(
      bindAddress,
      localRtpPort,
      codecs,
      audioChannel,
      sendPipeReady,
      sender,
      mediaStop,
      target,
      agreed,
      latching,
      latch,
      comfort,
      remoteSends
    )
}
