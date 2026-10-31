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
import softphone4s.media.AudioCodecs
import softphone4s.model.{DtmfDigit, MediaDirection, SoftphoneError}
import softphone4s.sdp.{Capabilities, Codec, MediaPlan}
import softphone4s.transport.{RtpLatch, RtpTransport}

private[softphone4s] class MediaSession[F[_]: Async: Network] private (
    bindAddress: IpAddress,
    localRtpPort: Port,
    codecs: AudioCodecs,
    audioChannel: Channel[F, Chunk[Byte]],
    sendPipeReady: Deferred[F, Pipe[F, Byte, Unit]],
    mediaStop: Deferred[F, Unit],
    target: Ref[F, Option[SocketAddress[IpAddress]]],
    agreed: Ref[F, Option[MediaPlan]],
    latching: RtpLatching,
    latch: Ref[F, RtpLatch]
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

  /** Starts media as `plan` says. Fails with
    * [[SoftphoneError.CodecUnavailable]] if the codec it settled on is not one
    * we have, which a plan made from our own capabilities cannot be.
    */
  def start(
      plan: MediaPlan,
      supervisor: Supervisor[F],
      onDtmf: DtmfDigit => F[Unit]
  ): F[Unit] =
    codecs
      .implementing(plan.codec)
      .liftTo[F](SoftphoneError.CodecUnavailable(plan.codec.encoding))
      .flatMap { codec =>
        agreed.set(Some(plan)) >>
          retarget(
            plan.remote.host,
            plan.remote.port,
            MediaDirection.SendRecv
          ) >>
          run(codec, plan, supervisor, onDtmf)
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
                    plan.telephoneEventType
                  )
                  .through(codec.decoder[F])
                  .chunks
                  .evalMap(audioChannel.send(_).void)
                  .compile
                  .drain

              val pcmToRtp: Pipe[F, Byte, Unit] =
                _.through(codec.encoder[F])
                  .through(
                    RtpTransport.sendPipe(
                      socket,
                      target.get,
                      codec.timestampStep,
                      plan.payloadType
                    )
                  )

              sendPipeReady.complete(pcmToRtp) >>
                receiveF.race(mediaStop.get.void).void
            } >> audioChannel.close.void
        }
      )
      .void

  def stop(): F[Unit] = mediaStop.complete(()).void
}

private[softphone4s] object MediaSession {
  def create[F[_]: Async: Network](
      bindAddress: IpAddress,
      localRtpPort: Port,
      codecs: AudioCodecs,
      latching: RtpLatching
  ): F[MediaSession[F]] =
    for {
      audioChannel  <- Channel.bounded[F, Chunk[Byte]](64)
      sendPipeReady <- Deferred[F, Pipe[F, Byte, Unit]]
      mediaStop     <- Deferred[F, Unit]
      target        <- Ref[F].of(Option.empty[SocketAddress[IpAddress]])
      agreed        <- Ref[F].of(Option.empty[MediaPlan])
      latch         <- Ref[F].of(RtpLatch.empty)
    } yield new MediaSession(
      bindAddress,
      localRtpPort,
      codecs,
      audioChannel,
      sendPipeReady,
      mediaStop,
      target,
      agreed,
      latching,
      latch
    )
}
