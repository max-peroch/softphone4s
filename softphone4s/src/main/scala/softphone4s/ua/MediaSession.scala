package softphone4s.ua

import cats.effect.*
import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.concurrent.Channel
import fs2.{Chunk, Pipe, Stream}
import fs2.io.net.Network
import softphone4s.codec.G711Codec
import softphone4s.config.RtpLatching
import softphone4s.model.{DtmfDigit, MediaDirection}
import softphone4s.transport.{RtpLatch, RtpTransport}

private[softphone4s] class MediaSession[F[_]: Async: Network] private (
    pcmFrameBytes: Int,
    bindAddress: IpAddress,
    localRtpPort: Port,
    audioChannel: Channel[F, Chunk[Byte]],
    sendPipeReady: Deferred[F, Pipe[F, Byte, Unit]],
    mediaStop: Deferred[F, Unit],
    target: Ref[F, Option[SocketAddress[IpAddress]]],
    latching: RtpLatching,
    latch: Ref[F, RtpLatch]
) {

  val source: Stream[F, Byte] =
    audioChannel.stream.flatMap(Stream.chunk)

  val sink: Pipe[F, Byte, Unit] = upstream =>
    Stream.eval(sendPipeReady.get).flatMap { pipe =>
      upstream.through(pipe).interruptWhen(mediaStop.get.attempt)
    }

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

  def start(
      remoteIp: IpAddress,
      remotePort: Port,
      supervisor: Supervisor[F],
      onDtmf: DtmfDigit => F[Unit]
  ): F[Unit] =
    retarget(remoteIp, remotePort, MediaDirection.SendRecv) >>
      run(supervisor, onDtmf)

  private def run(
      supervisor: Supervisor[F],
      onDtmf: DtmfDigit => F[Unit]
  ): F[Unit] = {
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
                  .receiveBytes(socket, lastDtmfKey, onDtmf, heardFrom)
                  .through(G711Codec.mulawToPcmPipe)
                  .chunks
                  .evalMap(audioChannel.send(_).void)
                  .compile
                  .drain

              val pcmToRtp: Pipe[F, Byte, Unit] =
                _.through(G711Codec.pcmToMulawPipe(pcmFrameBytes))
                  .through(RtpTransport.sendPipe(socket, target.get))

              sendPipeReady.complete(pcmToRtp) >>
                receiveF.race(mediaStop.get.void).void
            } >> audioChannel.close.void
        }
      )
      .void
  }

  def stop(): F[Unit] = mediaStop.complete(()).void
}

private[softphone4s] object MediaSession {
  def create[F[_]: Async: Network](
      pcmFrameBytes: Int,
      bindAddress: IpAddress,
      localRtpPort: Port,
      latching: RtpLatching
  ): F[MediaSession[F]] =
    for {
      audioChannel  <- Channel.bounded[F, Chunk[Byte]](64)
      sendPipeReady <- Deferred[F, Pipe[F, Byte, Unit]]
      mediaStop     <- Deferred[F, Unit]
      target        <- Ref[F].of(Option.empty[SocketAddress[IpAddress]])
      latch         <- Ref[F].of(RtpLatch.empty)
    } yield new MediaSession(
      pcmFrameBytes,
      bindAddress,
      localRtpPort,
      audioChannel,
      sendPipeReady,
      mediaStop,
      target,
      latching,
      latch
    )
}
