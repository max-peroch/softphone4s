package softphone4s.ua

import cats.effect.*
import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.concurrent.Channel
import fs2.{Chunk, Pipe, Stream}
import fs2.io.net.{Datagram, DatagramSocket, Network}
import softphone4s.codec.RtcpCodec
import softphone4s.config.RtpLatching
import softphone4s.media.{
  AudioCodec,
  AudioCodecs,
  ComfortNoise,
  Concealment,
  JitterConfig,
  JitterPacket,
  JitterPlayout,
  JitterStats,
  Playout,
  RtcpPacket,
  RtcpReports,
  RtcpSchedule,
  RtpReception
}
import softphone4s.media.RtcpPacket.ReportBlock
import softphone4s.model.{DtmfDigit, MediaDirection, SoftphoneError, Ssrc}
import softphone4s.sdp.{Capabilities, Codec, MediaPlan}
import softphone4s.srtp.{
  SrtcpInbound,
  SrtcpOutbound,
  SrtpError,
  SrtpInbound,
  SrtpOutbound,
  SrtpPlan
}

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
    playback: Ref[F, MediaSession.Playback],
    remoteSends: Ref[F, Boolean],
    jitterConfig: JitterConfig,
    stats: Ref[F, JitterStats],
    receptions: Ref[F, Map[Ssrc, RtpReception]],
    peerBlock: Ref[F, Option[ReportBlock]],
    rtcp: RtcpSchedule
) {
  import MediaSession.RtcpGuard

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
      playback.update(_.copy(noise = None)).whenA(!direction.sends) >>
      target.set(
        Option.when(direction.receives)(SocketAddress(remoteIp, remotePort))
      )

  /** Media arrived from `source`: if it keeps coming from somewhere other than
    * the target, send there (see [[RtpLatch]]). The target is changed only if
    * it is still the one the decision was made against.
    */
  private def heardFrom(source: SocketAddress[IpAddress], ssrc: Ssrc): F[Unit] =
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

  /** What the other side last reported of the stream we send: how much of it
    * they received, lost, and how much it jittered. None until they have.
    */
  def peerReport: F[Option[ReportBlock]] = peerBlock.get

  /** A packet of the stream arrived: counted as it came, before the jitter
    * buffer puts anything in order. A new source replaces the one before, since
    * a call has two parties.
    */
  private def observe(clockRate: Int)(packet: JitterPacket): F[Unit] =
    Async[F].monotonic.flatMap(now =>
      receptions.update(known =>
        known.get(packet.ssrc) match {
          case Some(seen) =>
            known.updated(
              packet.ssrc,
              seen.hear(packet.sequence, packet.timestamp, now)
            )
          case None =>
            Map(
              packet.ssrc -> RtpReception.start(
                packet.ssrc,
                clockRate,
                packet.sequence,
                packet.timestamp,
                now
              )
            )
        }
      )
    )

  /** The RTCP port of the party we send media to: the one after its RTP port.
    */
  private def rtcpTarget: F[Option[SocketAddress[IpAddress]]] =
    target.get.map(
      _.flatMap(media =>
        Port.fromInt(media.port.value + 1).map(SocketAddress(media.host, _))
      )
    )

  /** The socket RTCP uses, the port after the RTP one. It is optional, so a
    * port that is taken, or none after 65535, leaves the call without it.
    */
  private def rtcpSocket: Resource[F, Option[DatagramSocket[F]]] =
    Port
      .fromInt(localRtpPort.value + 1)
      .fold(Resource.pure[F, Option[DatagramSocket[F]]](None))(port =>
        Network[F]
          .bindDatagramSocket(address = SocketAddress(bindAddress, port))
          .attempt
          .map(_.toOption)
      )

  /** Sends a report to the party we send media to, if it is receiving: a
    * sender's if we have sent anything, with a block for what we heard. With
    * `leaving` a goodbye follows, and nothing is sent if there is nothing to
    * say goodbye to. Failing to send one is not an error: it is the next
    * report's to make up for.
    */
  private def report(
      socket: DatagramSocket[F],
      rtp: RtpSender[F],
      guard: Option[RtcpGuard[F]],
      leaving: Boolean
  ): F[Unit] =
    (Async[F].monotonic, rtp.senderInfo, rtcpTarget).flatMapN {
      (now, info, to) =>
        to.traverse_ { address =>
          receptions
            .modify { known =>
              val reported =
                known.toList.map((ssrc, seen) => ssrc -> seen.report(now))
              (
                reported.map((ssrc, step) => ssrc -> step._1).toMap,
                reported.map(_._2._2)
              )
            }
            .flatMap { blocks =>
              val name    = s"softphone4s@$bindAddress"
              val packets =
                if leaving then
                  RtcpReports.goodbye(rtp.source, info, blocks, name)
                else RtcpReports.regular(rtp.source, info, blocks, name)
              onTheWire(guard, RtcpCodec.encode(packets))
                .flatMap(
                  _.traverse_(bytes => socket.write(Datagram(address, bytes)))
                )
                .attempt
                .void
                .whenA(!leaving || info.isDefined || blocks.nonEmpty)
            }
        }
    }

  /** What a report from the other side tells us. */
  private def hear(ours: Ssrc, packet: RtcpPacket): F[Unit] = {
    def aboutUs(blocks: List[ReportBlock]): F[Unit] =
      blocks.find(_.ssrc == ours).traverse_(block => peerBlock.set(Some(block)))
    packet match {
      case RtcpPacket.SenderReport(ssrc, info, blocks) =>
        // Their report is what our blocks about them quote back.
        Async[F].monotonic.flatMap(now =>
          receptions.update(known =>
            known
              .get(ssrc)
              .fold(known)(seen =>
                known.updated(ssrc, seen.senderReportHeard(info.middle, now))
              )
          )
        ) >> aboutUs(blocks)
      case RtcpPacket.ReceiverReport(_, blocks) => aboutUs(blocks)
      case _                                    => Async[F].unit
    }
  }

  /** Reports every so often, and hears the other side's, until cancelled. Only
    * what comes from the host we send media to is believed.
    */
  private def reports(
      socket: DatagramSocket[F],
      rtp: RtpSender[F],
      guard: Option[RtcpGuard[F]]
  ): F[Unit] = {
    val wait: (Double => FiniteDuration) => F[Unit] = pick =>
      Async[F]
        .delay(scala.util.Random.nextDouble())
        .flatMap(r => Async[F].sleep(pick(r)))
    val sending =
      wait(rtcp.first) >> report(socket, rtp, guard, leaving = false) >>
        (wait(rtcp.next) >> report(socket, rtp, guard, leaving = false))
          .foreverM[Unit]
    val receiving =
      socket.reads
        .evalMap(datagram =>
          target.get.flatMap(media =>
            fromTheWire(guard, datagram.bytes)
              .map(
                _.flatMap(RtcpCodec.decode(_).toOption)
                  .filter(_ => media.exists(_.host == datagram.remote.host))
              )
              .flatMap(_.traverse_(_.traverse_(hear(rtp.source, _))))
          )
        )
        .compile
        .drain
    // A side channel: whatever goes wrong with it does not end the call.
    sending.both(receiving).attempt.void
  }

  /** `rtcp` as it goes on the wire: encrypted, if the call is. None if it could
    * not be, which an RTCP packet of ours never cannot.
    */
  private def onTheWire(
      guard: Option[RtcpGuard[F]],
      rtcp: Chunk[Byte]
  ): F[Option[Chunk[Byte]]] =
    guard.fold(Async[F].pure(Option(rtcp)))(g =>
      g.out.modify(out =>
        out.protect(rtcp) match {
          case Right((next, bytes)) => (next, Some(bytes))
          case Left(_)              => (out, None)
        }
      )
    )

  /** Takes SRTCP off what came: the RTCP in it, or nothing if it does not
    * authenticate, or has been seen. As for media, a packet from another source
    * starts a new count if it is authentic with the key we were given.
    */
  private def fromTheWire(
      guard: Option[RtcpGuard[F]],
      bytes: Chunk[Byte]
  ): F[Option[Chunk[Byte]]] =
    guard.fold(Async[F].pure(Option(bytes)))(g =>
      g.in.modify { current =>
        current.unprotect(bytes) match {
          case Right((next, rtcp))              => (next, Some(rtcp))
          case Left(SrtpError.SourceChanged(_)) =>
            SrtcpInbound(g.keys.receive).unprotect(bytes) match {
              case Right((next, rtcp)) => (next, Some(rtcp))
              case Left(_)             => (current, None)
            }
          case Left(_) => (current, None)
        }
      }
    )

  /** Media as it came, when it is not encrypted. */
  private val plain: Chunk[Byte] => F[Option[Chunk[Byte]]] =
    bytes => Async[F].pure(Some(bytes))

  /** Takes SRTP off what came: the packet in it, or nothing if it does not
    * authenticate, or has been seen, or is too old. A packet from another
    * source than the one counted is the other side's new stream, and starts a
    * new count, if the packet is authentic with the key we were given.
    */
  private def opener(
      keys: SrtpPlan,
      state: Ref[F, SrtpInbound]
  ): Chunk[Byte] => F[Option[Chunk[Byte]]] =
    bytes =>
      state.modify { current =>
        current.unprotect(bytes) match {
          case Right((next, packet))            => (next, Some(packet))
          case Left(SrtpError.SourceChanged(_)) =>
            SrtpInbound(keys.suite, keys.receive).unprotect(bytes) match {
              case Right((next, packet)) => (next, Some(packet))
              case Left(_)               => (current, None)
            }
          case Left(_) => (current, None)
        }
      }

  /** What to play for one thing the jitter buffer gave. */
  private def play(
      plan: MediaPlan,
      decode: AudioCodec.Session,
      frameBytes: Int,
      out: Playout
  ): F[Unit] =
    out match {
      case Playout.Frame(packet) if packet.payloadType == plan.payloadType =>
        val pcm = Chunk.array(decode(packet.payload.toArray))
        // Speech again: the noise that stood in for it ends.
        playback.update(_.heard(pcm)) >>
          audioChannel.send(pcm).void.whenA(pcm.nonEmpty)
      case Playout.Frame(packet)
          if plan.comfortNoiseType.contains(packet.payloadType) =>
        noiseHeard(packet.payload) >> fill(frameBytes, lost = false)
      // A telephone event: its digit was acted on when it arrived.
      case Playout.Frame(_) => fill(frameBytes, lost = false)
      case Playout.Lost     => fill(frameBytes, lost = true)
      case Playout.Waiting  => fill(frameBytes, lost = false)
    }

  /** How the jitter buffer stands: what it has received, played, concealed and
    * dropped.
    */
  def jitterStats: F[JitterStats] = stats.get

  /** News of the other side's background noise: its level (RFC 3389), from now
    * on until speech comes or it is stale.
    */
  private def noiseHeard(payload: Chunk[Byte]): F[Unit] =
    ComfortNoise.Level
      .parse(payload)
      .traverse_(level =>
        Async[F].monotonic.flatMap(now =>
          playback.update(before =>
            before.copy(noise =
              Some(ComfortNoise.Noise.heard(level, now, before.noise))
            )
          )
        )
      )

  /** What to play when there is no speech to: noise if some is wanted (the
    * other side told us the level, has not spoken since, is still sending and
    * the news is not stale), else, for a packet that was `lost`, the frame
    * before it again and fading, else nothing. Decided and recorded in one
    * step.
    */
  private def fill(frameBytes: Int, lost: Boolean): F[Unit] =
    (Async[F].monotonic, remoteSends.get).flatMapN { (now, sends) =>
      playback
        .modify(_.silent(now, sends, frameBytes, lost))
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
    // Decided in one step, so that two starts at once cannot both begin.
    agreed
      .modify {
        case Some(running) =>
          (
            Some(
              running.copy(
                remote = plan.remote,
                remoteDirection = plan.remoteDirection
              )
            ),
            Some(running)
          )
        case None => (Some(plan), None)
      }
      .flatMap {
        case Some(running) =>
          retarget(plan.remote.host, plan.remote.port, plan.remoteDirection)
            .as(
              if running.codec == plan.codec &&
                running.payloadType == plan.payloadType
              then MediaStart.Followed
              else MediaStart.CodecChanged
            )
        case None =>
          codecs
            .implementing(plan.codec)
            .liftTo[F](SoftphoneError.CodecUnavailable(plan.codec.encoding))
            .onError(_ => agreed.set(None))
            .flatMap { codec =>
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
        (
          Ref[F].of((-1L, -1)),
          plan.srtp.traverse(keys =>
            Ref[F].of(SrtpInbound(keys.suite, keys.receive)).map(keys -> _)
          ),
          plan.srtp.traverse(keys =>
            (
              Ref[F].of(SrtcpOutbound(keys.send)),
              Ref[F].of(SrtcpInbound(keys.receive))
            ).mapN(RtcpGuard(keys, _, _))
          )
        ).flatMapN { (lastDtmfKey, inbound, guard) =>
          Network[F]
            .bindDatagramSocket(address =
              SocketAddress(bindAddress, localRtpPort)
            )
            .use { socket =>
              // What arrives goes through the jitter buffer, and every 20 ms
              // what it gives is played: the packet's audio, a concealment for
              // one that is lost, or noise or nothing while there is nothing.
              val receiveF =
                Stream
                  .resource(codec.decoderSession[F])
                  .flatMap { decode =>
                    JitterPlayout(
                      jitterConfig,
                      MediaSession.FramePeriod,
                      RtpTransport
                        .receiveBytes(
                          socket,
                          lastDtmfKey,
                          onDtmf,
                          heardFrom,
                          plan.payloadType,
                          plan.telephoneEventType,
                          plan.comfortNoiseType,
                          inbound.fold(plain)(opener(_, _))
                        )
                        .evalTap(observe(plan.codec.clockRate))
                    ).evalMap((out, standing) =>
                      stats.set(standing) >>
                        play(plan, decode, codec.pcmPacketBytes, out)
                    )
                  }
                  .compile
                  .drain

              RtpSender
                .make(
                  socket,
                  target.get,
                  plan.payloadType,
                  plan.telephoneEventType,
                  codec.timestampStep,
                  plan.srtp.map(keys => SrtpOutbound(keys.suite, keys.send))
                )
                .flatMap { rtp =>
                  val pcmToRtp: Pipe[F, Byte, Unit] =
                    _.through(codec.encoder[F]).through(rtp.audio)
                  sender.complete(rtp) >>
                    sendPipeReady.complete(pcmToRtp) >>
                    rtcpSocket.use {
                      case None =>
                        receiveF.race(mediaStop.get.void).void
                      case Some(rtcpSocket) =>
                        receiveF
                          .race(mediaStop.get.void)
                          .race(reports(rtcpSocket, rtp, guard))
                          .void >> report(
                          rtcpSocket,
                          rtp,
                          guard,
                          leaving = true
                        )
                    }
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

  /** What an encrypted call protects its RTCP with: the keys, and the state of
    * each direction.
    */
  private[ua] final case class RtcpGuard[F[_]](
      keys: SrtpPlan,
      out: Ref[F, SrtcpOutbound],
      in: Ref[F, SrtcpInbound]
  )

  /** What the receiving end plays when it has no speech to play, kept together
    * because the two are decided together: the background noise the other side
    * told us of, and the speech to repeat for a packet that is lost.
    */
  private[ua] final case class Playback(
      noise: Option[ComfortNoise.Noise],
      concealment: Concealment
  ) {

    /** Speech was played: the noise that stood in for it ends. */
    def heard(pcm: Chunk[Byte]): Playback =
      Playback(None, concealment.heard(pcm))

    /** The frame to play in place of speech, if any, and the state after it. */
    def silent(
        now: FiniteDuration,
        remoteSends: Boolean,
        frameBytes: Int,
        lost: Boolean
    ): (Playback, Option[Chunk[Byte]]) =
      noise match {
        case Some(current) if remoteSends && current.isCurrent(now) =>
          val (next, frame) = current.frame(frameBytes / 2)
          (Playback(Some(next), Concealment.Empty), Some(frame))
        case Some(_)      => (Playback.Start, None)
        case None if lost =>
          val (next, frame) = concealment.fill(frameBytes)
          (copy(concealment = next), Some(frame))
        case None => (Playback.Start, None)
      }
  }

  private[ua] object Playback {
    val Start: Playback = Playback(None, Concealment.Empty)
  }

  /** The time one frame of audio stands for. */
  private val FramePeriod: FiniteDuration = 20.millis

  def create[F[_]: Async: Network](
      bindAddress: IpAddress,
      localRtpPort: Port,
      codecs: AudioCodecs,
      latching: RtpLatching,
      jitter: JitterConfig,
      rtcp: RtcpSchedule = RtcpSchedule.Default
  ): F[MediaSession[F]] =
    for {
      audioChannel  <- Channel.bounded[F, Chunk[Byte]](64)
      sendPipeReady <- Deferred[F, Pipe[F, Byte, Unit]]
      sender        <- Deferred[F, RtpSender[F]]
      mediaStop     <- Deferred[F, Unit]
      target        <- Ref[F].of(Option.empty[SocketAddress[IpAddress]])
      agreed        <- Ref[F].of(Option.empty[MediaPlan])
      latch         <- Ref[F].of(RtpLatch.empty)
      playback      <- Ref[F].of(Playback.Start)
      remoteSends   <- Ref[F].of(true)
      stats         <- Ref[F].of(JitterStats())
      receptions    <- Ref[F].of(Map.empty[Ssrc, RtpReception])
      peerBlock     <- Ref[F].of(Option.empty[ReportBlock])
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
      playback,
      remoteSends,
      jitter,
      stats,
      receptions,
      peerBlock,
      rtcp
    )
}
