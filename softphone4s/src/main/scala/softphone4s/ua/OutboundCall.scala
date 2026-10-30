package softphone4s.ua

import cats.effect.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import fs2.concurrent.Channel
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.Call
import softphone4s.codec.SdpCodec
import softphone4s.fsm.*
import softphone4s.fsm.SipMessages.*
import softphone4s.model.*
import softphone4s.runtime.Entropy
import SipAction.*
import TerminationReason.*

import scala.concurrent.duration.*

private[softphone4s] class OutboundCall[F[_]: Async] private (
    answered: Deferred[F, Either[Throwable, Unit]],
    callEnded: Deferred[F, Throwable],
    dtmfChannel: Channel[F, DtmfDigit],
    hangupSignal: Deferred[F, Unit],
    stateRef: Ref[F, CallState],
    sendSip: SipMessage => F[Unit],
    endpoint: LocalEndpoint,
    supervisor: Supervisor[F],
    inviteBranch: Ref[F, Branch],
    nonceCounts: Ref[F, Map[Nonce, NonceCount]],
    entropy: Entropy[F],
    media: MediaSession[F],
    inviteTimeout: FiniteDuration,
    logger: StructuredLogger[F]
) extends Call[F] {

  private def newBranch(): F[Branch] = entropy.branch

  private def hangupInterrupt: F[Either[Throwable, Unit]] =
    callEnded.get.map(Left(_))

  def source: Stream[F, Byte] =
    Stream.eval(callEnded.tryGet).flatMap {
      case Some(err) => Stream.raiseError[F](err)
      case None      => media.source.interruptWhen(hangupInterrupt)
    }
  def sink: Pipe[F, Byte, Unit]            = media.sink
  def awaitPickup: F[Unit]                 = answered.get.flatMap(_.liftTo[F])
  private[softphone4s] def hangup: F[Unit] = hangupSignal.complete(()).void
  def dtmfEvents: Stream[F, DtmfDigit]     =
    Stream.eval(callEnded.tryGet).flatMap {
      case Some(err) => Stream.raiseError[F](err)
      case None      => dtmfChannel.stream.interruptWhen(hangupInterrupt)
    }
  def awaitHangup: F[Unit] = callEnded.get.void

  private[softphone4s] def runEventLoop(
      sipIn: Stream[F, SipMessage]
  ): F[Unit] = {
    val timeout   = Stream.sleep[F](inviteTimeout).as(SipEvent.Timeout)
    val sipEvents = sipIn.map {
      case req: SipRequest  => SipEvent.RequestReceived(req)
      case res: SipResponse => SipEvent.ResponseReceived(res)
    }
    sipEvents
      .merge(Stream.eval(hangupSignal.get).as(SipEvent.HangupRequested))
      .merge(timeout)
      .evalMap(processEvent)
      .takeWhile(identity)
      .compile
      .drain
  }

  // Returns false when the call has reached a terminal state (loop should stop).
  private def processEvent(event: SipEvent): F[Boolean] =
    logIncoming(event) >>
      stateRef
        .modify(state => CallFsm.transition(event).run(state).value)
        .flatMap { actions =>
          actions
            .traverse_(interpretAction)
            .as(
              !actions.exists { case NotifyHangup(_) => true; case _ => false }
            )
        }

  private def logIncoming(event: SipEvent): F[Unit] = event match {
    case SipEvent.RequestReceived(req) =>
      logger.debug(
        Map(
          "callId" -> req.headers.callId.value,
          "method" -> req.method.toString
        )
      )("Received SIP request")
    case SipEvent.ResponseReceived(res) =>
      logger.debug(
        Map(
          "callId"     -> res.headers.callId.value,
          "cseq"       -> res.headers.cseq.method.toString,
          "statusCode" -> res.statusCode.toString,
          "reason"     -> res.reason
        )
      )("Received SIP response")
    case SipEvent.HangupRequested | SipEvent.Timeout => Async[F].unit
  }

  private def interpretAction(action: SipAction): F[Unit] =
    action match {
      case SendInvite(callee, callId, localTag, cseq, authHeader) =>
        newBranch().flatMap { branch =>
          inviteBranch.set(branch) >>
            logger.debug(
              Map("callee" -> callee.render, "callId" -> callId.value)
            )("Sending INVITE") >>
            sendSip(
              endpoint.buildInvite(
                callee,
                callId,
                localTag,
                cseq,
                branch,
                authHeader
              )
            )
        }

      case RetryInviteWithAuth(ctx, kind, challenge) =>
        for {
          cnonce     <- entropy.cnonce
          nonceCount <- nonceCounts.modify { m =>
            val next = m.get(challenge.nonce).fold(NonceCount.One)(_.next)
            (m.updated(challenge.nonce, next), next)
          }
          authHeader = Some(
            endpoint.authorizationFor(
              kind,
              ctx.callee,
              challenge,
              cnonce,
              nonceCount
            )
          )
          branch <- newBranch()
          _      <- inviteBranch.set(branch)
          _      <- logger.debug(
            Map(
              "callId"     -> ctx.callId.value,
              "cseq"       -> ctx.cseq.value.toString,
              "nonceCount" -> nonceCount.render
            )
          )("Retrying INVITE with authentication")
          _ <- sendSip(
            endpoint.buildInvite(
              ctx.callee,
              ctx.callId,
              ctx.localTag,
              ctx.cseq,
              branch,
              authHeader
            )
          )
        } yield ()

      case SendAck(dialog, remoteSdpForAnswer) =>
        newBranch().flatMap { branch =>
          logger.debug("Sending ACK") >>
            sendSip(
              endpoint.buildAck(
                dialog,
                branch,
                remoteSdpForAnswer.map(_ => endpoint.buildSdpAnswer)
              )
            )
        }

      case SendAckForNon2xx(ctx, response) =>
        inviteBranch.get.flatMap { branch =>
          logger.debug(
            Map("statusCode" -> response.statusCode.toString)
          )("Sending ACK for non-2xx response") >>
            sendSip(endpoint.buildAckForNon2xx(ctx, response, branch))
        }

      case SendCancel(ctx) =>
        inviteBranch.get.flatMap { branch =>
          logger.debug("Sending CANCEL") >> sendSip(
            endpoint.buildCancel(ctx, branch)
          )
        }

      case SendBye(dialog) =>
        newBranch().flatMap { branch =>
          logger.debug("Sending BYE") >> sendSip(
            endpoint.buildBye(dialog, branch)
          )
        }

      case SendByeOk(request) =>
        logger.debug("Acknowledging BYE") >> sendSip(
          endpoint.buildByeOk(request)
        )

      case SendInfoOk(request) =>
        logger.debug("Acknowledging INFO") >> sendSip(
          endpoint.buildInfoOk(request)
        )

      case StartMedia(sdpBody) =>
        val (remoteIp, remotePort) =
          SdpCodec.parseAudioEndpoint(
            sdpBody,
            endpoint.bindAddress,
            endpoint.rtpPort
          )
        media.start(remoteIp, remotePort, supervisor, dtmfChannel.send(_).void)

      case StopMedia =>
        media.stop()

      case NotifyAnswered =>
        answered.complete(Right(())).void

      case NotifyHangup(reason) =>
        val err: Throwable = reason match {
          case Rejected(code, msg) => SoftphoneError.CallRejected(code, msg)
          case TimedOut            => SoftphoneError.CallTimedOut
          case LocalHangup         => SoftphoneError.CallCancelled
          case RemoteBye           => SoftphoneError.RemoteHangup
        }
        val signalStreams = reason match {
          case LocalHangup => Async[F].unit
          case _           => callEnded.complete(err).void
        }
        logger.debug(Map("reason" -> reason.toString))("Call ended") >>
          signalStreams >>
          dtmfChannel.close.void >>
          answered.complete(Left(err)).void

      case NotifyDtmf(digit) =>
        dtmfChannel.send(digit).void
    }
}

private[softphone4s] object OutboundCall {
  def create[F[_]: Async](
      callId: CallId,
      callee: SipUri,
      sendSip: SipMessage => F[Unit],
      endpoint: LocalEndpoint,
      supervisor: Supervisor[F],
      entropy: Entropy[F],
      media: MediaSession[F],
      inviteTimeout: FiniteDuration,
      logger: StructuredLogger[F],
      extraHeaders: List[ExtensionHeader] = Nil
  ): F[OutboundCall[F]] =
    for {
      localTag     <- entropy.tag
      branch       <- entropy.branch
      inviteBranch <- Ref[F].of(branch)
      ctx = InviteCtx(
        callId = callId,
        localTag = localTag,
        cseq = CSeqNumber.One,
        callee = callee,
        localUri = endpoint.accountUri,
        offerInInvite = true
      )
      answered     <- Deferred[F, Either[Throwable, Unit]]
      callEnded    <- Deferred[F, Throwable]
      dtmfChannel  <- Channel.bounded[F, DtmfDigit](64)
      hangupSignal <- Deferred[F, Unit]
      stateRef     <- Ref[F].of[CallState](CallState.Inviting(ctx))
      nonceCounts  <- Ref[F].of(Map.empty[Nonce, NonceCount])
      call = new OutboundCall(
        answered,
        callEnded,
        dtmfChannel,
        hangupSignal,
        stateRef,
        sendSip,
        endpoint,
        supervisor,
        inviteBranch,
        nonceCounts,
        entropy,
        media,
        inviteTimeout,
        logger
      )
      _ <- logger.debug(
        Map("callee" -> callee.render, "callId" -> callId.value)
      )("Sending INVITE")
      _ <- sendSip(
        endpoint.buildInvite(
          callee,
          callId,
          localTag,
          cseq = CSeqNumber.One,
          branch,
          extraHeaders = extraHeaders
        )
      )
    } yield call
}
