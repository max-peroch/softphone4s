package softphone4s.ua

import cats.effect.*
import cats.effect.std.{Random, Supervisor}
import cats.syntax.all.*
import fs2.concurrent.Channel
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.Call
import softphone4s.codec.{DigestAuth, SdpCodec}
import softphone4s.fsm.*
import softphone4s.model.*
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
    builder: SipMessageBuilder,
    supervisor: Supervisor[F],
    inviteBranch: Ref[F, String],
    nonceCounts: Ref[F, Map[String, Int]],
    random: Random[F],
    media: MediaSession[F],
    inviteTimeout: FiniteDuration,
    logger: StructuredLogger[F]
) extends Call[F] {

  private def newHex(byteCount: Int): F[String] =
    random.nextBytes(byteCount).map(_.map("%02x".format(_)).mkString)
  private def newBranch(): F[String] = newHex(8).map("z9hG4bK" + _)

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
        Map("callId" -> req.headers.callId, "method" -> req.method.toString)
      )("Received SIP request")
    case SipEvent.ResponseReceived(res) =>
      logger.debug(
        Map(
          "callId"     -> res.headers.callId,
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
              Map("callee" -> callee, "callId" -> callId)
            )("Sending INVITE") >>
            sendSip(
              builder.buildInvite(
                callee,
                callId,
                localTag,
                cseq,
                branch,
                authHeader
              )
            )
        }

      case RetryInviteWithAuth(ctx, headerName, challengeValue) =>
        for {
          cnonce     <- newHex(8)
          nonceCount <- challengeValue
            .flatMap(DigestAuth.extractNonce)
            .fold(Async[F].pure(1)) { nonce =>
              nonceCounts.modify { m =>
                val next = m.getOrElse(nonce, 0) + 1
                (m + (nonce -> next), next)
              }
            }
          authHeader = builder.computeAuthHeader(
            ctx.callee,
            headerName,
            challengeValue,
            cnonce,
            nonceCount
          )
          branch <- newBranch()
          _      <- inviteBranch.set(branch)
          _      <- logger.debug(
            Map(
              "callId"     -> ctx.callId,
              "cseq"       -> ctx.cseq.toString,
              "nonceCount" -> nonceCount.toString
            )
          )("Retrying INVITE with authentication")
          _ <- sendSip(
            builder.buildInvite(
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
              builder.buildAck(
                dialog,
                branch,
                remoteSdpForAnswer.map(_ => builder.buildSdpAnswer)
              )
            )
        }

      case SendAckForNon2xx(ctx, response) =>
        inviteBranch.get.flatMap { branch =>
          logger.debug(
            Map("statusCode" -> response.statusCode.toString)
          )("Sending ACK for non-2xx response") >>
            sendSip(builder.buildAckForNon2xx(ctx, response, branch))
        }

      case SendCancel(ctx) =>
        inviteBranch.get.flatMap { branch =>
          logger.debug("Sending CANCEL") >> sendSip(
            builder.buildCancel(ctx, branch)
          )
        }

      case SendBye(dialog) =>
        newBranch().flatMap { branch =>
          logger.debug("Sending BYE") >> sendSip(
            builder.buildBye(dialog, branch)
          )
        }

      case SendByeOk(request) =>
        logger.debug("Acknowledging BYE") >> sendSip(
          builder.buildByeOk(request)
        )

      case SendInfoOk(request) =>
        logger.debug("Acknowledging INFO") >> sendSip(
          builder.buildInfoOk(request)
        )

      case StartMedia(sdpBody) =>
        val (remoteIp, remotePort) =
          SdpCodec.parseAudioEndpoint(
            sdpBody,
            builder.bindAddress,
            builder.localRtpPort
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
      callId: String,
      callee: String,
      sendSip: SipMessage => F[Unit],
      builder: SipMessageBuilder,
      supervisor: Supervisor[F],
      random: Random[F],
      media: MediaSession[F],
      inviteTimeout: FiniteDuration,
      logger: StructuredLogger[F],
      extraHeaders: Map[String, List[String]] = Map.empty
  ): F[OutboundCall[F]] =
    for {
      localTag <- random.nextBytes(4).map(_.map("%02x".format(_)).mkString)
      branch   <- random
        .nextBytes(8)
        .map(_.map("%02x".format(_)).mkString)
        .map("z9hG4bK" + _)
      inviteBranch <- Ref[F].of(branch)
      ctx = InviteCtx(
        callId = callId,
        localTag = localTag,
        cseq = 1,
        callee = callee,
        localUri = builder.localSipAddress,
        offerInInvite = true
      )
      answered     <- Deferred[F, Either[Throwable, Unit]]
      callEnded    <- Deferred[F, Throwable]
      dtmfChannel  <- Channel.bounded[F, DtmfDigit](64)
      hangupSignal <- Deferred[F, Unit]
      stateRef     <- Ref[F].of[CallState](CallState.Inviting(ctx))
      nonceCounts  <- Ref[F].of(Map.empty[String, Int])
      call = new OutboundCall(
        answered,
        callEnded,
        dtmfChannel,
        hangupSignal,
        stateRef,
        sendSip,
        builder,
        supervisor,
        inviteBranch,
        nonceCounts,
        random,
        media,
        inviteTimeout,
        logger
      )
      _ <- logger.debug(
        Map("callee" -> callee, "callId" -> callId)
      )("Sending INVITE")
      _ <- sendSip(
        builder.buildInvite(
          callee,
          callId,
          localTag,
          cseq = 1,
          branch,
          extraHeaders = extraHeaders
        )
      )
    } yield call
}
