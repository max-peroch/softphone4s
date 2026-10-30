package softphone4s.ua

import cats.effect.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.concurrent.Channel
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.Call
import softphone4s.codec.SdpCodec
import softphone4s.fsm.*
import softphone4s.fsm.SipMessages.*
import softphone4s.model.*
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transaction.{
  ClientNotice,
  ClientTransaction,
  ServerTransaction,
  TransactionLayer
}
import SipAction.*
import TerminationReason.*

import scala.concurrent.duration.*

/** One outbound call: the INVITE, BYE and CANCEL go out as client transactions,
  * in-dialog BYE and INFO arrive as server transactions, and everything that
  * happens becomes an event for [[CallFsm]].
  */
private[softphone4s] class OutboundCall[F[_]: Async] private (
    answered: Deferred[F, Either[Throwable, Unit]],
    callEnded: Deferred[F, Throwable],
    dtmfChannel: Channel[F, DtmfDigit],
    hangupSignal: Deferred[F, Unit],
    stateRef: Ref[F, CallState],
    layer: TransactionLayer[F],
    peer: SocketAddress[IpAddress],
    events: Channel[F, SipEvent],
    serverTransactions: Ref[F, Map[(SipMethod, CSeqNumber), ServerTransaction[
      F
    ]]],
    cancelled: Ref[F, Boolean],
    endpoint: LocalEndpoint,
    supervisor: Supervisor[F],
    inviteBranch: Ref[F, Branch],
    nonceCounts: Ref[F, Map[Nonce, NonceCount]],
    entropy: Entropy[F],
    media: MediaSession[F],
    timers: Timers[F, CallTimer],
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

  // ---- The event loop ----------------------------------------------------------

  private[softphone4s] def runEventLoop(
      dialogInput: Stream[F, DialogInput[F]]
  ): F[Unit] = {
    val fromDialog = dialogInput.evalMapFilter {
      case DialogInput.Request(transaction) =>
        serverTransactions
          .update(
            _.updated(
              (
                transaction.request.method,
                transaction.request.headers.cseq.seq
              ),
              transaction
            )
          )
          .as(Option(SipEvent.RequestReceived(transaction.request)))
      case DialogInput.Unmatched(inbound) =>
        Async[F].pure(inbound.message match {
          case response: SipResponse =>
            Some(SipEvent.ResponseReceived(response))
          case _: SipRequest => None
        })
    }
    fromDialog
      .merge(events.stream)
      .merge(Stream.eval(hangupSignal.get).as(SipEvent.HangupRequested))
      .merge(timers.fired.map(SipEvent.TimerFired(_)))
      .evalMap(processEvent)
      .takeWhile(identity)
      .compile
      .drain
  }

  /** Runs one event through the FSM. Returns false when the call is over and
    * the loop should stop. After a CANCEL the loop keeps going until the final
    * response to the INVITE arrives, since a 200 can cross the CANCEL and then
    * needs an ACK and a BYE (RFC 3261 §9.1).
    */
  private def processEvent(event: SipEvent): F[Boolean] =
    logIncoming(event) >>
      stateRef
        .modify(state => CallFsm.transition(event).run(state).value)
        .flatMap { actions =>
          for {
            _             <- actions.traverse_(interpretAction)
            _             <- cancelled.set(true).whenA(actions.exists(isCancel))
            awaitingFinal <- cancelled.get
          } yield {
            val ended = actions.exists {
              case NotifyHangup(_) => true; case _ => false
            }
            if awaitingFinal then !endsCancelledCall(event) else !ended
          }
        }

  private def isCancel(action: SipAction): Boolean = action match {
    case SendCancel(_) => true
    case _             => false
  }

  /** After a CANCEL, the INVITE's final response (or the transaction failing)
    * is the last thing to wait for.
    */
  private def endsCancelledCall(event: SipEvent): Boolean = event match {
    case SipEvent.ResponseReceived(r) =>
      r.headers.cseq.method == SipMethod.Invite && r.statusCode >= 200
    case SipEvent.TransactionFailed(_) => true
    case _                             => false
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
    case SipEvent.TransactionFailed(failure) =>
      logger.debug(Map("failure" -> failure.toString))("Transaction failed")
    case SipEvent.HangupRequested | SipEvent.TimerFired(_) => Async[F].unit
  }

  // ---- Transactions ---------------------------------------------------------------

  /** Sends `request` in a client transaction and turns what the transaction
    * reports into events. A request that cannot even start counts as a
    * transport failure.
    */
  private def startClient(request: SipRequest): F[Unit] =
    layer.client(request, peer).flatMap {
      case Right(transaction) =>
        supervisor.supervise(forward(transaction)).void
      case Left(error) =>
        logger.warn(Map("error" -> error.message))(
          "Cannot start a transaction"
        ) >>
          events
            .send(
              SipEvent.TransactionFailed(TransactionFailure.TransportFailed)
            )
            .void
    }

  private def forward(transaction: ClientTransaction[F]): F[Unit] =
    transaction.notices
      .evalMap {
        case ClientNotice.Response(response) =>
          events.send(SipEvent.ResponseReceived(response))
        case ClientNotice.TimedOut =>
          events.send(SipEvent.TransactionFailed(TransactionFailure.TimedOut))
        case ClientNotice.TransportFailed =>
          events.send(
            SipEvent.TransactionFailed(TransactionFailure.TransportFailed)
          )
      }
      .compile
      .drain

  /** Answers the in-dialog request `request` through its server transaction. */
  private def respondTo(request: SipRequest, response: SipResponse): F[Unit] = {
    val key = (request.method, request.headers.cseq.seq)
    serverTransactions.modify(m => (m - key, m.get(key))).flatMap {
      case Some(transaction) => transaction.respond(response)
      case None              =>
        logger.warn(Map("method" -> request.method.toString))(
          "No server transaction to answer; sending the response directly"
        ) >> layer.transmit(response, peer)
    }
  }

  // ---- Actions -------------------------------------------------------------------

  private def interpretAction(action: SipAction): F[Unit] =
    action match {
      case SendInvite(callee, callId, localTag, cseq, authHeader) =>
        newBranch().flatMap { branch =>
          inviteBranch.set(branch) >>
            logger.debug(
              Map("callee" -> callee.render, "callId" -> callId.value)
            )("Sending INVITE") >>
            startClient(
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
              SipMethod.Invite,
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
          _ <- startClient(
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

      // The ACK of a 2xx is not part of a transaction (RFC 3261 §13.2.2.4).
      case SendAck(dialog, remoteSdpForAnswer) =>
        newBranch().flatMap { branch =>
          logger.debug("Sending ACK") >>
            layer.transmit(
              endpoint.buildAck(
                dialog,
                branch,
                remoteSdpForAnswer.map(_ => endpoint.buildSdpAnswer)
              ),
              peer
            )
        }

      case SendCancel(ctx) =>
        inviteBranch.get.flatMap { branch =>
          logger.debug("Sending CANCEL") >> startClient(
            endpoint.buildCancel(ctx, branch)
          )
        }

      case SendBye(dialog) =>
        newBranch().flatMap { branch =>
          logger.debug("Sending BYE") >> startClient(
            endpoint.buildBye(dialog, branch)
          )
        }

      case SendByeOk(request) =>
        logger.debug("Acknowledging BYE") >>
          respondTo(request, endpoint.buildByeOk(request))

      case SendInfoOk(request) =>
        logger.debug("Acknowledging INFO") >>
          respondTo(request, endpoint.buildInfoOk(request))

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
          case TransportFailure    => SoftphoneError.TransportFailed
          case CancelledByCaller | RejectedLocally =>
            SoftphoneError.CallCancelled
          case LocalHangup => SoftphoneError.CallCancelled
          case RemoteBye   => SoftphoneError.RemoteHangup
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

      case Timer(action) =>
        timers.run(action)
    }
}

private[softphone4s] object OutboundCall {

  /** Starts the call by sending its INVITE to `peer` through `layer`. */
  def create[F[_]: Async](
      callId: CallId,
      callee: SipUri,
      layer: TransactionLayer[F],
      peer: SocketAddress[IpAddress],
      endpoint: LocalEndpoint,
      supervisor: Supervisor[F],
      entropy: Entropy[F],
      media: MediaSession[F],
      timers: Timers[F, CallTimer],
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
        offerInInvite = true,
        inviteTimeout = inviteTimeout
      )
      answered           <- Deferred[F, Either[Throwable, Unit]]
      callEnded          <- Deferred[F, Throwable]
      dtmfChannel        <- Channel.bounded[F, DtmfDigit](64)
      hangupSignal       <- Deferred[F, Unit]
      stateRef           <- Ref[F].of[CallState](CallState.Inviting(ctx))
      nonceCounts        <- Ref[F].of(Map.empty[Nonce, NonceCount])
      events             <- Channel.unbounded[F, SipEvent]
      serverTransactions <- Ref[F].of(
        Map.empty[(SipMethod, CSeqNumber), ServerTransaction[F]]
      )
      cancelled <- Ref[F].of(false)
      call = new OutboundCall(
        answered,
        callEnded,
        dtmfChannel,
        hangupSignal,
        stateRef,
        layer,
        peer,
        events,
        serverTransactions,
        cancelled,
        endpoint,
        supervisor,
        inviteBranch,
        nonceCounts,
        entropy,
        media,
        timers,
        logger
      )
      _ <- timers.start(CallTimer.InviteTimeout, inviteTimeout)
      _ <- logger.debug(
        Map("callee" -> callee.render, "callId" -> callId.value)
      )("Sending INVITE")
      _ <- call.startClient(
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
