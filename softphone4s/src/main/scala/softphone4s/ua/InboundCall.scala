package softphone4s.ua

import cats.effect.*
import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.concurrent.Channel
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.{Call, IncomingCall}
import softphone4s.codec.SdpCodec
import softphone4s.fsm.*
import softphone4s.fsm.SipMessages.*
import softphone4s.model.*
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transaction.{
  ClientNotice,
  ClientTransaction,
  ServerNotice,
  ServerTransaction,
  TransactionConfig,
  TransactionLayer,
  TransactionMessages
}
import InboundAction.*

import scala.concurrent.duration.*

/** One incoming call. It answers the INVITE through its server transaction,
  * repeats the 200 OK until the ACK, and turns what happens into events for
  * [[InboundCallFsm]]. Once accepted it is also the [[Call]].
  */
private[softphone4s] final class InboundCall[F[_]: Async] private (
    invite: ServerTransaction[F],
    layer: TransactionLayer[F],
    endpoint: LocalEndpoint,
    supervisor: Supervisor[F],
    entropy: Entropy[F],
    media: MediaSession[F],
    timers: Timers[F, CallTimer],
    hangupTimeout: FiniteDuration,
    stateRef: Ref[F, InboundState],
    events: Channel[F, InboundEvent],
    serverTransactions: Ref[F, Map[(SipMethod, CSeqNumber), ServerTransaction[
      F
    ]]],
    lastOk: Ref[F, Option[SipResponse]],
    established: Deferred[F, Either[Throwable, Unit]],
    callEnded: Deferred[F, Throwable],
    cancelledByCaller: Deferred[F, Unit],
    finished: Deferred[F, Unit],
    dtmfChannel: Channel[F, DtmfDigit],
    logger: StructuredLogger[F]
) extends Call[F]
    with IncomingCall[F] {

  private val peer: SocketAddress[IpAddress] = invite.from

  // ---- IncomingCall ------------------------------------------------------------

  def caller: NameAddr = invite.request.headers.from
  def callee: NameAddr = invite.request.headers.to

  def ringing: F[Unit] = events.send(InboundEvent.UserRinging).void

  def accept: Resource[F, Call[F]] =
    Resource.make(events.send(InboundEvent.UserAccept).as(this: Call[F]))(_ =>
      events.send(InboundEvent.HangupRequested) >>
        finished.get.timeoutTo(hangupTimeout, Async[F].unit)
    )

  def reject(status: FailureStatus): F[Unit] =
    events.send(InboundEvent.UserReject(status)).void

  def cancelled: F[Unit] = cancelledByCaller.get

  // ---- Call --------------------------------------------------------------------

  private def endedInterrupt: F[Either[Throwable, Unit]] =
    callEnded.get.map(Left(_))

  def source: Stream[F, Byte] =
    Stream.eval(callEnded.tryGet).flatMap {
      case Some(err) => Stream.raiseError[F](err)
      case None      => media.source.interruptWhen(endedInterrupt)
    }
  def sink: Pipe[F, Byte, Unit]        = media.sink
  def awaitPickup: F[Unit]             = established.get.flatMap(_.liftTo[F])
  def dtmfEvents: Stream[F, DtmfDigit] =
    Stream.eval(callEnded.tryGet).flatMap {
      case Some(err) => Stream.raiseError[F](err)
      case None      => dtmfChannel.stream.interruptWhen(endedInterrupt)
    }
  def awaitHangup: F[Unit] = callEnded.get.void

  /** True while the call can still be answered or is under way. */
  private[softphone4s] def isOpen: F[Boolean] =
    stateRef.get.map(_ != InboundState.Closed)

  // ---- The event loop -----------------------------------------------------------

  /** Runs until the call is over, then lets `finished` go. */
  private[softphone4s] def run(
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
          .as(Option(InboundEvent.RequestReceived(transaction.request)))
      case DialogInput.Unmatched(inbound) =>
        Async[F].pure(inbound.message match {
          case request: SipRequest if request.method == SipMethod.Ack =>
            Some(InboundEvent.AckReceived)
          case _ => None
        })
    }
    val fromInvite = invite.notices.collect {
      case ServerNotice.TransportFailed =>
        InboundEvent.TransactionFailed(TransactionFailure.TransportFailed)
    }
    fromDialog
      .merge(events.stream)
      .merge(fromInvite)
      .merge(timers.fired.map(InboundEvent.TimerFired(_)))
      .evalMap(process)
      .takeWhile(identity)
      .compile
      .drain
      .guarantee(finished.complete(()).void)
  }

  /** Runs one event through the machine; false once the call is over. */
  private def process(event: InboundEvent): F[Boolean] =
    stateRef
      .modify(s => InboundCallFsm.transition(event).run(s).value)
      .flatMap { actions =>
        actions
          .traverse_(interpret)
          .as(!actions.exists {
            case NotifyHangup(_) => true
            case _               => false
          })
      }

  // ---- Actions -------------------------------------------------------------------

  private[ua] def interpret(action: InboundAction): F[Unit] =
    action match {
      case SendRinging(ctx) =>
        invite.respond(
          endpoint.buildInviteResponse(ctx.invite, 180, "Ringing", ctx.localTag)
        )

      case SendInviteFailure(request, localTag, status) =>
        invite.respond(
          endpoint.buildInviteResponse(
            request,
            status.value,
            status.reason,
            localTag
          )
        )

      case SendInviteOk(ctx) =>
        val ok = endpoint.buildInviteResponse(
          ctx.invite,
          200,
          "OK",
          ctx.localTag,
          Some(SdpCodec.buildAnswerTo(ctx.offer, endpoint.ip, endpoint.rtpPort))
        )
        lastOk.set(Some(ok)) >> invite.respond(ok)

      case ResendOk =>
        lastOk.get.flatMap(_.traverse_(layer.transmit(_, peer)))

      case Reply(request, status, reason) =>
        respondTo(
          request,
          TransactionMessages.responseTo(request, status, reason)
        )

      case SendByeOk(request) =>
        respondTo(request, endpoint.buildByeOk(request))
      case SendInfoOk(request) =>
        respondTo(request, endpoint.buildInfoOk(request))

      case SendBye(dialog) =>
        entropy.branch.flatMap(branch =>
          logger.debug("Sending BYE") >> startClient(
            endpoint.buildBye(dialog, branch)
          )
        )

      case StartMedia(offer) =>
        media.start(offer.ip, offer.port, supervisor, dtmfChannel.send(_).void)

      case StopMedia => media.stop()

      case NotifyEstablished => established.complete(Right(())).void

      case NotifyHangup(reason) =>
        val err: Throwable = reason match {
          case TerminationReason.RemoteBye        => SoftphoneError.RemoteHangup
          case TerminationReason.TimedOut         => SoftphoneError.CallTimedOut
          case TerminationReason.TransportFailure =>
            SoftphoneError.TransportFailed
          case TerminationReason.Rejected(c, r) =>
            SoftphoneError.CallRejected(c, r)
          case TerminationReason.LocalHangup |
              TerminationReason.CancelledByCaller |
              TerminationReason.RejectedLocally =>
            SoftphoneError.CallCancelled
        }
        val signalStreams = reason match {
          case TerminationReason.LocalHangup => Async[F].unit
          case _                             => callEnded.complete(err).void
        }
        logger.debug(Map("reason" -> reason.toString))("Incoming call ended") >>
          cancelledByCaller
            .complete(())
            .void
            .whenA(reason == TerminationReason.CancelledByCaller) >>
          signalStreams >>
          dtmfChannel.close.void >>
          established.complete(Left(err)).void

      case NotifyDtmf(digit) => dtmfChannel.send(digit).void

      case Timer(timerAction) => timers.run(timerAction)
    }

  /** Answers `request` through its server transaction, or directly if it has
    * none.
    */
  private def respondTo(request: SipRequest, response: SipResponse): F[Unit] = {
    val key = (request.method, request.headers.cseq.seq)
    serverTransactions.modify(m => (m - key, m.get(key))).flatMap {
      case Some(transaction) => transaction.respond(response)
      case None              => layer.transmit(response, peer)
    }
  }

  private def startClient(request: SipRequest): F[Unit] =
    layer.client(request, peer).flatMap {
      case Right(transaction) => supervisor.supervise(forward(transaction)).void
      case Left(error)        =>
        logger.warn(Map("error" -> error.message))(
          "Cannot start a transaction"
        ) >>
          events
            .send(
              InboundEvent.TransactionFailed(TransactionFailure.TransportFailed)
            )
            .void
    }

  private def forward(transaction: ClientTransaction[F]): F[Unit] =
    transaction.notices
      .evalMap {
        case ClientNotice.Response(response) =>
          events.send(InboundEvent.ResponseReceived(response))
        case ClientNotice.TimedOut =>
          events.send(
            InboundEvent.TransactionFailed(TransactionFailure.TimedOut)
          )
        case ClientNotice.TransportFailed =>
          events.send(
            InboundEvent.TransactionFailed(TransactionFailure.TransportFailed)
          )
      }
      .compile
      .drain
}

private[softphone4s] object InboundCall {

  /** Takes the INVITE of `invite` and starts the call: the machine's first
    * actions run at once, so a call we cannot take is already answered with its
    * failure when this returns (check `isOpen`).
    */
  def create[F[_]: Async](
      invite: ServerTransaction[F],
      layer: TransactionLayer[F],
      endpoint: LocalEndpoint,
      supervisor: Supervisor[F],
      entropy: Entropy[F],
      media: MediaSession[F],
      timers: Timers[F, CallTimer],
      transactionConfig: TransactionConfig,
      answerTimeout: FiniteDuration,
      hangupTimeout: FiniteDuration,
      logger: StructuredLogger[F]
  ): F[InboundCall[F]] =
    for {
      localTag <- entropy.tag
      (state, firstActions) = InboundCallFsm.start(
        invite.request,
        localTag,
        transactionConfig,
        answerTimeout
      )
      stateRef           <- Ref[F].of(state)
      events             <- Channel.unbounded[F, InboundEvent]
      serverTransactions <- Ref[F].of(
        Map.empty[(SipMethod, CSeqNumber), ServerTransaction[F]]
      )
      lastOk            <- Ref[F].of(Option.empty[SipResponse])
      established       <- Deferred[F, Either[Throwable, Unit]]
      callEnded         <- Deferred[F, Throwable]
      cancelledByCaller <- Deferred[F, Unit]
      finished          <- Deferred[F, Unit]
      dtmfChannel       <- Channel.bounded[F, DtmfDigit](64)
      call = new InboundCall(
        invite,
        layer,
        endpoint,
        supervisor,
        entropy,
        media,
        timers,
        hangupTimeout,
        stateRef,
        events,
        serverTransactions,
        lastOk,
        established,
        callEnded,
        cancelledByCaller,
        finished,
        dtmfChannel,
        logger
      )
      _ <- firstActions.traverse_(call.interpret)
    } yield call
}
