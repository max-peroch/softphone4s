package softphone4s.fsm

import cats.data.State
import cats.syntax.all.*
import softphone4s.codec.SdpCodec
import softphone4s.model.*
import softphone4s.sdp.Capabilities
import softphone4s.transaction.TransactionConfig
import scala.concurrent.duration.*

/** An incoming call (RFC 3261 §13.3, UAS side) as a pure state machine: the
  * application rings, accepts or rejects; the caller may cancel; the 200 OK is
  * repeated until the ACK; BYE and INFO are answered once the call stands.
  */
object InboundCallFsm {
  import InboundAction.*
  import InboundEvent.*
  import InboundState.*

  type Transition = State[InboundState, List[InboundAction]]

  /** A call not yet answered: offered or ringing. */
  private object Pending {
    def unapply(state: InboundState): Option[InboundCtx] = state match {
      case Offered(ctx) => Some(ctx)
      case Ringing(ctx) => Some(ctx)
      case _            => None
    }
  }

  /** Begins the call for `invite`. A call we cannot take (no SDP we can answer,
    * or no From-tag to build a dialog on) is turned away at once: the state is
    * `Closed` and the actions say so.
    */
  def start(
      invite: SipRequest,
      localTag: Tag,
      config: TransactionConfig,
      answerTimeout: FiniteDuration,
      capabilities: Capabilities
  ): (InboundState, List[InboundAction]) = {
    def turnAway(status: FailureStatus) =
      (
        Closed,
        List(
          SendInviteFailure(invite, localTag, status),
          NotifyHangup(TerminationReason.RejectedLocally)
        )
      )
    // A sips: request has to arrive over TLS on every hop (RFC 3261 §26.2.2),
    // and the last one is the one we can see.
    val insecureSips =
      invite.requestUri.scheme == SipScheme.Sips &&
        invite.headers.topVia.transport != TransportProtocol.Tls
    SipDialog.forInboundInvite(invite, localTag) match {
      case _ if insecureSips => turnAway(FailureStatus.Forbidden)
      case None              => turnAway(FailureStatus.BadRequest)
      case Some(dialog)      =>
        SdpCodec.parseOffer(invite.body, capabilities) match {
          case None        => turnAway(FailureStatus.NotAcceptableHere)
          case Some(offer) =>
            (
              Offered(InboundCtx(invite, localTag, offer, dialog, config)),
              List(
                Timer(TimerAction.Start(CallTimer.AnswerTimeout, answerTimeout))
              )
            )
        }
    }
  }

  def transition(
      event: InboundEvent,
      capabilities: Capabilities = Capabilities.Default
  ): Transition =
    State.get[InboundState].flatMap { state =>
      (state, event) match {

        // ---- Before the answer ----------------------------------------------
        case (Offered(ctx), UserRinging) =>
          go(Ringing(ctx), List(SendRinging(ctx)))

        case (Offered(ctx), UserAccept) => accept(ctx)
        case (Ringing(ctx), UserAccept) => accept(ctx)

        case (Offered(ctx), UserReject(status)) =>
          reject(ctx, status, TerminationReason.RejectedLocally)
        case (Ringing(ctx), UserReject(status)) =>
          reject(ctx, status, TerminationReason.RejectedLocally)
        case (Offered(ctx), HangupRequested) =>
          reject(ctx, FailureStatus.Decline, TerminationReason.RejectedLocally)
        case (Ringing(ctx), HangupRequested) =>
          reject(ctx, FailureStatus.Decline, TerminationReason.RejectedLocally)

        case (Pending(ctx), TimerFired(CallTimer.AnswerTimeout)) =>
          reject(
            ctx,
            FailureStatus.TemporarilyUnavailable,
            TerminationReason.TimedOut
          )

        // The caller gives up: 200 for the CANCEL, 487 for the INVITE (§9.2).
        case (Pending(ctx), RequestReceived(request))
            if request.method == SipMethod.Cancel =>
          if sameTransaction(ctx, request) then
            go(
              Closed,
              List(
                stopTimer(CallTimer.AnswerTimeout),
                Reply(request, 200, "OK"),
                SendInviteFailure(
                  ctx.invite,
                  ctx.localTag,
                  FailureStatus.RequestTerminated
                ),
                NotifyHangup(TerminationReason.CancelledByCaller)
              )
            )
          else State.pure(List(noSuchTransaction(request)))

        // ---- 200 OK sent, awaiting the ACK ----------------------------------
        case (Answered(ctx, _), AckReceived) =>
          go(
            Active(ctx.dialog),
            List(
              stopTimer(CallTimer.RetransmitOk),
              stopTimer(CallTimer.AckTimeout),
              NotifyEstablished
            )
          )

        case (Answered(ctx, interval), TimerFired(CallTimer.RetransmitOk)) =>
          val next = ctx.config.nextCapped(interval)
          go(
            Answered(ctx, next),
            List(
              ResendOk,
              Timer(TimerAction.Start(CallTimer.RetransmitOk, next))
            )
          )

        // No ACK in 64 * T1: the dialog is over (§13.3.1.4).
        case (Answered(ctx, _), TimerFired(CallTimer.AckTimeout)) =>
          go(
            Closed,
            List(
              stopTimer(CallTimer.RetransmitOk),
              StopMedia,
              SendBye(ctx.dialog),
              NotifyHangup(TerminationReason.TimedOut)
            )
          )

        // ---- Once answered ----------------------------------------------------
        case (Answered(ctx, _), HangupRequested) =>
          hangUp(ctx.dialog, stopRetransmits)
        case (Active(dialog), HangupRequested)        => hangUp(dialog, Nil)
        case (Reinviting(dialog, _), HangupRequested) => hangUp(dialog, Nil)
        case (Retrying(dialog, _), HangupRequested)   =>
          hangUp(dialog, List(stopTimer(CallTimer.ReinviteRetry)))

        // ---- Our own session changes (RFC 3261 §14.1) -------------------------
        case (Active(dialog), SessionChangeRequested(change)) =>
          sendReinvite(dialog, change)

        case (Retrying(dialog, change), TimerFired(CallTimer.ReinviteRetry)) =>
          sendReinvite(dialog, change)

        case (Reinviting(dialog, change), ResponseReceived(response))
            if response.headers.cseq.method == change.method.sipMethod &&
              response.headers.cseq.seq == dialog.localCseq =>
          Reinvite.outcome(response) match {
            case Reinvite.Outcome.Progress => State.pure(Nil)
            case Reinvite.Outcome.Accepted =>
              go(
                Active(dialog),
                // Only an INVITE's 2xx is ACKed.
                Option
                  .when(change.method == SessionMethod.Invite)(SendAck(dialog))
                  .toList :+ ApplyAnswer(response)
              )
            case Reinvite.Outcome.Glare =>
              go(
                Retrying(dialog, change),
                List(ScheduleReinviteRetry(dialog.role))
              )
            case Reinvite.Outcome.DialogGone => hangUp(dialog, Nil)
            case Reinvite.Outcome.Refused    => go(Active(dialog), Nil)
          }

        case (Reinviting(dialog, _), TransactionFailed(_)) =>
          hangUp(dialog, Nil)

        case (Answered(ctx, _), RequestReceived(request)) =>
          inDialog(
            capabilities,
            ctx.dialog,
            request,
            stopRetransmits,
            answeredWithoutAck = true,
            Active(_)
          )
        case (Active(dialog), RequestReceived(request)) =>
          inDialog(
            capabilities,
            dialog,
            request,
            Nil,
            answeredWithoutAck = false,
            Active(_)
          )
        case (Reinviting(dialog, change), RequestReceived(request)) =>
          inDialog(
            capabilities,
            dialog,
            request,
            Nil,
            answeredWithoutAck = false,
            Reinviting(_, change),
            reinvitePending = true
          )
        case (Retrying(dialog, change), RequestReceived(request)) =>
          inDialog(
            capabilities,
            dialog,
            request,
            Nil,
            answeredWithoutAck = false,
            Retrying(_, change)
          )

        // The BYE we sent was answered, or could not be.
        case (ByeSent(_), ResponseReceived(response))
            if response.statusCode >= 200 && response.headers.cseq.method == SipMethod.Bye =>
          go(Closed, List(NotifyHangup(TerminationReason.LocalHangup)))
        case (ByeSent(_), TransactionFailed(_)) =>
          go(Closed, List(NotifyHangup(TerminationReason.LocalHangup)))

        // A CANCEL that arrives too late changes nothing, but is answered.
        case (Answered(_, _) | Active(_) | ByeSent(_), RequestReceived(request))
            if request.method == SipMethod.Cancel =>
          State.pure(List(Reply(request, 200, "OK")))

        case (
              Offered(_) | Ringing(_),
              TransactionFailed(TransactionFailure.TransportFailed)
            ) =>
          go(
            Closed,
            List(
              stopTimer(CallTimer.AnswerTimeout),
              NotifyHangup(TerminationReason.TransportFailure)
            )
          )

        // Stale timers, repeats, events after the end.
        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------

  private def accept(ctx: InboundCtx): Transition =
    go(
      Answered(ctx, ctx.config.t1),
      List(
        stopTimer(CallTimer.AnswerTimeout),
        SendInviteOk(ctx),
        StartMedia(ctx.offer),
        Timer(TimerAction.Start(CallTimer.RetransmitOk, ctx.config.t1)),
        Timer(
          TimerAction.Start(CallTimer.AckTimeout, ctx.config.transactionTimeout)
        )
      )
    )

  private def reject(
      ctx: InboundCtx,
      status: FailureStatus,
      reason: TerminationReason
  ): Transition =
    go(
      Closed,
      List(
        stopTimer(CallTimer.AnswerTimeout),
        SendInviteFailure(ctx.invite, ctx.localTag, status),
        NotifyHangup(reason)
      )
    )

  private def hangUp(
      dialog: SipDialog,
      before: List[InboundAction]
  ): Transition =
    go(ByeSent(dialog), before ::: List(StopMedia, SendBye(dialog)))

  private def stopRetransmits: List[InboundAction] =
    List(stopTimer(CallTimer.RetransmitOk), stopTimer(CallTimer.AckTimeout))

  private def stopTimer(timer: CallTimer): InboundAction =
    Timer(TimerAction.Cancel(timer))

  /** A CANCEL cancels the INVITE it names: same Call-ID (routing guarantees)
    * and the same topmost Via branch (§9.2).
    */
  private def sameTransaction(ctx: InboundCtx, cancel: SipRequest): Boolean =
    cancel.headers.topVia.branch == ctx.invite.headers.topVia.branch

  private def noSuchTransaction(request: SipRequest): InboundAction =
    Reply(
      request,
      FailureStatus.CallDoesNotExist.value,
      FailureStatus.CallDoesNotExist.reason
    )

  /** A request within the dialog once the call is answered. A BYE ends it; an
    * INFO may carry a digit; the rest we do not support. Requests must be newer
    * than the last (§12.2.2).
    */
  private def inDialog(
      capabilities: Capabilities,
      dialog: SipDialog,
      request: SipRequest,
      before: List[InboundAction],
      answeredWithoutAck: Boolean,
      rebuild: SipDialog => InboundState,
      reinvitePending: Boolean = false
  ): Transition = {
    val cseq = request.headers.cseq.seq
    if request.method == SipMethod.Cancel then
      State.pure(List(Reply(request, 200, "OK")))
    else if !dialog.matches(request) then
      State.pure(
        List(
          Reply(
            request,
            FailureStatus.CallDoesNotExist.value,
            FailureStatus.CallDoesNotExist.reason
          )
        )
      )
    else if !dialog.accepts(cseq) then
      State.pure(
        List(
          Reply(
            request,
            FailureStatus.ServerInternalError.value,
            FailureStatus.ServerInternalError.reason
          )
        )
      )
    else {
      val seen = dialog.seen(cseq)
      request.method match {
        case SipMethod.Bye =>
          go(
            Closed,
            before ::: List(
              StopMedia,
              SendByeOk(request),
              NotifyHangup(TerminationReason.RemoteBye)
            )
          )
        case SipMethod.Info if !answeredWithoutAck =>
          val digit = CallFsm.parseDtmf(request.body).map(NotifyDtmf(_)).toList
          go(rebuild(seen), SendInfoOk(request) :: digit)
        case _ if answeredWithoutAck =>
          State.pure(
            List(
              Reply(
                request,
                FailureStatus.NotImplemented.value,
                FailureStatus.NotImplemented.reason
              )
            )
          )
        case SipMethod.Invite | SipMethod.Update if reinvitePending =>
          State.pure(
            List(
              Reply(
                request,
                FailureStatus.RequestPending.value,
                FailureStatus.RequestPending.reason
              )
            )
          )
        case _ =>
          go(
            rebuild(seen),
            DialogRequest.of(request, capabilities) match {
              case DialogRequest.Query   => List(SendOptionsOk(request))
              case DialogRequest.Refresh => List(SendSessionOk(request, None))
              case DialogRequest.DelayedOffer => List(SendOfferOk(request))
              case DialogRequest.Offer(offer) =>
                List(SendSessionOk(request, Some(offer)))
              case DialogRequest.Refused(status) =>
                List(Reply(request, status.value, status.reason))
            }
          )
      }
    }
  }

  private def sendReinvite(
      dialog: SipDialog,
      change: SessionChange
  ): Transition = {
    val next = dialog.copy(localCseq = dialog.localCseq.next)
    go(Reinviting(next, change), List(SendReinvite(next, change)))
  }

  private def go(next: InboundState, actions: List[InboundAction]): Transition =
    State.set(next).as(actions)
}
