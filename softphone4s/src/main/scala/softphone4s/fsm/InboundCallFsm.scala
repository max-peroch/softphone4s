package softphone4s.fsm

import cats.data.State
import cats.syntax.all.*
import softphone4s.codec.SdpCodec
import softphone4s.codec.SdpCodec.AudioOffer
import softphone4s.model.*
import softphone4s.transaction.TransactionConfig

import scala.concurrent.duration.*

/** What an incoming call knows from its INVITE. */
final case class InboundCtx(
    invite: SipRequest,
    localTag: Tag,
    offer: AudioOffer,
    dialog: SipDialog,
    config: TransactionConfig
)

enum InboundState {

  /** The INVITE is with the application; only `100 Trying` has been sent. */
  case Offered(ctx: InboundCtx)

  /** `180 Ringing` has been sent. */
  case Ringing(ctx: InboundCtx)

  /** `200 OK` is out and is repeated until the ACK; `interval` is the next
    * repeat delay.
    */
  case Answered(ctx: InboundCtx, interval: FiniteDuration)

  case Active(dialog: SipDialog)
  case ByeSent(dialog: SipDialog)
  case Closed
}

enum InboundEvent {
  case UserRinging
  case UserAccept
  case UserReject(status: FailureStatus)

  /** The application ends the call, whatever its state. */
  case HangupRequested

  /** A request within the call: CANCEL, BYE, INFO, or something we do not
    * support.
    */
  case RequestReceived(request: SipRequest)

  /** The ACK of our 200 OK. */
  case AckReceived

  /** The response to our own BYE. */
  case ResponseReceived(response: SipResponse)
  case TransactionFailed(failure: TransactionFailure)
  case TimerFired(timer: CallTimer)
}

enum InboundAction {
  case SendRinging(ctx: InboundCtx)
  case SendInviteFailure(
      invite: SipRequest,
      localTag: Tag,
      status: FailureStatus
  )

  /** Answer the INVITE with a 200 OK carrying our SDP, and remember it for
    * retransmission.
    */
  case SendInviteOk(ctx: InboundCtx)

  /** Send the 200 OK again. */
  case ResendOk

  /** A final response of our own making to `request` (CANCEL's 200, a 481 ...).
    */
  case Reply(request: SipRequest, status: Int, reason: String)
  case SendByeOk(request: SipRequest)
  case SendInfoOk(request: SipRequest)
  case SendOptionsOk(request: SipRequest)

  /** A 200 to a re-INVITE or UPDATE: with our answer to `offer` if there is
    * one, after which media goes where the offer says (or stops, on hold).
    */
  case SendSessionOk(request: SipRequest, offer: Option[AudioOffer])
  case SendBye(dialog: SipDialog)
  case StartMedia(offer: AudioOffer)
  case StopMedia

  /** The ACK came: the call is established. */
  case NotifyEstablished
  case NotifyHangup(reason: TerminationReason)
  case NotifyDtmf(digit: DtmfDigit)
  case Timer(action: TimerAction[CallTimer])
}

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
      answerTimeout: FiniteDuration
  ): (InboundState, List[InboundAction]) = {
    def turnAway(status: FailureStatus) =
      (
        Closed,
        List(
          SendInviteFailure(invite, localTag, status),
          NotifyHangup(TerminationReason.RejectedLocally)
        )
      )
    SipDialog.forInboundInvite(invite, localTag) match {
      case None         => turnAway(FailureStatus.BadRequest)
      case Some(dialog) =>
        SdpCodec.parseOffer(invite.body).filter(_.isAcceptable) match {
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

  def transition(event: InboundEvent): Transition =
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
        case (Active(dialog), HangupRequested) => hangUp(dialog, Nil)

        case (Answered(ctx, _), RequestReceived(request)) =>
          inDialog(
            ctx.dialog,
            request,
            stopRetransmits,
            answeredWithoutAck = true
          )
        case (Active(dialog), RequestReceived(request)) =>
          inDialog(dialog, request, Nil, answeredWithoutAck = false)

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
      dialog: SipDialog,
      request: SipRequest,
      before: List[InboundAction],
      answeredWithoutAck: Boolean
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
          go(Active(seen), SendInfoOk(request) :: digit)
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
        case _ =>
          go(
            Active(seen),
            DialogRequest.of(request) match {
              case DialogRequest.Query   => List(SendOptionsOk(request))
              case DialogRequest.Refresh => List(SendSessionOk(request, None))
              case DialogRequest.Offer(offer) =>
                List(SendSessionOk(request, Some(offer)))
              case DialogRequest.Refused(status) =>
                List(Reply(request, status.value, status.reason))
            }
          )
      }
    }
  }

  private def go(next: InboundState, actions: List[InboundAction]): Transition =
    State.set(next).as(actions)
}
