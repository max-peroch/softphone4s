package softphone4s.fsm

import cats.data.State
import cats.syntax.all.*
import softphone4s.model.*
import softphone4s.sdp.Capabilities

// ---------------------------------------------------------------------------
// Pure state machine
// ---------------------------------------------------------------------------

object CallFsm {
  import CallState.*
  import SipAction.*
  import SipEvent.*
  import TerminationReason.*

  private val Trying           = 100
  private val Ok               = 200
  private val ErrorResponseMin = 300
  private val FinalResponseMin = 200

  type Transition = State[CallState, List[SipAction]]

  def transition(
      event: SipEvent,
      capabilities: Capabilities = Capabilities.Default
  ): Transition =
    State.get[CallState].flatMap { state =>
      (state, event) match {

        case (Inviting(_), ResponseReceived(response))
            if response.statusCode == Trying =>
          State.pure(Nil)

        // Ringing or progress. An answer in it starts the media early (RFC 3960):
        // what the other side sends before it picks up, ringback or an
        // announcement, is heard, and what we send is heard by it.
        case (Inviting(ctx), ResponseReceived(response))
            if isProgress(response) =>
          go(
            CallState.Ringing(ctx, SipDialog.fromProvisional(ctx, response)),
            earlyMedia(ctx, response)
          )

        // More progress from the same party. A fork that answers with another
        // tag is a different early dialog, and its media is not taken.
        case (CallState.Ringing(ctx, dialog), ResponseReceived(response))
            if isProgress(
              response
            ) && response.headers.to.tag == dialog.remoteTag =>
          go(CallState.Ringing(ctx, dialog), earlyMedia(ctx, response))

        // Without a Digest challenge we can answer, the 401 or 407 is final.
        case (
              Inviting(ctx),
              ResponseReceived(
                response @ Challenges.Challenged(kind, challenges)
              )
            ) =>
          answerChallenge(ctx, response, kind, challenges)

        case (Inviting(ctx), ResponseReceived(response))
            if response.statusCode == Ok =>
          handle200(ctx, response)

        case (CallState.Ringing(ctx, _), ResponseReceived(response))
            if response.statusCode == Ok =>
          handle200(ctx, response)

        case (Inviting(ctx), ResponseReceived(response))
            if response.statusCode >= ErrorResponseMin =>
          reject(response)

        case (CallState.Ringing(ctx, _), ResponseReceived(response))
            if response.statusCode >= ErrorResponseMin =>
          reject(response)

        case (
              Inviting(_) | CallState.Ringing(_, _),
              TimerFired(CallTimer.InviteTimeout) |
              TransactionFailed(TransactionFailure.TimedOut)
            ) =>
          go(
            Failed(408, "Request Timeout"),
            List(stopInviteTimer, NotifyHangup(TimedOut))
          )

        case (
              Inviting(_) | CallState.Ringing(_, _),
              TransactionFailed(TransactionFailure.TransportFailed)
            ) =>
          go(
            Failed(503, "Transport failure"),
            List(stopInviteTimer, NotifyHangup(TransportFailure))
          )

        case (Inviting(ctx), HangupRequested) =>
          cancelCall(ctx)

        case (CallState.Ringing(ctx, _), HangupRequested) =>
          cancelCall(ctx)

        case (Active(dialog), HangupRequested) =>
          go(ByeSent(dialog), List(StopMedia, SendBye(dialog)))

        case (Reinviting(dialog, _), HangupRequested) =>
          go(ByeSent(dialog), List(StopMedia, SendBye(dialog)))

        case (Retrying(dialog, _), HangupRequested) =>
          go(
            ByeSent(dialog),
            List(
              Timer(TimerAction.Cancel(CallTimer.ReinviteRetry)),
              StopMedia,
              SendBye(dialog)
            )
          )

        // ---- Our own session changes (RFC 3261 §14.1) -------------------------
        case (Active(dialog), SessionChangeRequested(change)) =>
          sendReinvite(dialog, change)

        // A digit by SIP INFO. Only when the call is quiet: during a re-INVITE
        // the dialog's CSeq is the one its answer is matched on.
        case (Active(dialog), DtmfRequested(digit, duration)) =>
          val next = dialog.copy(localCseq = dialog.localCseq.next)
          go(Active(next), List(SendInfo(next, digit, duration)))

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
                  .when(change.method == SessionMethod.Invite)(
                    SendAck(dialog, None)
                  )
                  .toList :+ ApplyAnswer(response)
              )
            case Reinvite.Outcome.Glare =>
              go(
                Retrying(dialog, change),
                List(ScheduleReinviteRetry(dialog.role))
              )
            case Reinvite.Outcome.DialogGone =>
              go(ByeSent(dialog), List(StopMedia, SendBye(dialog)))
            case Reinvite.Outcome.Refused => go(Active(dialog), Nil)
          }

        // The re-INVITE got no answer: the dialog is gone.
        case (Reinviting(dialog, _), TransactionFailed(_)) =>
          go(ByeSent(dialog), List(StopMedia, SendBye(dialog)))

        // A request that is not part of this dialog (RFC 3261 §12.2.2), or not
        // newer than the last one, is refused.
        case (Established(dialog), RequestReceived(request))
            if !dialog.matches(request) =>
          State.pure(List(SendReply(request, FailureStatus.CallDoesNotExist)))

        case (Established(dialog), RequestReceived(request))
            if !dialog.accepts(request.headers.cseq.seq) =>
          State.pure(
            List(SendReply(request, FailureStatus.ServerInternalError))
          )

        // Both sides sent a re-INVITE at once: ours stands, theirs waits
        // (RFC 3261 §14.2).
        case (Reinviting(_, _), RequestReceived(request))
            if request.method == SipMethod.Invite ||
              request.method == SipMethod.Update =>
          State.pure(List(SendReply(request, FailureStatus.RequestPending)))

        case (Established(_), RequestReceived(request))
            if request.method == SipMethod.Bye =>
          go(
            Closed(RemoteBye),
            List(NotifyHangup(RemoteBye), StopMedia, SendByeOk(request))
          )

        // The BYE's own final response, not a retransmitted 200 to the INVITE
        // and not the answer to a BYE we sent a forked branch.
        case (ByeSent(dialog), ResponseReceived(response))
            if response.statusCode >= FinalResponseMin &&
              response.headers.cseq.method == SipMethod.Bye &&
              response.headers.to.tag == dialog.remoteTag =>
          go(Closed(LocalHangup), List(NotifyHangup(LocalHangup)))

        // The BYE got no answer (Timer F) or could not be sent.
        case (ByeSent(_), TransactionFailed(_)) =>
          go(Closed(LocalHangup), List(NotifyHangup(LocalHangup)))

        // Another 200 to an INVITE once the call stands: our ACK was lost, or
        // another branch of a forked call answered as well.
        case (Established(dialog), ResponseReceived(response))
            if isInviteOk(response) =>
          lateOk(dialog, response)

        case (ByeSent(dialog), ResponseReceived(response))
            if isInviteOk(response) =>
          lateOk(dialog, response)

        case (Established(dialog), RequestReceived(request))
            if request.method == SipMethod.Info =>
          val notify = parseDtmf(request.body).map(NotifyDtmf(_)).toList
          go(
            withDialog(state, dialog.seen(request.headers.cseq.seq)),
            SendInfoOk(request) :: notify
          )

        // OPTIONS, re-INVITE, UPDATE, or something we do not handle.
        case (Established(dialog), RequestReceived(request)) =>
          go(
            withDialog(state, dialog.seen(request.headers.cseq.seq)),
            DialogRequest.of(request, capabilities) match {
              case DialogRequest.Query   => List(SendOptionsOk(request))
              case DialogRequest.Refresh => List(SendSessionOk(request, None))
              case DialogRequest.DelayedOffer => List(SendOfferOk(request))
              case DialogRequest.Offer(offer) =>
                List(SendSessionOk(request, Some(offer)))
              case DialogRequest.Refused(status) =>
                List(SendReply(request, status))
            }
          )

        // 200 OK after CANCEL (RFC 3261 §9 race) — must ACK then BYE immediately
        case (Closed(LocalHangup), ResponseReceived(response))
            if isInviteOk(response) =>
          State.pure(
            SipDialog
              .fromLateOk(response)
              .toList
              .flatMap(dialog => List(SendAck(dialog, None), SendBye(dialog)))
          )

        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------

  /** The dialog of a call that stands, whether or not a re-INVITE is pending.
    */
  private object Established {
    def unapply(state: CallState): Option[SipDialog] = state match {
      case Active(dialog)        => Some(dialog)
      case Reinviting(dialog, _) => Some(dialog)
      case Retrying(dialog, _)   => Some(dialog)
      case _                     => None
    }
  }

  private def withDialog(state: CallState, dialog: SipDialog): CallState =
    state match {
      case Reinviting(_, change) => Reinviting(dialog, change)
      case Retrying(_, change)   => Retrying(dialog, change)
      case _                     => Active(dialog)
    }

  private def sendReinvite(
      dialog: SipDialog,
      change: SessionChange
  ): Transition = {
    val next = dialog.copy(localCseq = dialog.localCseq.next)
    go(Reinviting(next, change), List(SendReinvite(next, change)))
  }

  /** A 200 to an INVITE that arrives after the call was answered. From the
    * branch we are talking to, it is a retransmission: our ACK was lost, so
    * send it again (RFC 3261 §13.2.2.4), numbered as the INVITE the 200
    * answers. From any other branch of a forked call (§13.2.2.4, §19.3) there
    * is no call to keep: ACK it so it stops, then end that dialog with a BYE.
    */
  private def lateOk(dialog: SipDialog, response: SipResponse): Transition =
    if response.headers.to.tag == dialog.remoteTag then
      State.pure(
        List(
          SendAck(dialog.copy(localCseq = response.headers.cseq.seq), None)
        )
      )
    else
      State.pure(
        SipDialog
          .fromLateOk(response)
          .toList
          .flatMap(fork => List(SendAck(fork, None), SendBye(fork)))
      )

  private def go(next: CallState, actions: List[SipAction]): Transition =
    State.set(next).as(actions)

  private val stopInviteTimer: SipAction =
    Timer(TimerAction.Cancel(CallTimer.InviteTimeout))

  private def cancelCall(ctx: InviteCtx): Transition =
    go(
      Closed(LocalHangup),
      List(stopInviteTimer, SendCancel(ctx), NotifyHangup(LocalHangup))
    )

  /** A 200 OK answering an INVITE (not the CANCEL or BYE, which also get 200).
    */
  private def isInviteOk(response: SipResponse): Boolean =
    response.statusCode == Ok && response.headers.cseq.method == SipMethod.Invite

  // The transaction layer has already ACKed the failure (RFC 3261 §17.1.1.3).
  private def reject(response: SipResponse): Transition =
    go(
      Failed(response.statusCode, response.reason),
      List(
        stopInviteTimer,
        NotifyHangup(Rejected(response.statusCode, response.reason))
      )
    )

  private def attempts(ctx: InviteCtx, kind: AuthKind): Int =
    ctx.authAttempts.getOrElse(kind, 0)

  private def answerChallenge(
      ctx: InviteCtx,
      response: SipResponse,
      kind: AuthKind,
      challenges: List[AuthChallenge]
  ): Transition =
    Challenges
      .toAnswer(challenges, attempts(ctx, kind))
      .fold(reject(response)) { challenge =>
        val newCtx = ctx.copy(
          cseq = ctx.cseq.next,
          authAttempts = ctx.authAttempts.updated(kind, attempts(ctx, kind) + 1)
        )
        go(
          Inviting(newCtx),
          List(
            RetryInviteWithAuth(newCtx, kind, challenge),
            // The retry is a new INVITE transaction, so Timer B starts over.
            Timer(TimerAction.Start(CallTimer.InviteTimeout, ctx.inviteTimeout))
          )
        )
      }

  /** 180, 183 and the other provisional responses that are not 100. */
  private def isProgress(response: SipResponse): Boolean =
    response.statusCode > Trying && response.statusCode < FinalResponseMin

  /** Starting the media on an answer that came before the 200, if there is one:
    * only when we made the offer, and in an SDP.
    */
  private def earlyMedia(
      ctx: InviteCtx,
      response: SipResponse
  ): List[SipAction] =
    Option
      .when(
        ctx.offerInInvite && response.body.nonEmpty &&
          response.headers.contentType.forall(_.isSdp)
      )(StartMedia(response.body))
      .toList

  private def handle200(ctx: InviteCtx, response: SipResponse): Transition = {
    val dialog             = SipDialog.fromOk(ctx, response)
    val remoteSdpForAnswer = Option.unless(ctx.offerInInvite)(response.body)
    go(
      Active(dialog),
      List(
        stopInviteTimer,
        SendAck(dialog, remoteSdpForAnswer),
        StartMedia(response.body),
        NotifyAnswered
      )
    )
  }

  /** The digit in the body of an INFO: `Signal=5` of a dtmf-relay, where the
    * signal is a symbol (`5`, `*`, `#`, `A`) or an event number (`10`), or a
    * body that is only the symbol.
    */
  private[fsm] def parseDtmf(body: String): Option[DtmfDigit] =
    body.linesIterator
      .find(_.startsWith("Signal="))
      .map(_.stripPrefix("Signal=").trim)
      .orElse(Some(body.trim))
      .flatMap(signal =>
        if signal.length == 1 then DtmfDigit.fromSymbol(signal.head)
        else signal.toIntOption.flatMap(DtmfDigit.fromCode)
      )
}
