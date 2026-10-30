package softphone4s.fsm

import cats.data.State
import cats.syntax.all.*
import softphone4s.model.*

// ---------------------------------------------------------------------------
// Call state
// ---------------------------------------------------------------------------

sealed trait TerminationReason
object TerminationReason {
  case object LocalHangup                        extends TerminationReason
  case object RemoteBye                          extends TerminationReason
  case class Rejected(code: Int, reason: String) extends TerminationReason
  case object TimedOut                           extends TerminationReason
  case object TransportFailure                   extends TerminationReason

  /** The caller cancelled an incoming call before it was answered. */
  case object CancelledByCaller extends TerminationReason

  /** We turned an incoming call away. */
  case object RejectedLocally extends TerminationReason
}

sealed trait CallState
object CallState {
  case object Idle                                      extends CallState
  case class Inviting(ctx: InviteCtx)                   extends CallState
  case class Ringing(ctx: InviteCtx, dialog: SipDialog) extends CallState
  case class Active(dialog: SipDialog)                  extends CallState
  case class ByeSent(dialog: SipDialog)                 extends CallState
  case class Closed(reason: TerminationReason)          extends CallState
  case class Failed(code: Int, reason: String)          extends CallState
}

// ---------------------------------------------------------------------------
// Events driving the FSM
// ---------------------------------------------------------------------------

sealed trait SipEvent
object SipEvent {
  case class ResponseReceived(response: SipResponse) extends SipEvent
  case class RequestReceived(request: SipRequest)    extends SipEvent
  case object HangupRequested                        extends SipEvent
  case class TimerFired(timer: CallTimer)            extends SipEvent

  /** A client transaction of the call ended without a final response. */
  case class TransactionFailed(failure: TransactionFailure) extends SipEvent
}

// ---------------------------------------------------------------------------
// Semantic actions — interpreted by the effect layer, never by the FSM itself
// ---------------------------------------------------------------------------

sealed trait SipAction
object SipAction {
  // Requests the interpreter must build (need random Via branches)
  case class SendInvite(
      callee: SipUri,
      callId: CallId,
      localTag: Tag,
      cseq: CSeqNumber,
      authHeader: Option[ExtensionHeader] = None
  ) extends SipAction
  case class RetryInviteWithAuth(
      ctx: InviteCtx,
      kind: AuthKind,
      challenge: DigestChallenge
  ) extends SipAction
  case class SendAck(
      dialog: SipDialog,
      remoteSdpForAnswer: Option[String] // Some(body) when offer was in 200 OK
  ) extends SipAction
  case class SendCancel(ctx: InviteCtx) extends SipAction
  case class SendBye(dialog: SipDialog) extends SipAction
  // Responses the interpreter builds deterministically
  case class SendByeOk(request: SipRequest)  extends SipAction
  case class SendInfoOk(request: SipRequest) extends SipAction

  /** A final failure response to a request within the dialog. */
  case class SendReply(request: SipRequest, status: FailureStatus)
      extends SipAction
  // Media
  case class StartMedia(sdpBody: String) extends SipAction
  case object StopMedia                  extends SipAction
  // Notifications
  case object NotifyAnswered                         extends SipAction
  case class NotifyHangup(reason: TerminationReason) extends SipAction
  case class NotifyDtmf(digit: DtmfDigit)            extends SipAction
  // Timers
  case class Timer(action: TimerAction[CallTimer]) extends SipAction
}

// ---------------------------------------------------------------------------
// Pure state machine
// ---------------------------------------------------------------------------

object CallFsm {
  import CallState.*
  import SipAction.*
  import SipEvent.*
  import TerminationReason.*

  /** One answer, plus one more if the server reports a stale nonce. */
  private val MaxAuthAttempts = 2

  private val Trying            = 100
  private val Ringing           = 180
  private val SessionProgress   = 183
  private val Ok                = 200
  private val Unauthorized      = 401
  private val ProxyAuthRequired = 407
  private val ErrorResponseMin  = 300
  private val FinalResponseMin  = 200
  private val StarDigitCode     = 10
  private val HashDigitCode     = 11

  type Transition = State[CallState, List[SipAction]]

  def transition(event: SipEvent): Transition =
    State.get[CallState].flatMap { state =>
      (state, event) match {

        case (Inviting(_), ResponseReceived(response))
            if response.statusCode == Trying =>
          State.pure(Nil)

        case (Inviting(ctx), ResponseReceived(response))
            if response.statusCode == Ringing || response.statusCode == SessionProgress =>
          go(
            CallState.Ringing(ctx, SipDialog.fromProvisional(ctx, response)),
            Nil
          )

        // Without a Digest challenge we can answer, the 401 or 407 is final.
        case (Inviting(ctx), ResponseReceived(response))
            if response.statusCode == Unauthorized =>
          answerChallenge(
            ctx,
            response,
            AuthKind.Origin,
            response.headers.wwwAuthenticate
          )

        case (Inviting(ctx), ResponseReceived(response))
            if response.statusCode == ProxyAuthRequired =>
          answerChallenge(
            ctx,
            response,
            AuthKind.Proxy,
            response.headers.proxyAuthenticate
          )

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

        // A request that is not part of this dialog (RFC 3261 §12.2.2), or not
        // newer than the last one, is refused.
        case (Active(dialog), RequestReceived(request))
            if !dialog.matches(request) =>
          State.pure(List(SendReply(request, FailureStatus.CallDoesNotExist)))

        case (Active(dialog), RequestReceived(request))
            if !dialog.accepts(request.headers.cseq.seq) =>
          State.pure(
            List(SendReply(request, FailureStatus.ServerInternalError))
          )

        case (Active(dialog), RequestReceived(request))
            if request.method == SipMethod.Bye =>
          go(
            Closed(RemoteBye),
            List(NotifyHangup(RemoteBye), StopMedia, SendByeOk(request))
          )

        // The BYE's own final response, not a retransmitted 200 to the INVITE.
        case (ByeSent(_), ResponseReceived(response))
            if response.statusCode >= FinalResponseMin &&
              response.headers.cseq.method == SipMethod.Bye =>
          go(Closed(LocalHangup), List(NotifyHangup(LocalHangup)))

        // The BYE got no answer (Timer F) or could not be sent.
        case (ByeSent(_), TransactionFailed(_)) =>
          go(Closed(LocalHangup), List(NotifyHangup(LocalHangup)))

        // A retransmitted 200 to the INVITE means our ACK was lost: send it
        // again (RFC 3261 §13.2.2.4), for as long as the dialog stands.
        case (Active(dialog), ResponseReceived(response))
            if isInviteOk(response) =>
          State.pure(List(SendAck(dialog, None)))

        case (ByeSent(dialog), ResponseReceived(response))
            if isInviteOk(response) =>
          State.pure(List(SendAck(dialog, None)))

        case (Active(dialog), RequestReceived(request))
            if request.method == SipMethod.Info =>
          val notify = parseDtmf(request.body).map(NotifyDtmf(_)).toList
          go(
            Active(dialog.seen(request.headers.cseq.seq)),
            SendInfoOk(request) :: notify
          )

        // A re-INVITE, UPDATE or anything else we do not handle yet.
        case (Active(dialog), RequestReceived(request)) =>
          go(
            Active(dialog.seen(request.headers.cseq.seq)),
            List(SendReply(request, FailureStatus.NotImplemented))
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

  /** We answer a challenge once. A second one means the credentials were
    * refused, so it is final, unless the server says the nonce only expired,
    * which earns one more try (RFC 3261 §22.2, RFC 2617 §3.2.1).
    */
  private def mayAnswer(
      ctx: InviteCtx,
      kind: AuthKind,
      challenge: DigestChallenge
  ): Boolean = {
    val made = attempts(ctx, kind)
    made == 0 || (made < MaxAuthAttempts && challenge.staleness == Staleness.Stale)
  }

  private def answerChallenge(
      ctx: InviteCtx,
      response: SipResponse,
      kind: AuthKind,
      challenges: List[AuthChallenge]
  ): Transition =
    AuthChallenge
      .best(challenges)
      .filter(challenge => mayAnswer(ctx, kind, challenge))
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

  private[fsm] def parseDtmf(body: String): Option[DtmfDigit] =
    parseDtmfFromRelay(body)
      .orElse(parseDtmfFromBare(body))
      .flatMap(DtmfDigit.fromCode)

  private def parseDtmfFromRelay(body: String): Option[Int] =
    body.linesIterator
      .find(_.startsWith("Signal="))
      .flatMap(_.stripPrefix("Signal=").trim.toIntOption)

  private def parseDtmfFromBare(body: String): Option[Int] =
    body.trim match {
      case s if s.length == 1 => charToDigitCode(s.head)
      case _                  => None
    }

  private def charToDigitCode(c: Char): Option[Int] =
    if c >= '0' && c <= '9' then Some(c - '0')
    else if c == '*' then Some(StarDigitCode)
    else if c == '#' then Some(HashDigitCode)
    else None
}
