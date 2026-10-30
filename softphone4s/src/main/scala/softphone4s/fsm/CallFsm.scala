package softphone4s.fsm

import cats.data.State
import cats.syntax.all.*
import softphone4s.model.*

import scala.concurrent.duration.*

// ---------------------------------------------------------------------------
// Call state
// ---------------------------------------------------------------------------

sealed trait TerminationReason
object TerminationReason {
  case object LocalHangup                        extends TerminationReason
  case object RemoteBye                          extends TerminationReason
  case class Rejected(code: Int, reason: String) extends TerminationReason
  case object TimedOut                           extends TerminationReason
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
  // RFC 3261 §17.1.1.3 — transaction-layer ACK for non-2xx INVITE responses
  case class SendAckForNon2xx(ctx: InviteCtx, response: SipResponse)
      extends SipAction
  case class SendCancel(ctx: InviteCtx) extends SipAction
  case class SendBye(dialog: SipDialog) extends SipAction
  // Responses the interpreter builds deterministically
  case class SendByeOk(request: SipRequest)  extends SipAction
  case class SendInfoOk(request: SipRequest) extends SipAction
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

  /** RFC 3261 Timer F: 64 * T1, with T1 = 500 ms. */
  val ByeTimeoutAfter: FiniteDuration = 32.seconds

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
          reject(ctx, response)

        case (CallState.Ringing(ctx, _), ResponseReceived(response))
            if response.statusCode >= ErrorResponseMin =>
          reject(ctx, response)

        case (
              Inviting(_) | CallState.Ringing(_, _),
              TimerFired(CallTimer.InviteTimeout)
            ) =>
          go(Failed(408, "Request Timeout"), List(NotifyHangup(TimedOut)))

        case (Inviting(ctx), HangupRequested) =>
          cancelCall(ctx)

        case (CallState.Ringing(ctx, _), HangupRequested) =>
          cancelCall(ctx)

        case (Active(dialog), HangupRequested) =>
          go(
            ByeSent(dialog),
            List(
              StopMedia,
              SendBye(dialog),
              Timer(TimerAction.Start(CallTimer.ByeTimeout, ByeTimeoutAfter))
            )
          )

        case (Active(dialog), RequestReceived(request))
            if request.method == SipMethod.Bye =>
          go(
            Closed(RemoteBye),
            List(NotifyHangup(RemoteBye), StopMedia, SendByeOk(request))
          )

        case (ByeSent(_), ResponseReceived(response))
            if response.statusCode >= FinalResponseMin =>
          go(
            Closed(LocalHangup),
            List(
              Timer(TimerAction.Cancel(CallTimer.ByeTimeout)),
              NotifyHangup(LocalHangup)
            )
          )

        case (ByeSent(_), TimerFired(CallTimer.ByeTimeout)) =>
          go(Closed(LocalHangup), List(NotifyHangup(LocalHangup)))

        case (Active(_), RequestReceived(request))
            if request.method == SipMethod.Info =>
          val notify = parseDtmf(request.body).map(NotifyDtmf(_)).toList
          State.pure(SendInfoOk(request) :: notify)

        // 200 OK after CANCEL (RFC 3261 §9 race) — must ACK then BYE immediately
        case (Closed(LocalHangup), ResponseReceived(response))
            if response.statusCode == Ok =>
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

  private def reject(ctx: InviteCtx, response: SipResponse): Transition =
    go(
      Failed(response.statusCode, response.reason),
      List(
        stopInviteTimer,
        SendAckForNon2xx(ctx, response),
        NotifyHangup(Rejected(response.statusCode, response.reason))
      )
    )

  private def answerChallenge(
      ctx: InviteCtx,
      response: SipResponse,
      kind: AuthKind,
      challenges: List[AuthChallenge]
  ): Transition =
    challenges
      .collectFirst {
        case AuthChallenge.Digest(c) if c.isAnswerable => c
      }
      .fold(reject(ctx, response)) { challenge =>
        val newCtx = ctx.copy(cseq = ctx.cseq.next)
        go(
          Inviting(newCtx),
          List(
            SendAckForNon2xx(ctx, response),
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

  private def parseDtmf(body: String): Option[DtmfDigit] =
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
