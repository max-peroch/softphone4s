package softphone4s.registration

import cats.data.State
import cats.syntax.all.*
import softphone4s.fsm.{TimerAction, TransactionFailure}
import softphone4s.model.*

import scala.concurrent.duration.*

/** What a registration remembers between REGISTERs. */
final case class RegistrationContext(
    callId: CallId,
    localTag: Tag,
    cseq: CSeqNumber,
    contact: SipUri,
    requested: Expires,
    authAttempts: Map[AuthKind, Int] = Map.empty
) {

  /** The context for a new REGISTER: the next CSeq, and no challenge answered
    * yet (RFC 3261 §10.2: one Call-ID, a rising CSeq).
    */
  def next: RegistrationContext =
    copy(cseq = cseq.next, authAttempts = Map.empty)
}

/** Whether a REGISTER in flight sets up the binding or refreshes it. */
enum RegistrationPhase {
  case Establishing, Refreshing
}

enum RegistrationTimer {

  /** Time to refresh the binding. */
  case Refresh

  /** Time to try again after a failure. */
  case Retry
}

enum RegistrationFsmState {
  case Idle

  /** A REGISTER is in flight; `attempt` counts failures since the last success,
    * for the backoff.
    */
  case Registering(
      context: RegistrationContext,
      attempt: Int,
      phase: RegistrationPhase
  )

  case Registered(context: RegistrationContext, lifetime: Expires)

  /** The last attempt failed; the retry timer is running. */
  case Backoff(context: RegistrationContext, attempt: Int)

  /** A REGISTER with `Expires: 0` is in flight. */
  case Unregistering(context: RegistrationContext)

  case Stopped
}

enum RegistrationEvent {

  /** Begin registering with these identifiers. */
  case Start(context: RegistrationContext)
  case Response(response: SipResponse)
  case TransactionFailed(failure: TransactionFailure)
  case TimerFired(timer: RegistrationTimer)

  /** The registration is released: remove the binding and stop. */
  case Stop
}

enum RegistrationAction {

  /** Send a REGISTER for `lifetime`, answering `authorization` if present. */
  case SendRegister(
      context: RegistrationContext,
      lifetime: Expires,
      authorization: Option[(AuthKind, DigestChallenge)]
  )

  case Notify(state: RegistrationState)
  case Timer(action: TimerAction[RegistrationTimer])
}

/** Registration (RFC 3261 §10) as a pure state machine: register, refresh at
  * half the granted lifetime, retry with backoff after a failure, answer a
  * challenge once, raise the lifetime on `423 Interval Too Brief`, and remove
  * the binding on stop.
  */
object RegistrationFsm {
  import RegistrationAction.*
  import RegistrationEvent.*
  import RegistrationFsmState.*

  type Transition = State[RegistrationFsmState, List[RegistrationAction]]

  private val RefreshFraction = 2 // refresh at 1/2 of the lifetime
  private val BackoffBase     = 5.seconds
  private val BackoffCap      = 5.minutes
  private val MaxAuthAttempts = 2

  /** The wait before retry number `attempt` (1, 2, ...): doubling from 5 s,
    * capped at 5 minutes.
    */
  def backoff(attempt: Int): FiniteDuration =
    (BackoffBase * math.pow(2, (attempt - 1).max(0).min(10)).toLong)
      .min(BackoffCap)

  /** When to refresh a binding granted for `lifetime`. */
  def refreshAfter(lifetime: Expires): FiniteDuration =
    lifetime.duration / RefreshFraction.toLong

  def transition(event: RegistrationEvent): Transition =
    State.get[RegistrationFsmState].flatMap { state =>
      (state, event) match {

        case (Idle, Start(context)) =>
          go(
            Registering(context, 1, RegistrationPhase.Establishing),
            List(
              Notify(RegistrationState.Registering),
              SendRegister(context, context.requested, None)
            )
          )

        // ---- A REGISTER in flight -------------------------------------------
        case (Registering(context, attempt, phase), Response(response)) =>
          answered(context, attempt, phase, response)

        case (Registering(context, attempt, _), TransactionFailed(failure)) =>
          fail(context, attempt, failureOf(failure))

        // ---- Registered -----------------------------------------------------
        case (Registered(context, _), TimerFired(RegistrationTimer.Refresh)) =>
          val next = context.next
          go(
            Registering(next, 1, RegistrationPhase.Refreshing),
            List(SendRegister(next, next.requested, None))
          )

        // ---- Waiting to retry -----------------------------------------------
        case (Backoff(context, attempt), TimerFired(RegistrationTimer.Retry)) =>
          val next = context.next
          go(
            Registering(next, attempt + 1, RegistrationPhase.Establishing),
            List(
              Notify(RegistrationState.Registering),
              SendRegister(next, next.requested, None)
            )
          )

        // ---- Stopping -------------------------------------------------------
        case (Backoff(_, _) | Idle, Stop) =>
          go(Stopped, stopTimers :+ Notify(RegistrationState.Unregistered))

        case (Registered(context, _), Stop)     => unregister(context)
        case (Registering(context, _, _), Stop) => unregister(context)

        case (Unregistering(context), Response(response)) =>
          unregistered(context, response)

        case (Unregistering(_), TransactionFailed(_)) =>
          go(Stopped, List(Notify(RegistrationState.Unregistered)))

        // Stale timers, responses after the fact, a second Stop, and so on.
        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------

  private def answered(
      context: RegistrationContext,
      attempt: Int,
      phase: RegistrationPhase,
      response: SipResponse
  ): State[RegistrationFsmState, List[RegistrationAction]] =
    response.statusCode match {
      case code if code < 200 => State.pure(Nil)
      case code if code < 300 =>
        granted(context, response) match {
          case lifetime if lifetime.isZero =>
            fail(
              context,
              attempt,
              RegistrationFailure.Rejected(code, "granted no lifetime")
            )
          case lifetime => registered(context, lifetime)
        }
      case 401 =>
        retryWithAuth(
          context,
          attempt,
          phase,
          AuthKind.Origin,
          response.headers.wwwAuthenticate,
          context.requested
        )
      case 407 =>
        retryWithAuth(
          context,
          attempt,
          phase,
          AuthKind.Proxy,
          response.headers.proxyAuthenticate,
          context.requested
        )
      case 423 =>
        response.headers.minExpires.filter(_ > context.requested) match {
          case Some(minimum) =>
            val next = context.next.copy(requested = minimum)
            go(
              Registering(next, attempt, phase),
              List(SendRegister(next, minimum, None))
            )
          case None =>
            fail(
              context,
              attempt,
              RegistrationFailure.Rejected(423, response.reason)
            )
        }
      case code =>
        fail(
          context,
          attempt,
          RegistrationFailure.Rejected(code, response.reason)
        )
    }

  /** The lifetime the registrar granted: our Contact's `expires`, else the
    * `Expires` header, else what we asked for (RFC 3261 §10.2.4).
    */
  private def granted(
      context: RegistrationContext,
      response: SipResponse
  ): Expires =
    response.headers.contacts
      .find(_.uri == context.contact)
      .flatMap(_.expires)
      .orElse(response.headers.expires)
      .getOrElse(context.requested)

  private def registered(
      context: RegistrationContext,
      lifetime: Expires
  ): Transition =
    go(
      Registered(context, lifetime),
      List(
        Notify(RegistrationState.Registered(lifetime.duration)),
        Timer(TimerAction.Cancel(RegistrationTimer.Retry)),
        Timer(
          TimerAction.Start(RegistrationTimer.Refresh, refreshAfter(lifetime))
        )
      )
    )

  /** A challenge is answered once; a second one means the credentials were
    * refused, unless the nonce only went stale.
    */
  private def retryWithAuth(
      context: RegistrationContext,
      attempt: Int,
      phase: RegistrationPhase,
      kind: AuthKind,
      challenges: List[AuthChallenge],
      lifetime: Expires
  ): Transition = {
    val made = context.authAttempts.getOrElse(kind, 0)
    AuthChallenge
      .best(challenges)
      .filter(c =>
        made == 0 || (made < MaxAuthAttempts && c.staleness == Staleness.Stale)
      )
      .fold(fail(context, attempt, RegistrationFailure.AuthenticationFailed)) {
        challenge =>
          val next = context.copy(
            cseq = context.cseq.next,
            authAttempts = context.authAttempts.updated(kind, made + 1)
          )
          go(
            Registering(next, attempt, phase),
            List(SendRegister(next, lifetime, Some((kind, challenge))))
          )
      }
  }

  private def fail(
      context: RegistrationContext,
      attempt: Int,
      failure: RegistrationFailure
  ): Transition = {
    val wait = backoff(attempt)
    go(
      Backoff(context, attempt),
      List(
        Timer(TimerAction.Cancel(RegistrationTimer.Refresh)),
        Notify(RegistrationState.Failed(failure, wait)),
        Timer(TimerAction.Start(RegistrationTimer.Retry, wait))
      )
    )
  }

  private def failureOf(failure: TransactionFailure): RegistrationFailure =
    failure match {
      case TransactionFailure.TimedOut        => RegistrationFailure.TimedOut
      case TransactionFailure.TransportFailed =>
        RegistrationFailure.TransportFailed
    }

  private def stopTimers: List[RegistrationAction] =
    List(
      Timer(TimerAction.Cancel(RegistrationTimer.Refresh)),
      Timer(TimerAction.Cancel(RegistrationTimer.Retry))
    )

  private def unregister(context: RegistrationContext): Transition = {
    val next = context.next
    go(
      Unregistering(next),
      stopTimers ::: List(
        Notify(RegistrationState.Unregistering),
        SendRegister(next, Expires.Zero, None)
      )
    )
  }

  /** The response to the REGISTER that removes the binding. */
  private def unregistered(
      context: RegistrationContext,
      response: SipResponse
  ): Transition = {
    val done = go(Stopped, List(Notify(RegistrationState.Unregistered)))
    response.statusCode match {
      case code if code < 200 => State.pure(Nil)
      case 401                =>
        challengeAgain(
          context,
          AuthKind.Origin,
          response.headers.wwwAuthenticate,
          done
        )
      case 407 =>
        challengeAgain(
          context,
          AuthKind.Proxy,
          response.headers.proxyAuthenticate,
          done
        )
      case _ => done
    }
  }

  private def challengeAgain(
      context: RegistrationContext,
      kind: AuthKind,
      challenges: List[AuthChallenge],
      giveUp: Transition
  ): Transition = {
    val made = context.authAttempts.getOrElse(kind, 0)
    AuthChallenge
      .best(challenges)
      .filter(_ => made == 0)
      .fold(giveUp) { challenge =>
        val next = context.copy(
          cseq = context.cseq.next,
          authAttempts = context.authAttempts.updated(kind, made + 1)
        )
        go(
          Unregistering(next),
          List(SendRegister(next, Expires.Zero, Some((kind, challenge))))
        )
      }
  }

  private def go(
      next: RegistrationFsmState,
      actions: List[RegistrationAction]
  ): Transition =
    State.set(next).as(actions)
}
