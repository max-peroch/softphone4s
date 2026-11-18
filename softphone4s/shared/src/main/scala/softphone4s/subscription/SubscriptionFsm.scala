package softphone4s.subscription

import cats.data.State
import cats.syntax.all.*
import softphone4s.fsm.{TimerAction, TransactionFailure}
import softphone4s.model.*

import scala.concurrent.duration.*

/** The subscriber side of a subscription (RFC 6665) as a pure state machine,
  * for the life of one dialog: subscribe, take the NOTIFYs, refresh at half the
  * lifetime, answer a challenge once, raise the lifetime on `423`, and end the
  * subscription on request. How it ended says whether to subscribe again
  * ([[Ending.resubscribeAfter]]); that is for the one who runs it, with a new
  * dialog.
  */
object SubscriptionFsm {
  import SubscriptionAction.*
  import SubscriptionEvent.*
  import SubscriptionFsmState.*

  type Transition = State[SubscriptionFsmState, List[SubscriptionAction]]

  /** How long the first NOTIFY is awaited after the 2xx: 64 times T1. */
  val NotifyWait: FiniteDuration = 32.seconds

  /** How long the last NOTIFY of a subscription we ended is awaited. */
  val LeaveWait: FiniteDuration = 4.seconds

  /** When to refresh a subscription with `lifetime` left. */
  def refreshAfter(lifetime: Expires): FiniteDuration = lifetime.duration / 2

  /** The wait before refresh attempt number `failures` (1, 2, ...) is repeated:
    * 5 s, doubling, up to a minute. The lifetime still runs out meanwhile.
    */
  def refreshBackoff(failures: Int): FiniteDuration =
    (5.seconds * math.pow(2, (failures - 1).max(0).min(4)).toLong).min(1.minute)

  def transition(event: SubscriptionEvent): Transition =
    State.get[SubscriptionFsmState].flatMap { state =>
      (state, event) match {

        case (Idle, Start(context)) =>
          go(
            Establishing(context, None),
            List(
              Report(SubscriptionStatus.Subscribing),
              SendSubscribe(context, context.requested, None)
            )
          )

        // ---- The first SUBSCRIBE is in flight ---------------------------------
        case (Establishing(context, early), Response(response)) =>
          established(context, early, response)

        case (Establishing(_, _), TransactionFailed(failure)) =>
          end(Ending.Failed(failureOf(failure)))

        case (Establishing(context, early), NotifyReceived(request, info)) =>
          notifiedEarly(context, request, info)

        case (Establishing(context, early), Stop) =>
          early.fold(end(Ending.Unsubscribed))(_ => unsubscribe(context))

        // ---- Accepted; the first NOTIFY is awaited ----------------------------
        case (
              AwaitingNotify(context, lifetime),
              NotifyReceived(request, info)
            ) =>
          notified(context, lifetime, request, info)(
            (c, standing, left) => Subscribed(c, standing, left),
            reschedule = true
          )

        case (AwaitingNotify(_, _), TimerFired(SubscriptionTimer.NotifyWait)) =>
          end(Ending.Failed(SubscriptionFailure.NoNotify))

        case (AwaitingNotify(context, _), Stop) => unsubscribe(context)

        // ---- Subscribed -------------------------------------------------------
        case (
              Subscribed(context, standing, lifetime),
              NotifyReceived(request, info)
            ) =>
          notified(context, lifetime, request, info)(
            (c, s, left) => Subscribed(c, s, left),
            reschedule = true
          )

        case (
              Subscribed(context, standing, lifetime),
              TimerFired(SubscriptionTimer.Refresh)
            ) =>
          val next = context.next
          go(
            Refreshing(next, standing, lifetime),
            List(SendSubscribe(next, next.requested, next.cached))
          )

        case (Subscribed(context, _, _), Stop) => unsubscribe(context)

        // ---- A refresh is in flight -------------------------------------------
        case (Refreshing(context, standing, lifetime), Response(response)) =>
          refreshed(context, standing, lifetime, response)

        case (Refreshing(context, standing, lifetime), TransactionFailed(_)) =>
          refreshFailed(context, standing, lifetime)

        case (
              Refreshing(context, standing, lifetime),
              NotifyReceived(request, info)
            ) =>
          notified(context, lifetime, request, info)(
            (c, s, left) => Refreshing(c, s, left),
            reschedule = false
          )

        case (Refreshing(context, _, _), Stop) => unsubscribe(context)

        // ---- Time runs out ----------------------------------------------------
        case (
              AwaitingNotify(_, _) | Subscribed(_, _, _) | Refreshing(_, _, _),
              TimerFired(SubscriptionTimer.Expiry)
            ) =>
          end(Ending.Expired)

        // ---- Ending it --------------------------------------------------------
        // Only the answer to the SUBSCRIBE that ends it counts. One to an earlier
        // SUBSCRIBE, still in flight when it was asked to end, is not that answer.
        case (Unsubscribing(context), Response(response)) =>
          if response.headers.cseq.seq == context.cseq then
            unsubscribed(context, response)
          else State.pure(Nil)

        case (Unsubscribing(_), TransactionFailed(_)) =>
          end(Ending.Unsubscribed)

        case (Unsubscribing(context), NotifyReceived(request, info)) =>
          lastNotify(context, request, info)(Unsubscribing(_))

        case (Leaving(context), NotifyReceived(request, info)) =>
          lastNotify(context, request, info)(Leaving(_))

        case (Leaving(_), TimerFired(SubscriptionTimer.LeaveWait)) =>
          end(Ending.Unsubscribed)

        // ---- A NOTIFY for what is not, or is no more ----------------------------
        case (Idle | Ended(_), NotifyReceived(request, _)) =>
          State.pure(List(Respond(request, 481, "Subscription Does Not Exist")))

        // Stale timers, responses after the fact, a second Stop, and so on.
        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------
  // The first SUBSCRIBE

  private def established(
      context: SubscriptionContext,
      early: Option[Early],
      response: SipResponse
  ): Transition =
    response.statusCode match {
      case code if code < 200 => State.pure(Nil)
      case code if code < 300 =>
        val lifetime = granted(context, response)
        if lifetime.isZero then
          end(Ending.Rejected(code, "granted no lifetime"))
        else {
          val known = context.copy(remoteTag = response.headers.to.tag)
          early match {
            case Some(Early(standing, left)) =>
              subscribed(known, standing, left.getOrElse(lifetime), Nil)
            case None =>
              go(
                AwaitingNotify(known, lifetime),
                List(
                  Timer(
                    TimerAction.Start(
                      SubscriptionTimer.NotifyWait,
                      NotifyWait
                    )
                  ),
                  Timer(
                    TimerAction.Start(
                      SubscriptionTimer.Expiry,
                      lifetime.duration
                    )
                  )
                )
              )
          }
        }
      case _ =>
        Challenges.of(response) match {
          case Some((kind, challenges)) =>
            retryWithAuth(
              context,
              kind,
              challenges,
              context.requested,
              response,
              Establishing(_, early)
            )
          case None => notEstablished(context, early, response)
        }
    }

  private def notEstablished(
      context: SubscriptionContext,
      early: Option[Early],
      response: SipResponse
  ): Transition =
    (response.statusCode, response.headers.minExpires) match {
      case (423, Some(minimum)) if minimum > context.requested =>
        val next = context.next.copy(requested = minimum)
        go(
          Establishing(next, early),
          List(SendSubscribe(next, minimum, next.cached))
        )
      case (code, _) => refused(code, response.reason)
    }

  /** A SUBSCRIBE refused: a status that may pass is a failure to try again
    * after, and any other is final.
    */
  private def refused(code: Int, reason: String): Transition =
    if code == 408 || code == 480 || (code >= 500 && code < 600) then
      end(Ending.Failed(SubscriptionFailure.Refused(code, reason)))
    else end(Ending.Rejected(code, reason))

  /** A NOTIFY before the 2xx: the dialog is made by it (RFC 6665 §4.4.1). */
  private def notifiedEarly(
      context: SubscriptionContext,
      request: SipRequest,
      info: NotifyInfo
  ): Transition =
    checked(context, request, info) { ctx =>
      val reply = List(
        Respond(request, 200, "OK"),
        Deliver(Notification(info.state, info.contentType, info.body))
      )
      info.state match {
        case NotifyState.Terminated(reason, retry) =>
          end(Ending.Terminated(reason, retry), reply)
        case NotifyState.Active(left) =>
          go(
            Establishing(ctx, Some(Early(Standing.Active, left))),
            reply
          )
        case NotifyState.Pending(left) =>
          go(
            Establishing(ctx, Some(Early(Standing.Pending, left))),
            reply
          )
      }
    }

  // -------------------------------------------------------------------------
  // NOTIFYs

  /** `info` accepted into `context`, or answered as it deserves: 481 for
    * another event than ours, 500 for one out of order (RFC 3261 §12.2.2).
    */
  private def checked(
      context: SubscriptionContext,
      request: SipRequest,
      info: NotifyInfo
  )(accepted: SubscriptionContext => Transition): Transition =
    if !context.event.matches(info.event) then
      State.pure(List(Respond(request, 481, "Subscription Does Not Exist")))
    else if context.remoteCseq.exists(_ >= info.cseq) then
      State.pure(List(Respond(request, 500, "Server Internal Error")))
    else accepted(context.copy(remoteCseq = Some(info.cseq)))

  /** A NOTIFY in a subscription that has begun. `next` makes the state to be
    * in, and `reschedule` says whether the timers start over (they do not while
    * a refresh is in flight, which will start them).
    */
  private def notified(
      context: SubscriptionContext,
      lifetime: Expires,
      request: SipRequest,
      info: NotifyInfo
  )(
      next: (SubscriptionContext, Standing, Expires) => SubscriptionFsmState,
      reschedule: Boolean
  ): Transition =
    checked(context, request, info) { ctx =>
      val reply = List(
        Respond(request, 200, "OK"),
        Deliver(Notification(info.state, info.contentType, info.body))
      )
      def standing(s: Standing, left: Option[Expires]): Transition = {
        val remaining = left.getOrElse(lifetime)
        if reschedule then subscribed(ctx, s, remaining, reply, next)
        else
          go(
            next(ctx, s, remaining),
            reply :+ Report(status(s, remaining))
          )
      }
      info.state match {
        case NotifyState.Terminated(reason, retry) =>
          end(Ending.Terminated(reason, retry), reply)
        case NotifyState.Active(left)  => standing(Standing.Active, left)
        case NotifyState.Pending(left) => standing(Standing.Pending, left)
      }
    }

  private def subscribed(
      context: SubscriptionContext,
      standing: Standing,
      lifetime: Expires,
      before: List[SubscriptionAction],
      next: (SubscriptionContext, Standing, Expires) => SubscriptionFsmState =
        (c, s, left) => Subscribed(c, s, left)
  ): Transition =
    go(
      next(context.copy(refreshFailures = 0), standing, lifetime),
      before ::: List(
        Report(status(standing, lifetime)),
        Timer(TimerAction.Cancel(SubscriptionTimer.NotifyWait)),
        Timer(
          TimerAction.Start(SubscriptionTimer.Refresh, refreshAfter(lifetime))
        ),
        Timer(TimerAction.Start(SubscriptionTimer.Expiry, lifetime.duration))
      )
    )

  private def status(
      standing: Standing,
      lifetime: Expires
  ): SubscriptionStatus =
    standing match {
      case Standing.Active  => SubscriptionStatus.Active(lifetime.duration)
      case Standing.Pending => SubscriptionStatus.Pending(lifetime.duration)
    }

  // -------------------------------------------------------------------------
  // Refreshing

  private def refreshed(
      context: SubscriptionContext,
      standing: Standing,
      lifetime: Expires,
      response: SipResponse
  ): Transition =
    response.statusCode match {
      case code if code < 200 => State.pure(Nil)
      case code if code < 300 =>
        val granted_ = granted(context, response)
        if granted_.isZero then
          end(Ending.Rejected(code, "granted no lifetime"))
        else subscribed(context, standing, granted_, Nil)
      case 481 => end(Ending.DialogLost)
      case _   =>
        Challenges.of(response) match {
          case Some((kind, challenges)) =>
            retryWithAuth(
              context,
              kind,
              challenges,
              context.requested,
              response,
              Refreshing(_, standing, lifetime)
            )
          case None =>
            (response.statusCode, response.headers.minExpires) match {
              case (423, Some(minimum)) if minimum > context.requested =>
                val next = context.next.copy(requested = minimum)
                go(
                  Refreshing(next, standing, lifetime),
                  List(SendSubscribe(next, minimum, next.cached))
                )
              case (code, _)
                  if code == 408 || code == 480 || (code >= 500 && code < 600) =>
                refreshFailed(context, standing, lifetime)
              case (code, _) => end(Ending.Rejected(code, response.reason))
            }
        }
    }

  /** The refresh did not get through: we stay subscribed, try again after a
    * wait, and the lifetime goes on running out.
    */
  private def refreshFailed(
      context: SubscriptionContext,
      standing: Standing,
      lifetime: Expires
  ): Transition = {
    val failed = context.copy(refreshFailures = context.refreshFailures + 1)
    go(
      Subscribed(failed, standing, lifetime),
      List(
        Timer(
          TimerAction.Start(
            SubscriptionTimer.Refresh,
            refreshBackoff(failed.refreshFailures)
          )
        )
      )
    )
  }

  // -------------------------------------------------------------------------
  // Ending it

  private def unsubscribe(context: SubscriptionContext): Transition = {
    val next = context.next
    go(
      Unsubscribing(next),
      cancelTimers ::: List(
        Report(SubscriptionStatus.Leaving),
        SendSubscribe(next, Expires.Zero, next.cached)
      )
    )
  }

  /** The response to the SUBSCRIBE that ends the subscription. A challenge is
    * answered once, as the removal of a binding is; any other answer ends it.
    */
  private def unsubscribed(
      context: SubscriptionContext,
      response: SipResponse
  ): Transition =
    response.statusCode match {
      case code if code < 200 => State.pure(Nil)
      case code if code < 300 =>
        go(
          Leaving(context),
          List(
            Timer(TimerAction.Start(SubscriptionTimer.LeaveWait, LeaveWait))
          )
        )
      case _ =>
        Challenges.of(response) match {
          case Some((kind, challenges)) =>
            val made = context.authAttempts.getOrElse(kind, 0)
            Challenges
              .once(challenges, made)
              .fold(end(Ending.Unsubscribed)) { challenge =>
                val next = context.copy(
                  cseq = context.cseq.next,
                  authAttempts = context.authAttempts.updated(kind, made + 1),
                  cached = Some((kind, challenge))
                )
                go(
                  Unsubscribing(next),
                  List(
                    SendSubscribe(next, Expires.Zero, Some((kind, challenge)))
                  )
                )
              }
          case None => end(Ending.Unsubscribed)
        }
    }

  /** A NOTIFY while we are ending the subscription: answered, and the one that
    * says `terminated` is the end.
    */
  private def lastNotify(
      context: SubscriptionContext,
      request: SipRequest,
      info: NotifyInfo
  )(stay: SubscriptionContext => SubscriptionFsmState): Transition =
    checked(context, request, info) { ctx =>
      val reply = List(Respond(request, 200, "OK"))
      if info.state.isTerminated then end(Ending.Unsubscribed, reply)
      else go(stay(ctx), reply)
    }

  // -------------------------------------------------------------------------

  /** The lifetime the notifier granted: the `Expires` of the 2xx, else what we
    * asked for (RFC 6665 §4.2.1.1).
    */
  private def granted(
      context: SubscriptionContext,
      response: SipResponse
  ): Expires = response.headers.expires.getOrElse(context.requested)

  /** A challenge is answered once; a second one means the credentials were
    * refused, unless the nonce only went stale. `inState` makes the state to
    * wait for the answer in.
    */
  private def retryWithAuth(
      context: SubscriptionContext,
      kind: AuthKind,
      challenges: List[AuthChallenge],
      lifetime: Expires,
      response: SipResponse,
      inState: SubscriptionContext => SubscriptionFsmState
  ): Transition = {
    val made = context.authAttempts.getOrElse(kind, 0)
    Challenges
      .toAnswer(challenges, made)
      .fold(end(Ending.Rejected(response.statusCode, response.reason))) {
        challenge =>
          val next = context.copy(
            cseq = context.cseq.next,
            authAttempts = context.authAttempts.updated(kind, made + 1),
            cached = Some((kind, challenge))
          )
          go(
            inState(next),
            List(SendSubscribe(next, lifetime, Some((kind, challenge))))
          )
      }
  }

  private def failureOf(failure: TransactionFailure): SubscriptionFailure =
    failure match {
      case TransactionFailure.TimedOut        => SubscriptionFailure.TimedOut
      case TransactionFailure.TransportFailed =>
        SubscriptionFailure.TransportFailed
    }

  private def cancelTimers: List[SubscriptionAction] =
    List(
      SubscriptionTimer.Refresh,
      SubscriptionTimer.Expiry,
      SubscriptionTimer.NotifyWait,
      SubscriptionTimer.LeaveWait
    ).map(t => Timer(TimerAction.Cancel(t)))

  /** Over, for `ending`: the timers stop and the program is told, after
    * `before`.
    */
  private def end(
      ending: Ending,
      before: List[SubscriptionAction] = Nil
  ): Transition =
    go(
      Ended(ending),
      (before ::: cancelTimers) :+ Report(SubscriptionStatus.Ended(ending))
    )

  private def go(
      next: SubscriptionFsmState,
      actions: List[SubscriptionAction]
  ): Transition =
    State.set(next).as(actions)
}
