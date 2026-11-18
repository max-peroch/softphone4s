package softphone4s.publication

import cats.data.State
import cats.syntax.all.*
import softphone4s.fsm.{TimerAction, TransactionFailure}
import softphone4s.model.*
import softphone4s.subscription.SubscriptionFsm

import scala.concurrent.duration.*

/** The publisher side of event state (RFC 3903) as a pure state machine:
  * publish the state, refresh it at half its lifetime under the tag the
  * notifier gave, change it when the program has a new one, and remove it on
  * request.
  *
  * The tag changes with every 200, so each request uses the latest. A `412`
  * says the notifier no longer holds the state, and so does a lifetime that
  * runs out: the state is then published anew, with no tag. A `423` raises the
  * lifetime, a challenge is answered once, and a refresh that does not get
  * through is tried again after a wait while the lifetime runs out. A state
  * given while a request is in flight is sent once that is answered. How it
  * ended says whether to try again ([[PublicationEnding]]); that is for the one
  * who runs it.
  */
object PublicationFsm {
  import PublicationAction.*
  import PublicationEvent.*
  import PublicationState.*

  type Transition = State[PublicationState, List[PublicationAction]]

  /** When to refresh state with `lifetime` left. */
  def refreshAfter(lifetime: Expires): FiniteDuration = lifetime.duration / 2

  def transition(event: PublicationEvent): Transition =
    State.get[PublicationState].flatMap { state =>
      (state, event) match {

        case (Idle, Start(context)) =>
          go(
            Publishing(context),
            List(
              Report(PublicationStatus.Publishing),
              SendPublish(
                context,
                context.requested,
                None,
                Some(context.document),
                None
              )
            )
          )

        // ---- A request is in flight -------------------------------------------
        case (Published(_, _, _) | Ended(_) | Idle, Response(_)) =>
          State.pure(Nil)
        case (_, Response(response)) =>
          inFlight(state).fold(State.pure(Nil))(answered(state, _, response))

        case (_, TransactionFailed(failure)) =>
          inFlight(state).fold(State.pure(Nil))(
            transient(state, _, failureOf(failure))
          )

        // ---- Time ----------------------------------------------------------------
        case (
              Published(context, tag, lifetime),
              TimerFired(PublicationTimer.Refresh)
            ) =>
          if context.dirty then change(context, tag, lifetime)
          else refresh(context, tag, lifetime)

        case (
              Published(_, _, _) | Refreshing(_, _, _) | Changing(_, _, _),
              TimerFired(PublicationTimer.Expiry)
            ) =>
          inFlightOrPublished(state).fold(State.pure(Nil))(context =>
            if context.stopping then end(PublicationEnding.Removed)
            else publishAnew(context)
          )

        // ---- The program ---------------------------------------------------------
        case (Published(context, tag, lifetime), Update(document)) =>
          change(context.copy(document = document), tag, lifetime)

        case (
              Publishing(_) | Refreshing(_, _, _) | Changing(_, _, _),
              Update(document)
            ) =>
          inFlight(state).fold(State.pure(Nil))(context =>
            go(
              withContext(
                state,
                context.copy(document = document, dirty = true)
              ),
              Nil
            )
          )

        case (Idle, Stop) => end(PublicationEnding.Removed)

        case (Published(context, tag, _), Stop) => remove(context, tag)

        case (Publishing(_) | Refreshing(_, _, _) | Changing(_, _, _), Stop) =>
          inFlight(state).fold(State.pure(Nil))(context =>
            go(withContext(state, context.copy(stopping = true)), Nil)
          )

        // Stale timers, a second Stop, an Update when there is nothing to update.
        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------
  // The states a request is in flight in

  private def inFlight(state: PublicationState): Option[PublicationContext] =
    state match {
      case Publishing(context)       => Some(context)
      case Refreshing(context, _, _) => Some(context)
      case Changing(context, _, _)   => Some(context)
      case Removing(context, _)      => Some(context)
      case _                         => None
    }

  private def inFlightOrPublished(
      state: PublicationState
  ): Option[PublicationContext] =
    state match {
      case Published(context, _, _) => Some(context)
      case other                    => inFlight(other)
    }

  /** The same state with another context. */
  private def withContext(
      state: PublicationState,
      context: PublicationContext
  ): PublicationState =
    state match {
      case Publishing(_)                => Publishing(context)
      case Published(_, tag, lifetime)  => Published(context, tag, lifetime)
      case Refreshing(_, tag, lifetime) => Refreshing(context, tag, lifetime)
      case Changing(_, tag, lifetime)   => Changing(context, tag, lifetime)
      case Removing(_, tag)             => Removing(context, tag)
      case other                        => other
    }

  // -------------------------------------------------------------------------
  // The answer to a request

  private def answered(
      state: PublicationState,
      context: PublicationContext,
      response: SipResponse
  ): Transition =
    response.statusCode match {
      case code if code < 200 => State.pure(Nil)
      case code if code < 300 => accepted(state, context, response)
      case _                  => declined(state, context, response)
    }

  private def accepted(
      state: PublicationState,
      context: PublicationContext,
      response: SipResponse
  ): Transition =
    state match {
      case Removing(_, _) => end(PublicationEnding.Removed)
      case _              =>
        val lifetime = response.headers.expires.getOrElse(context.requested)
        EntityTag.of(response) match {
          case None =>
            end(
              PublicationEnding.Failed(
                PublicationFailure.BadResponse("no SIP-ETag in the 2xx")
              )
            )
          case Some(Left(reason)) =>
            end(
              PublicationEnding.Failed(PublicationFailure.BadResponse(reason))
            )
          case Some(Right(_)) if lifetime.isZero =>
            end(
              PublicationEnding.Rejected(
                response.statusCode,
                "granted no lifetime"
              )
            )
          case Some(Right(tag)) =>
            if context.stopping then remove(context, tag)
            else if context.dirty then change(context, tag, lifetime)
            else published(context, tag, lifetime)
        }
    }

  private def declined(
      state: PublicationState,
      context: PublicationContext,
      response: SipResponse
  ): Transition =
    Challenges.of(response) match {
      case Some((kind, challenges)) =>
        val made = context.authAttempts.getOrElse(kind, 0)
        Challenges
          .toAnswer(challenges, made)
          .fold(
            rejected(state, context, response.statusCode, response.reason)
          ) { challenge =>
            val next = context.copy(
              cseq = context.cseq.next,
              authAttempts = context.authAttempts.updated(kind, made + 1),
              cached = Some((kind, challenge))
            )
            resend(state, next, Some((kind, challenge)))
          }
      case None =>
        (response.statusCode, response.headers.minExpires) match {
          case (412, _) =>
            conditionFailed(state, context, response.reason)
          case (423, Some(minimum))
              if minimum > context.requested && !isRemoving(state) =>
            resend(state, context.next.copy(requested = minimum), None)
          case (code, _)
              if code == 408 || code == 480 || (code >= 500 && code < 600) =>
            transient(
              state,
              context,
              PublicationFailure.Refused(code, response.reason)
            )
          case (code, _) => rejected(state, context, code, response.reason)
        }
    }

  private def isRemoving(state: PublicationState): Boolean =
    state match {
      case Removing(_, _) => true
      case _              => false
    }

  /** The same request again, with `next`: the state stays what it was. */
  private def resend(
      state: PublicationState,
      next: PublicationContext,
      authorization: Option[(AuthKind, DigestChallenge)]
  ): Transition = {
    val auth = authorization.orElse(next.cached)
    state match {
      case Publishing(_) =>
        go(
          Publishing(next),
          List(
            SendPublish(next, next.requested, None, Some(next.document), auth)
          )
        )
      case Refreshing(_, tag, lifetime) =>
        go(
          Refreshing(next, tag, lifetime),
          List(SendPublish(next, next.requested, Some(tag), None, auth))
        )
      case Changing(_, tag, lifetime) =>
        go(
          Changing(next, tag, lifetime),
          List(
            SendPublish(
              next,
              next.requested,
              Some(tag),
              Some(next.document),
              auth
            )
          )
        )
      case Removing(_, tag) =>
        go(
          Removing(next, tag),
          List(SendPublish(next, Expires.Zero, Some(tag), None, auth))
        )
      case _ => State.pure(Nil)
    }
  }

  /** `412`: the tag is not one the notifier holds. */
  private def conditionFailed(
      state: PublicationState,
      context: PublicationContext,
      reason: String
  ): Transition =
    state match {
      case Removing(_, _)        => end(PublicationEnding.Removed)
      case Publishing(_)         => end(PublicationEnding.Rejected(412, reason))
      case _ if context.stopping => end(PublicationEnding.Removed)
      case _                     => publishAnew(context)
    }

  /** Refused for good. Something we were removing is gone either way. */
  private def rejected(
      state: PublicationState,
      context: PublicationContext,
      code: Int,
      reason: String
  ): Transition =
    if isRemoving(state) || context.stopping then end(PublicationEnding.Removed)
    else end(PublicationEnding.Rejected(code, reason))

  /** A request that did not get through, or was answered with a status that may
    * pass. The first PUBLISH is a failure to try again after; a refresh or a
    * change leaves the state published, to be tried again after a wait while
    * the lifetime runs out.
    */
  private def transient(
      state: PublicationState,
      context: PublicationContext,
      failure: PublicationFailure
  ): Transition =
    state match {
      case Removing(_, _)        => end(PublicationEnding.Removed)
      case _ if context.stopping => end(PublicationEnding.Removed)
      case Publishing(_)         => end(PublicationEnding.Failed(failure))
      case Refreshing(_, tag, lifetime) =>
        retryLater(context, tag, lifetime, owed = context.dirty)
      case Changing(_, tag, lifetime) =>
        retryLater(context, tag, lifetime, owed = true)
      case _ => State.pure(Nil)
    }

  private def retryLater(
      context: PublicationContext,
      tag: EntityTag,
      lifetime: Expires,
      owed: Boolean
  ): Transition = {
    val failed = context.copy(
      refreshFailures = context.refreshFailures + 1,
      dirty = owed
    )
    go(
      Published(failed, tag, lifetime),
      List(
        Timer(
          TimerAction.Start(
            PublicationTimer.Refresh,
            SubscriptionFsm.refreshBackoff(failed.refreshFailures)
          )
        )
      )
    )
  }

  private def failureOf(failure: TransactionFailure): PublicationFailure =
    failure match {
      case TransactionFailure.TimedOut        => PublicationFailure.TimedOut
      case TransactionFailure.TransportFailed =>
        PublicationFailure.TransportFailed
    }

  // -------------------------------------------------------------------------
  // Requests

  /** The notifier holds the state under `tag`: refresh it at half the lifetime,
    * and know it lapsed when the lifetime runs out.
    */
  private def published(
      context: PublicationContext,
      tag: EntityTag,
      lifetime: Expires
  ): Transition =
    go(
      Published(
        context.copy(refreshFailures = 0, dirty = false),
        tag,
        lifetime
      ),
      List(
        Report(PublicationStatus.Published(lifetime.duration)),
        Timer(
          TimerAction.Start(PublicationTimer.Refresh, refreshAfter(lifetime))
        ),
        Timer(TimerAction.Start(PublicationTimer.Expiry, lifetime.duration))
      )
    )

  private def refresh(
      context: PublicationContext,
      tag: EntityTag,
      lifetime: Expires
  ): Transition = {
    val next = context.next
    go(
      Refreshing(next, tag, lifetime),
      List(SendPublish(next, next.requested, Some(tag), None, next.cached))
    )
  }

  /** A PUBLISH with the document as it is now, under the tag. */
  private def change(
      context: PublicationContext,
      tag: EntityTag,
      lifetime: Expires
  ): Transition = {
    val next = context.next.copy(dirty = false)
    go(
      Changing(next, tag, lifetime),
      List(
        SendPublish(
          next,
          next.requested,
          Some(tag),
          Some(next.document),
          next.cached
        )
      )
    )
  }

  private def remove(
      context: PublicationContext,
      tag: EntityTag
  ): Transition = {
    val next = context.next
    go(
      Removing(next, tag),
      cancelTimers ::: List(
        Report(PublicationStatus.Removing),
        SendPublish(next, Expires.Zero, Some(tag), None, next.cached)
      )
    )
  }

  /** The notifier no longer holds the state: publish it all again, with no tag.
    */
  private def publishAnew(context: PublicationContext): Transition = {
    val next = context.next.copy(dirty = false, refreshFailures = 0)
    go(
      Publishing(next),
      cancelTimers ::: List(
        Report(PublicationStatus.Publishing),
        SendPublish(
          next,
          next.requested,
          None,
          Some(next.document),
          next.cached
        )
      )
    )
  }

  private def cancelTimers: List[PublicationAction] =
    List(PublicationTimer.Refresh, PublicationTimer.Expiry).map(t =>
      Timer(TimerAction.Cancel(t))
    )

  /** Over, for `ending`: the timers stop and the program is told. */
  private def end(ending: PublicationEnding): Transition =
    go(
      Ended(ending),
      cancelTimers :+ Report(PublicationStatus.Ended(ending))
    )

  private def go(
      next: PublicationState,
      actions: List[PublicationAction]
  ): Transition =
    State.set(next).as(actions)
}
