package softphone4s

import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.effect.{Async, Deferred, Ref, Resource}
import cats.syntax.all.*
import fs2.concurrent.Signal
import softphone4s.fsm.TerminationReason
import softphone4s.media.JitterStats
import softphone4s.metrics.{CallDirection, CallOutcome, CallReport, Metrics}
import softphone4s.model.AccountId
import softphone4s.registration.RegistrationState

/** Tells [[Metrics]] what happens to calls and registrations, from outside
  * them: what a call already says (`awaitPickup`, and why it ended) or a
  * registration does (`state`) is all that is needed, so the machines that run
  * them know nothing of it. It runs on the phone's own supervisor, so that it
  * outlives the call it watches and can say how it ended.
  */
private[softphone4s] final class CallObserver[F[_]: Async](
    metrics: Metrics[F],
    supervisor: Supervisor[F]
) {

  /** Counts `call` as begun, and reports it when it ends, however it ends.
    * `why` says why it did, and `media` is read then.
    */
  def observe(
      direction: CallDirection,
      call: Call[F],
      why: F[TerminationReason],
      media: F[JitterStats]
  ): F[Unit] =
    metrics.callBegan(direction) >>
      supervisor
        .supervise(
          for {
            answered <- supervisor.supervise(
              call.awaitPickup.attempt.product(Async[F].monotonic)
            )
            reason <- why
            ended  <- Async[F].monotonic
            pickup <- answered.joinWithNever
            stats  <- media
            (result, picked) = pickup
            _ <- metrics.callEnded(
              CallReport(
                direction,
                CallObserver.outcomeOf(reason, result.isRight),
                Option.when(result.isRight)(ended - picked),
                stats
              )
            )
          } yield ()
        )
        .void

  /** Reports every state of the registration of `account` that `state` goes
    * through, and that it is gone when it is released.
    *
    * It returns once it follows `state`, so acquire it before the registration
    * begins: `state` gives its current value to a new subscriber, and the ones
    * before it are lost. It is released after the registration is removed,
    * which is when `state` reports `Unregistered`; if that was not seen (the
    * registrar did not answer in time), it is reported here.
    */
  def registered(
      account: AccountId,
      state: Signal[F, RegistrationState]
  ): Resource[F, Unit] =
    Resource
      .make(
        for {
          last       <- Ref.of[F, Option[RegistrationState]](None)
          subscribed <- Deferred[F, Unit]
          fiber      <- supervisor.supervise(
            state.discrete
              .evalMap { current =>
                // `subscribed` first: a slow `Metrics` must not hold up the
                // registration. Cancelling waits for the report to end, so
                // `last` is what was reported.
                subscribed.complete(()) >>
                  Async[F].uncancelable(_ =>
                    metrics.registration(account, current) >>
                      last.set(Some(current))
                  )
              }
              .compile
              .drain
              .guarantee(subscribed.complete(()).void)
          )
          _ <- subscribed.get
        } yield (fiber, last)
      ) { (fiber, last) =>
        fiber.cancel >>
          last.get.flatMap(reported =>
            metrics
              .registration(account, RegistrationState.Unregistered)
              .unlessA(reported.contains(RegistrationState.Unregistered))
          )
      }
      .void
}

private[softphone4s] object CallObserver {

  /** How a call came out, from why it ended and whether it was ever answered.
    */
  def outcomeOf(reason: TerminationReason, answered: Boolean): CallOutcome =
    reason match {
      case TerminationReason.Rejected(status, _) => CallOutcome.Rejected(status)
      case TerminationReason.RejectedLocally     => CallOutcome.Declined
      case TerminationReason.TransportFailure    => CallOutcome.Failed
      case TerminationReason.LocalHangup | TerminationReason.RemoteBye |
          TerminationReason.TimedOut | TerminationReason.CancelledByCaller =>
        if answered then CallOutcome.Completed else CallOutcome.Unanswered
    }
}
