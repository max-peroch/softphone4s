package softphone4s

import cats.effect.std.Supervisor
import cats.effect.{Async, Resource}
import cats.syntax.all.*
import softphone4s.fsm.TerminationReason
import softphone4s.media.JitterStats
import softphone4s.metrics.{CallDirection, CallOutcome, CallReport, Metrics}
import softphone4s.model.AccountId
import softphone4s.registration.{Registration, RegistrationState}

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

  /** Reports every state `registration` of `account` goes through, and that it
    * is gone when it is released.
    */
  def registered(
      account: AccountId,
      registration: Registration[F]
  ): Resource[F, Unit] =
    Resource
      .make(
        supervisor.supervise(
          registration.state.discrete
            .evalMap(metrics.registration(account, _))
            .compile
            .drain
        )
      )(fiber =>
        fiber.cancel >>
          metrics.registration(account, RegistrationState.Unregistered)
      )
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
