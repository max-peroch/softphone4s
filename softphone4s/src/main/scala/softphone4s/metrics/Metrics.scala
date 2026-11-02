package softphone4s.metrics

import cats.MonadThrow
import cats.syntax.all.*
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.model.AccountId
import softphone4s.registration.RegistrationState
import softphone4s.transaction.TransactionEvent

/** Where the stack tells what it is doing, for whatever counts it: calls begun
  * and ended, registrations, retransmissions and timeouts of SIP transactions,
  * and the quality of the audio of each call. `softphone4s-otel4s` has an
  * implementation that makes OpenTelemetry metrics of them.
  *
  * A method that fails does not fail the stack: see [[Metrics.guarded]].
  */
trait Metrics[F[_]] {

  /** A call was made, or came in. */
  def callBegan(direction: CallDirection): F[Unit]

  /** A call has ended, and how. */
  def callEnded(report: CallReport): F[Unit]

  /** An account's registration moved to `state`. */
  def registration(account: AccountId, state: RegistrationState): F[Unit]

  /** A SIP transaction retransmitted, or timed out. */
  def transaction(event: TransactionEvent): F[Unit]
}

object Metrics {

  /** Metrics that count nothing. */
  def noop[F[_]](using F: cats.Applicative[F]): Metrics[F] =
    new Metrics[F] {
      def callBegan(direction: CallDirection): F[Unit]              = F.unit
      def callEnded(report: CallReport): F[Unit]                    = F.unit
      def registration(a: AccountId, s: RegistrationState): F[Unit] = F.unit
      def transaction(event: TransactionEvent): F[Unit]             = F.unit
    }

  /** `metrics`, with an error that one of its methods raises logged and
    * dropped, so that a counter that breaks cannot break a call.
    */
  def guarded[F[_]](metrics: Metrics[F], logger: StructuredLogger[F])(using
      F: MonadThrow[F]
  ): Metrics[F] = {
    def safely(what: String)(effect: F[Unit]): F[Unit] =
      effect.handleErrorWith(error =>
        logger.warn(
          Map(
            "metric"        -> what,
            LogFields.Error -> String.valueOf(error.getMessage)
          )
        )("A metric could not be recorded")
      )
    new Metrics[F] {
      def callBegan(direction: CallDirection): F[Unit] =
        safely("call-began")(metrics.callBegan(direction))
      def callEnded(report: CallReport): F[Unit] =
        safely("call-ended")(metrics.callEnded(report))
      def registration(a: AccountId, s: RegistrationState): F[Unit] =
        safely("registration")(metrics.registration(a, s))
      def transaction(event: TransactionEvent): F[Unit] =
        safely("transaction")(metrics.transaction(event))
    }
  }
}
