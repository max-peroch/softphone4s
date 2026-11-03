package softphone4s.transaction

import cats.Applicative
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.syntax.all.*
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.metrics.Metrics

/** Where the transaction layer tells of each transaction, for whatever traces
  * it: a span begins with the request (sent or received) and ends with the
  * final answer, a timeout or a send failure, not with the timers that keep the
  * transaction around after that. `softphone4s-otel4s` has an implementation
  * that makes OpenTelemetry spans of them.
  *
  * A method that fails does not fail the stack, and one that takes longer than
  * [[Metrics.HookBudget]] is cancelled: see [[TransactionTracing.guarded]].
  * They must not block: they are called on the one fiber that runs the
  * transaction layer, so a slow one would hold up every call.
  */
trait TransactionTracing[F[_]] {

  /** A transaction began. */
  def began(info: TransactionInfo): F[TransactionSpan[F]]
}

object TransactionTracing {

  /** A span that records nothing. */
  def noSpan[F[_]](using F: Applicative[F]): TransactionSpan[F] =
    _ => F.unit

  /** Tracing that traces nothing. */
  def noop[F[_]](using F: Applicative[F]): TransactionTracing[F] =
    _ => F.pure(noSpan[F])

  /** `tracing`, with an error that it raises logged and dropped, so that a span
    * that breaks cannot break a call. A transaction whose span could not begin
    * has none.
    */
  def guarded[F[_]](
      tracing: TransactionTracing[F],
      logger: StructuredLogger[F]
  )(using
      F: Temporal[F]
  ): TransactionTracing[F] = {
    def failed(what: String, error: Throwable): F[Unit] =
      logger.warn(
        Map(
          LogFields.Span  -> what,
          LogFields.Error -> String.valueOf(error.getMessage)
        )
      )("A span could not be recorded")

    def late(what: String): F[Unit] =
      logger.warn(Map(LogFields.Span -> what))(
        "A span took longer than it may, and was dropped"
      )

    info =>
      tracing
        .began(info)
        .map(span =>
          new TransactionSpan[F] {
            def end(outcome: TransactionOutcome): F[Unit] =
              span
                .end(outcome)
                .timeoutTo(Metrics.HookBudget, late("end"))
                .handleErrorWith(failed("end", _))
          }
        )
        .timeoutTo(Metrics.HookBudget, late("begin").as(noSpan[F]))
        .handleErrorWith(error => failed("begin", error).as(noSpan[F]))
  }
}
