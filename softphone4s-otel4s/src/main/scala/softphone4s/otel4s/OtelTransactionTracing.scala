package softphone4s.otel4s

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.{SpanKind, StatusCode, Tracer}
import softphone4s.transaction.*

/** [[softphone4s.transaction.TransactionTracing]] as OpenTelemetry spans: one
  * span per SIP transaction, named `SIP <METHOD>`, from the request to the
  * final answer.
  *
  * Attributes: `sip.method`, `sip.transaction.side` (`client` or `server`),
  * `sip.call_id` and `sip.branch`; and when it ends `sip.transaction.outcome`
  * (`answered`, `responded`, `timed_out`, `send_failed`, `abandoned`) and
  * `sip.status_code` where there is one. A transaction that timed out or could
  * not send, and a request the other side refused (4xx and up), end with the
  * status `Error`.
  *
  * The spans are roots: the transaction layer runs apart from the calls that
  * use it, so a transaction is tied to its call by `sip.call_id`, which is also
  * the `call-id` field of the log lines of that call.
  */
object OtelTransactionTracing {

  def apply[F[_]: MonadCancelThrow](tracer: Tracer[F]): TransactionTracing[F] =
    info =>
      tracer
        .spanBuilder(s"SIP ${info.method.token}")
        .withSpanKind(kindOf(info.side))
        .root
        .addAttributes(
          Attribute("sip.method", info.method.token),
          Attribute("sip.transaction.side", info.side.token),
          Attribute("sip.call_id", info.callId.value),
          Attribute("sip.branch", info.branch.value)
        )
        .build
        .startUnmanaged
        .map(span =>
          new TransactionSpan[F] {
            def end(outcome: TransactionOutcome): F[Unit] =
              span.addAttributes(attributesOf(outcome)) >>
                failure(outcome)
                  .traverse_(span.setStatus(StatusCode.Error, _)) >>
                span.end
          }
        )

  private def kindOf(side: TransactionSide): SpanKind =
    side match {
      case TransactionSide.Client => SpanKind.Client
      case TransactionSide.Server => SpanKind.Server
    }

  private def attributesOf(outcome: TransactionOutcome): List[Attribute[?]] =
    outcome match {
      case TransactionOutcome.Answered(status) =>
        List(
          Attribute("sip.transaction.outcome", "answered"),
          Attribute("sip.status_code", status.toLong)
        )
      case TransactionOutcome.Responded(status) =>
        List(
          Attribute("sip.transaction.outcome", "responded"),
          Attribute("sip.status_code", status.toLong)
        )
      case TransactionOutcome.TimedOut =>
        List(Attribute("sip.transaction.outcome", "timed_out"))
      case TransactionOutcome.SendFailed =>
        List(Attribute("sip.transaction.outcome", "send_failed"))
      case TransactionOutcome.Abandoned =>
        List(Attribute("sip.transaction.outcome", "abandoned"))
    }

  /** Why the span is an error, if it is. */
  private def failure(outcome: TransactionOutcome): Option[String] =
    outcome match {
      case TransactionOutcome.TimedOut   => Some("no answer")
      case TransactionOutcome.SendFailed => Some("could not be sent")
      case TransactionOutcome.Answered(s) if s >= 400 =>
        Some(s"refused with $s")
      case _ => None
    }
}
