package softphone4s.otel4s

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.metrics.{BucketBoundaries, Meter}
import softphone4s.metrics.*
import softphone4s.model.{AccountId, SipMethod}
import softphone4s.registration.RegistrationState
import softphone4s.transaction.{TransactionEvent, TransactionSide}

import scala.concurrent.duration.FiniteDuration

/** [[Metrics]] as OpenTelemetry metrics. What is measured, with its attributes:
  *
  *   - `softphone4s.calls` (counter): calls ended, by `direction` and `outcome`
  *     (`completed`, `rejected`, `declined`, `unanswered`, `failed`), and the
  *     `status` of a rejection.
  *   - `softphone4s.calls.active` (up-down counter): calls in progress, by
  *     `direction`.
  *   - `softphone4s.call.duration` (histogram, seconds): from the answer to the
  *     end of a call that was answered, by `direction`.
  *   - `softphone4s.registrations.active` (up-down counter): accounts that are
  *     registered now.
  *   - `softphone4s.registration.changes` (counter): by `state` entered.
  *   - `softphone4s.sip.retransmissions` and `softphone4s.sip.timeouts`
  *     (counters): by `side` (`client`, `server`) and SIP `method`.
  *   - `softphone4s.rtp.frames` (counter): what the jitter buffers did, by
  *     `event` (`received`, `played`, `lost`, `late`, `duplicate`, `reordered`,
  *     `overflow`, `underrun`).
  *
  * The attributes are of low cardinality: nothing in them identifies a call, an
  * account or an address.
  */
object OtelMetrics {

  private val DurationBoundaries: BucketBoundaries =
    BucketBoundaries(1.0, 5.0, 10.0, 30.0, 60.0, 120.0, 300.0, 600.0, 1800.0,
      3600.0)

  def apply[F[_]: Async](meter: Meter[F]): F[Metrics[F]] =
    for {
      calls <- meter
        .counter[Long]("softphone4s.calls")
        .withUnit("{call}")
        .withDescription("Calls that have ended, by direction and outcome")
        .create
      active <- meter
        .upDownCounter[Long]("softphone4s.calls.active")
        .withUnit("{call}")
        .withDescription("Calls in progress")
        .create
      duration <- meter
        .histogram[Double]("softphone4s.call.duration")
        .withUnit("s")
        .withDescription(
          "From the answer to the end of a call that was answered"
        )
        .withExplicitBucketBoundaries(DurationBoundaries)
        .create
      registered <- meter
        .upDownCounter[Long]("softphone4s.registrations.active")
        .withUnit("{account}")
        .withDescription("Accounts that are registered")
        .create
      changes <- meter
        .counter[Long]("softphone4s.registration.changes")
        .withUnit("{change}")
        .withDescription("Registration states entered")
        .create
      retransmissions <- meter
        .counter[Long]("softphone4s.sip.retransmissions")
        .withUnit("{message}")
        .withDescription(
          "Requests or responses sent again for want of an answer"
        )
        .create
      timeouts <- meter
        .counter[Long]("softphone4s.sip.timeouts")
        .withUnit("{transaction}")
        .withDescription("Transactions that ended without what they waited for")
        .create
      frames <- meter
        .counter[Long]("softphone4s.rtp.frames")
        .withUnit("{frame}")
        .withDescription(
          "What the jitter buffers did with the audio of ended calls"
        )
        .create
      held <- Ref.of[F, Set[AccountId]](Set.empty)
    } yield new Metrics[F] {

      private def direction(d: CallDirection): Attribute[String] =
        Attribute("direction", d.token)

      def callBegan(d: CallDirection): F[Unit] =
        active.add(1L, direction(d))

      def callEnded(report: CallReport): F[Unit] = {
        val outcome = report.outcome match {
          case CallOutcome.Completed => List(Attribute("outcome", "completed"))
          case CallOutcome.Rejected(s) =>
            List(
              Attribute("outcome", "rejected"),
              Attribute("status", s.toLong)
            )
          case CallOutcome.Declined   => List(Attribute("outcome", "declined"))
          case CallOutcome.Unanswered =>
            List(Attribute("outcome", "unanswered"))
          case CallOutcome.Failed => List(Attribute("outcome", "failed"))
        }
        val m      = report.media
        val events = List(
          "received"  -> m.received,
          "played"    -> m.played,
          "lost"      -> m.lost,
          "late"      -> m.late,
          "duplicate" -> m.duplicates,
          "reordered" -> m.reordered,
          "overflow"  -> m.overflows,
          "underrun"  -> m.underruns
        )
        active.add(-1L, direction(report.direction)) >>
          calls.add(1L, direction(report.direction) :: outcome) >>
          report.talkTime.traverse_(t =>
            duration.record(seconds(t), direction(report.direction))
          ) >>
          events.traverse_((event, n) =>
            frames.add(n, Attribute("event", event)).whenA(n > 0)
          )
      }

      def registration(
          account: AccountId,
          state: RegistrationState
      ): F[Unit] = {
        val now = state match {
          case RegistrationState.Registered(_) => true
          case _                               => false
        }
        val name = state match {
          case RegistrationState.Unregistered  => "unregistered"
          case RegistrationState.Registering   => "registering"
          case RegistrationState.Registered(_) => "registered"
          case RegistrationState.Failed(_, _)  => "failed"
          case RegistrationState.Unregistering => "unregistering"
        }
        held
          .modify(set =>
            (
              if now then set + account else set - account,
              set.contains(account)
            )
          )
          .flatMap(was =>
            changes.add(1L, Attribute("state", name)) >>
              (if now && !was then registered.add(1L)
               else if !now && was then registered.add(-1L)
               else Async[F].unit)
          )
      }

      def transaction(event: TransactionEvent): F[Unit] =
        event match {
          case TransactionEvent.Retransmitted(side, method) =>
            retransmissions.add(1L, attributes(side, method))
          case TransactionEvent.TimedOut(side, method) =>
            timeouts.add(1L, attributes(side, method))
        }
    }

  private def seconds(d: FiniteDuration): Double = d.toMillis / 1000.0

  private def attributes(
      side: TransactionSide,
      method: SipMethod
  ): List[Attribute[String]] =
    List(
      Attribute("side", side.token),
      Attribute("method", method.token)
    )
}
