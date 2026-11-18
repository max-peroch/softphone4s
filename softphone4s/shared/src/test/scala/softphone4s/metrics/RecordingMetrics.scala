package softphone4s.metrics

import cats.effect.{IO, Ref}
import softphone4s.model.AccountId
import softphone4s.registration.RegistrationState
import softphone4s.transaction.TransactionEvent

/** What a [[Metrics]] was told, in order. */
final class RecordingMetrics private (
    events: Ref[IO, Vector[RecordingMetrics.Recorded]]
) extends Metrics[IO] {
  import RecordingMetrics.Recorded.*

  def callBegan(direction: CallDirection): IO[Unit] =
    events.update(_ :+ CallBegan(direction))
  def callEnded(report: CallReport): IO[Unit] =
    events.update(_ :+ CallEnded(report))
  def registration(account: AccountId, state: RegistrationState): IO[Unit] =
    events.update(_ :+ Registration(account, state))
  def transaction(event: TransactionEvent): IO[Unit] =
    events.update(_ :+ Transaction(event))

  def recorded: IO[Vector[RecordingMetrics.Recorded]] = events.get

  def reports: IO[Vector[CallReport]] =
    recorded.map(_.collect { case CallEnded(report) => report })

  /** Waits until `count` calls have ended, and gives their reports. */
  def reportsOf(count: Int): IO[Vector[CallReport]] =
    reports.flatMap(r =>
      if r.size >= count then IO.pure(r)
      else
        IO.sleep(scala.concurrent.duration.DurationInt(50).millis) >> reportsOf(
          count
        )
    )
}

object RecordingMetrics {

  enum Recorded {
    case CallBegan(direction: CallDirection)
    case CallEnded(report: CallReport)
    case Registration(account: AccountId, state: RegistrationState)
    case Transaction(event: TransactionEvent)
  }

  def apply(): IO[RecordingMetrics] =
    Ref.of[IO, Vector[Recorded]](Vector.empty).map(new RecordingMetrics(_))
}
