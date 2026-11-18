package softphone4s.transaction

import cats.effect.{IO, Ref}

/** A [[TransactionTracing]] that writes down what it is told, in order. */
final class RecordingTracing private (
    log: Ref[IO, Vector[RecordingTracing.Traced]]
) extends TransactionTracing[IO] {

  import RecordingTracing.Traced

  def began(info: TransactionInfo): IO[TransactionSpan[IO]] =
    log
      .update(_ :+ Traced.Began(info))
      .as(outcome => log.update(_ :+ Traced.Ended(info, outcome)))

  def traced: IO[Vector[Traced]] = log.get

  def ended(
      side: TransactionSide
  ): IO[Vector[(TransactionInfo, TransactionOutcome)]] =
    traced.map(_.collect {
      case Traced.Ended(info, outcome) if info.side == side => (info, outcome)
    })

  def began(side: TransactionSide): IO[Vector[TransactionInfo]] =
    traced.map(_.collect {
      case Traced.Began(info) if info.side == side => info
    })
}

object RecordingTracing {

  enum Traced {
    case Began(info: TransactionInfo)
    case Ended(info: TransactionInfo, outcome: TransactionOutcome)
  }

  def apply(): IO[RecordingTracing] =
    Ref.of[IO, Vector[Traced]](Vector.empty).map(new RecordingTracing(_))
}
