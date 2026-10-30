package softphone4s.runtime

import cats.effect.std.{Queue, Supervisor}
import cats.effect.{Fiber, Resource, Temporal}
import cats.syntax.all.*
import fs2.Stream
import softphone4s.fsm.TimerAction

import scala.concurrent.duration.FiniteDuration

/** One-shot named timers whose expiry is an event for a pure state machine.
  *
  * A timer that fires and one that is cancelled can cross: an `Id` already on
  * its way to [[fired]] is not recalled by `cancel`, so a state machine must
  * ignore expiries that no longer apply, as RFC 3261's do.
  */
trait Timers[F[_], Id] {

  /** Starts the timer `id`, replacing it if it is running. */
  def start(id: Id, after: FiniteDuration): F[Unit]

  def cancel(id: Id): F[Unit]

  /** Runs what a state machine asked for. */
  def run(action: TimerAction[Id]): F[Unit] = action match {
    case TimerAction.Start(id, after) => start(id, after)
    case TimerAction.Cancel(id)       => cancel(id)
  }

  /** The ids of timers as they expire; ends when the `Timers` resource closes.
    */
  def fired: Stream[F, Id]
}

object Timers {

  private final case class Running[F[_]](
      generation: Long,
      fiber: Fiber[F, Throwable, Unit]
  )

  /** Timers backed by fibers; every timer still running is cancelled when the
    * resource is released.
    */
  def resource[F[_]: Temporal, Id]: Resource[F, Timers[F, Id]] =
    for {
      supervisor <- Supervisor[F](await = false)
      queue      <- Resource.eval(Queue.unbounded[F, Id])
      state      <- Resource.eval(
        cats.effect.Ref.of[F, (Long, Map[Id, Running[F]])]((0L, Map.empty))
      )
    } yield new Timers[F, Id] {

      def start(id: Id, after: FiniteDuration): F[Unit] =
        cancel(id) >> state
          .modify { case (next, running) =>
            ((next + 1, running), next)
          }
          .flatMap { generation =>
            supervisor
              .supervise(Temporal[F].sleep(after) >> expire(id, generation))
              .flatMap(fiber =>
                state.update { case (next, running) =>
                  (next, running.updated(id, Running(generation, fiber)))
                }
              )
          }

      def cancel(id: Id): F[Unit] =
        state
          .modify { case (next, running) =>
            ((next, running - id), running.get(id))
          }
          .flatMap(_.traverse_(_.fiber.cancel))

      def fired: Stream[F, Id] = Stream.fromQueueUnterminated(queue)

      // Only the timer generation that is still registered may fire.
      private def expire(id: Id, generation: Long): F[Unit] =
        state
          .modify { case (next, running) =>
            running.get(id) match {
              case Some(r) if r.generation == generation =>
                ((next, running - id), true)
              case _ => ((next, running), false)
            }
          }
          .flatMap(current => queue.offer(id).whenA(current))
    }
}
