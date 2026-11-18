package softphone4s.metrics

import cats.effect.IO
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite
import org.typelevel.log4cats.testing.StructuredTestingLogger
import softphone4s.logging.LogFields

import scala.concurrent.duration.*

/** A metrics hook is held to the time it may take, since it is called where
  * something else is waiting.
  */
class GuardedMetricsSuite extends CatsEffectSuite {

  private def logger =
    StructuredTestingLogger.ref[IO](true, true, true, true, true)

  /** A hook that takes `takes` over each thing it is told, and says that it
    * did.
    */
  private def slow(takes: FiniteDuration, done: IO[Unit]): Metrics[IO] =
    new Metrics[IO] {
      def callBegan(direction: CallDirection): IO[Unit] =
        IO.sleep(takes) >> done
      def callEnded(report: CallReport): IO[Unit] = IO.unit
      def registration(
          account: softphone4s.model.AccountId,
          state: softphone4s.registration.RegistrationState
      ): IO[Unit] = IO.unit
      def transaction(
          event: softphone4s.transaction.TransactionEvent
      ): IO[Unit] = IO.unit
    }

  test("a hook that takes too long is cancelled at its budget, and says so") {
    TestControl.executeEmbed(
      for {
        log      <- logger
        finished <- IO.ref(false)
        guarded = Metrics.guarded(slow(1.minute, finished.set(true)), log)
        start <- IO.monotonic
        _     <- guarded.callBegan(CallDirection.Outgoing)
        took  <- IO.monotonic.map(_ - start)
        _     <- IO.sleep(
          2.minutes
        ) // long enough for it to finish, if it were not cancelled
        ran    <- finished.get
        logged <- log.logged
      } yield {
        assertEquals(took, Metrics.HookBudget)
        assert(!ran, "the hook went on after it was cancelled")
        assertEquals(
          logged.map(_.ctx.get(LogFields.Metric)),
          Vector(Some("call-began"))
        )
      }
    )
  }

  test("a hook that is quick is left alone, and logs nothing") {
    TestControl.executeEmbed(
      for {
        log      <- logger
        finished <- IO.ref(false)
        guarded = Metrics.guarded(slow(10.millis, finished.set(true)), log)
        _      <- guarded.callBegan(CallDirection.Incoming)
        ran    <- finished.get
        logged <- log.logged
      } yield {
        assert(ran)
        assertEquals(logged, Vector.empty)
      }
    )
  }

  test("a direction is spelt the same wherever it is written") {
    assertEquals(CallDirection.Outgoing.token, "outgoing")
    assertEquals(CallDirection.Incoming.token, "incoming")
  }
}
