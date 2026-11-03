package softphone4s.transaction

import cats.effect.IO
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite
import org.typelevel.log4cats.testing.StructuredTestingLogger
import softphone4s.metrics.Metrics
import softphone4s.model.{Branch, CallId, SipMethod}

import scala.concurrent.duration.*

class TransactionTracingSuite extends CatsEffectSuite {

  private val info = TransactionInfo(
    TransactionSide.Client,
    SipMethod.Options,
    Branch("z9hG4bKabc").fold(e => sys.error(e), identity),
    CallId("call-1").fold(e => sys.error(e), identity)
  )

  private val broken = new RuntimeException("collector is down")

  private def logger =
    StructuredTestingLogger.ref[IO](true, true, true, true, true)

  test("a span that cannot begin does not fail the transaction, and says so") {
    for {
      log <- logger
      failing = new TransactionTracing[IO] {
        def began(info: TransactionInfo): IO[TransactionSpan[IO]] =
          IO.raiseError(broken)
      }
      span   <- TransactionTracing.guarded(failing, log).began(info)
      ended  <- span.end(TransactionOutcome.Answered(200)).attempt
      logged <- log.logged
    } yield {
      assertEquals(ended, Right(()))
      assertEquals(logged.map(_.ctx.get("span")), Vector(Some("begin")))
      assertEquals(logged.head.ctx.get("error"), Some("collector is down"))
    }
  }

  test("a span that cannot end does not fail the transaction, and says so") {
    for {
      log <- logger
      failing = new TransactionTracing[IO] {
        def began(info: TransactionInfo): IO[TransactionSpan[IO]] =
          IO.pure(_ => IO.raiseError(broken))
      }
      span   <- TransactionTracing.guarded(failing, log).began(info)
      ended  <- span.end(TransactionOutcome.TimedOut).attempt
      logged <- log.logged
    } yield {
      assertEquals(ended, Right(()))
      assertEquals(logged.map(_.ctx.get("span")), Vector(Some("end")))
    }
  }

  test("a span that works is passed on as it is") {
    for {
      log     <- logger
      tracing <- RecordingTracing()
      span    <- TransactionTracing.guarded(tracing, log).began(info)
      _       <- span.end(TransactionOutcome.Answered(486))
      traced  <- tracing.traced
      logged  <- log.logged
    } yield {
      assertEquals(
        traced,
        Vector(
          RecordingTracing.Traced.Began(info),
          RecordingTracing.Traced.Ended(info, TransactionOutcome.Answered(486))
        )
      )
      assertEquals(logged, Vector.empty)
    }
  }

  test(
    "a span that takes too long to begin is dropped at the budget, and the transaction has none"
  ) {
    TestControl.executeEmbed(
      for {
        log <- logger
        slowToBegin = new TransactionTracing[IO] {
          def began(info: TransactionInfo): IO[TransactionSpan[IO]] =
            IO.sleep(1.minute).as(_ => IO.unit)
        }
        start  <- IO.monotonic
        span   <- TransactionTracing.guarded(slowToBegin, log).began(info)
        took   <- IO.monotonic.map(_ - start)
        ended  <- span.end(TransactionOutcome.Answered(200)).attempt
        logged <- log.logged
      } yield {
        assertEquals(took, Metrics.HookBudget)
        assertEquals(ended, Right(()))
        assertEquals(logged.map(_.ctx.get("span")), Vector(Some("begin")))
      }
    )
  }

  test("a span that takes too long to end is dropped at the budget") {
    TestControl.executeEmbed(
      for {
        log <- logger
        slowToEnd = new TransactionTracing[IO] {
          def began(info: TransactionInfo): IO[TransactionSpan[IO]] =
            IO.pure(_ => IO.sleep(1.minute))
        }
        span   <- TransactionTracing.guarded(slowToEnd, log).began(info)
        start  <- IO.monotonic
        _      <- span.end(TransactionOutcome.TimedOut)
        took   <- IO.monotonic.map(_ - start)
        logged <- log.logged
      } yield {
        assertEquals(took, Metrics.HookBudget)
        assertEquals(logged.map(_.ctx.get("span")), Vector(Some("end")))
      }
    )
  }
}
