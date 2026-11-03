package softphone4s.loopback

import softphone4s.SipLiterals.*
import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.port
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.metrics.*
import softphone4s.model.FailureStatus

import scala.concurrent.duration.*

/** What the metrics of each end are told of calls that end in each way. */
class CallMetricsSuite extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 2.minutes

  private val frame: Chunk[Byte] =
    Chunk.array(
      Array.tabulate[Byte](320)(i => if i % 2 == 0 then 0x40 else 0x1f)
    )

  private def both: IO[(RecordingMetrics, RecordingMetrics)] =
    (RecordingMetrics(), RecordingMetrics()).tupled

  test(
    "an answered call is counted at both ends, with its talk time and its audio"
  ) {
    for {
      (alice, bob) <- both
      _            <- Loopback
        .pair(aliceMetrics = Some(alice), bobMetrics = Some(bob))
        .use { p =>
          p.alice.call(p.callee()).use { call =>
            for {
              incoming <- p.bob.incoming.head.compile.lastOrError
                .timeout(10.seconds)
              _ <- incoming.accept.use { bobCall =>
                (call.awaitPickup, bobCall.awaitPickup).parTupled
                  .timeout(10.seconds) >>
                  Stream
                    .fixedRate[IO](20.millis)
                    .flatMap(_ => Stream.chunk(frame))
                    .through(call.sink)
                    .interruptAfter(2.seconds)
                    .compile
                    .drain
              }
            } yield ()
          }
        }
      aliceReports <- alice.reportsOf(1)
      bobReports   <- bob.reportsOf(1)
      aliceSeen    <- alice.recorded
      bobSeen      <- bob.recorded
    } yield {
      assertEquals(
        aliceSeen.headOption,
        Some(RecordingMetrics.Recorded.CallBegan(CallDirection.Outgoing))
      )
      assertEquals(
        bobSeen.headOption,
        Some(RecordingMetrics.Recorded.CallBegan(CallDirection.Incoming))
      )
      val a = aliceReports.head
      val b = bobReports.head
      assertEquals(
        (a.direction, a.outcome),
        (CallDirection.Outgoing, CallOutcome.Completed)
      )
      assertEquals(
        (b.direction, b.outcome),
        (CallDirection.Incoming, CallOutcome.Completed)
      )
      assert(a.talkTime.exists(_ >= 1.second), a.talkTime.toString)
      assert(b.talkTime.exists(_ >= 1.second), b.talkTime.toString)
      // alice spoke; bob's jitter buffer received and played what she sent
      assert(b.media.received > 50, b.media.toString)
      assert(b.media.played > 50, b.media.toString)
    }
  }

  test("a call the callee rejects is counted as rejected at both ends") {
    for {
      (alice, bob) <- both
      _            <- Loopback
        .pair(aliceMetrics = Some(alice), bobMetrics = Some(bob))
        .use { p =>
          p.alice.call(p.callee()).use { call =>
            for {
              incoming <- p.bob.incoming.head.compile.lastOrError
                .timeout(10.seconds)
              _ <- incoming.reject(FailureStatus.Decline)
              _ <- call.awaitPickup.attempt.timeout(10.seconds)
            } yield ()
          }
        }
      a <- alice.reportsOf(1)
      b <- bob.reportsOf(1)
    } yield {
      assertEquals(a.head.outcome, CallOutcome.Rejected(603))
      assertEquals(a.head.talkTime, None)
      assertEquals(b.head.direction, CallDirection.Incoming)
      assertEquals(b.head.outcome, CallOutcome.Declined)
    }
  }

  test(
    "a call cancelled before it is answered, and one nobody answers, are counted as unanswered or refused"
  ) {
    for {
      (alice, bob) <- both
      _            <- Loopback
        .pair(
          Loopback.Settings(answerTimeout = 1.second.positive),
          Some(alice),
          Some(bob)
        )
        .use { p =>
          val cancelled =
            p.alice.call(p.callee()).allocated.flatMap { (_, hangUp) =>
              p.bob.incoming.head.compile.lastOrError
                .timeout(10.seconds)
                .flatMap(_ => hangUp)
            }
          val unanswered = p.alice
            .call(p.callee())
            .use(_.awaitPickup.attempt.timeout(15.seconds).void)
          cancelled >> unanswered
        }
      a <- alice.reportsOf(2)
      b <- bob.reportsOf(2)
    } yield {
      assertEquals(
        a.map(_.outcome),
        Vector(CallOutcome.Unanswered, CallOutcome.Rejected(480))
      )
      assertEquals(
        b.map(_.outcome),
        Vector(CallOutcome.Unanswered, CallOutcome.Unanswered)
      )
      assert(a.forall(_.talkTime.isEmpty))
      assert(b.forall(_.talkTime.isEmpty))
    }
  }

  test(
    "a call to an address where nobody listens is counted once, as unanswered"
  ) {
    for {
      (alice, _) <- both
      _          <- Loopback
        .pair(
          Loopback.Settings(inviteTimeout = 2.seconds.positive),
          Some(alice),
          None
        )
        .use { p =>
          p.alice
            .call(p.callee(port = port"19429"))
            .use(_.awaitPickup.attempt.timeout(20.seconds).void)
        }
      a    <- alice.reportsOf(1)
      seen <- alice.recorded
    } yield {
      assertEquals(a.head.outcome, CallOutcome.Unanswered)
      assertEquals(a.head.talkTime, None)
      // begun once, ended once
      assertEquals(
        seen.count(_.isInstanceOf[RecordingMetrics.Recorded.CallBegan]),
        1
      )
    }
  }

  test("metrics that fail do not fail a call") {
    val failing = new Metrics[IO] {
      private def boom: IO[Unit] =
        IO.raiseError(new RuntimeException("the counter broke"))
      def callBegan(direction: CallDirection): IO[Unit] = boom
      def callEnded(report: CallReport): IO[Unit]       = boom
      def registration(
          account: softphone4s.model.AccountId,
          state: softphone4s.registration.RegistrationState
      ): IO[Unit] = boom
      def transaction(
          event: softphone4s.transaction.TransactionEvent
      ): IO[Unit] = boom
    }
    Loopback
      .pair(aliceMetrics = Some(failing), bobMetrics = Some(failing))
      .use { p =>
        p.alice.call(p.callee()).use { call =>
          for {
            incoming <- p.bob.incoming.head.compile.lastOrError
              .timeout(10.seconds)
            bobSide <- incoming.accept.use(bobCall =>
              (call.awaitPickup, bobCall.awaitPickup).parTupled
                .timeout(10.seconds)
                .attempt
            )
          } yield assertEquals(bobSide, Right(((), ())))
        }
      }
  }
}
