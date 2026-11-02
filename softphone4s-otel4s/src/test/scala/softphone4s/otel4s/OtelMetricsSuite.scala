package softphone4s.otel4s

import cats.effect.IO
import io.opentelemetry.sdk.metrics.data.MetricData
import munit.CatsEffectSuite
import org.typelevel.otel4s.oteljava.testkit.metrics.MetricsTestkit
import softphone4s.media.JitterStats
import softphone4s.metrics.*
import softphone4s.model.{AccountId, SipMethod, SipUri}
import softphone4s.registration.{RegistrationFailure, RegistrationState}
import softphone4s.transaction.{TransactionEvent, TransactionSide}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class OtelMetricsSuite extends CatsEffectSuite {

  private val account = AccountId(
    SipUri.parse("sip:alice@example.com").fold(e => fail(e), identity)
  )

  /** The metrics, and what the SDK has collected of them so far. */
  private def measured[A](
      use: (Metrics[IO], IO[Map[String, MetricData]]) => IO[A]
  ): IO[A] =
    MetricsTestkit.inMemory[IO]().use { kit =>
      for {
        meter   <- kit.meterProvider.get("softphone4s")
        metrics <- OtelMetrics[IO](meter)
        a       <- use(
          metrics,
          kit.collectMetrics.map(_.map(m => m.getName -> m).toMap)
        )
      } yield a
    }

  /** The points of a counter or up-down counter: its attributes and its value.
    */
  private def points(
      metric: Option[MetricData]
  ): Map[Map[String, String], Long] =
    metric.toList
      .flatMap(_.getLongSumData.getPoints.asScala)
      .map(p =>
        p.getAttributes.asMap.asScala
          .map((k, v) => k.getKey -> v.toString)
          .toMap -> p.getValue
      )
      .toMap

  private def total(metric: Option[MetricData]): Long = points(
    metric
  ).values.sum

  private val media =
    JitterStats(received = 100, played = 98, lost = 2, late = 1)

  test(
    "a call that is answered and ends is counted once, with its duration and what the jitter buffer did"
  ) {
    measured { (metrics, collect) =>
      for {
        _      <- metrics.callBegan(CallDirection.Outgoing)
        during <- collect
        _      <- metrics.callEnded(
          CallReport(
            CallDirection.Outgoing,
            CallOutcome.Completed,
            Some(90.seconds),
            media
          )
        )
        after <- collect
      } yield {
        assertEquals(
          points(during.get("softphone4s.calls.active")),
          Map(Map("direction" -> "outgoing") -> 1L)
        )
        assertEquals(total(after.get("softphone4s.calls.active")), 0L)
        assertEquals(
          points(after.get("softphone4s.calls")),
          Map(Map("direction" -> "outgoing", "outcome" -> "completed") -> 1L)
        )
        val histogram = after(
          "softphone4s.call.duration"
        ).getHistogramData.getPoints.asScala.toList
        assertEquals(
          histogram.map(p => (p.getCount, p.getSum)),
          List((1L, 90.0))
        )
        // what happened is counted, what did not is not
        assertEquals(
          points(after.get("softphone4s.rtp.frames")),
          Map(
            Map("event" -> "received") -> 100L,
            Map("event" -> "played")   -> 98L,
            Map("event" -> "lost")     -> 2L,
            Map("event" -> "late")     -> 1L
          )
        )
      }
    }
  }

  test("a call that is refused says with what status, and has no duration") {
    measured { (metrics, collect) =>
      for {
        _ <- metrics.callBegan(CallDirection.Incoming)
        _ <- metrics.callEnded(
          CallReport(
            CallDirection.Incoming,
            CallOutcome.Rejected(486),
            None,
            JitterStats()
          )
        )
        _ <- metrics.callBegan(CallDirection.Incoming)
        _ <- metrics.callEnded(
          CallReport(
            CallDirection.Incoming,
            CallOutcome.Declined,
            None,
            JitterStats()
          )
        )
        seen <- collect
      } yield {
        assertEquals(
          points(seen.get("softphone4s.calls")),
          Map(
            Map(
              "direction" -> "incoming",
              "outcome"   -> "rejected",
              "status"    -> "486"
            )                                                       -> 1L,
            Map("direction" -> "incoming", "outcome" -> "declined") -> 1L
          )
        )
        assert(!seen.contains("softphone4s.call.duration"), seen.keys.toString)
        assertEquals(total(seen.get("softphone4s.calls.active")), 0L)
      }
    }
  }

  test(
    "the registered accounts go up and down with the registration, and each state entered is counted"
  ) {
    measured { (metrics, collect) =>
      def at(state: RegistrationState) =
        metrics.registration(account, state) >> collect.map(m =>
          total(m.get("softphone4s.registrations.active"))
        )
      for {
        unregistered <- at(RegistrationState.Unregistered)
        registering  <- at(RegistrationState.Registering)
        registered   <- at(RegistrationState.Registered(3600.seconds))
        again        <- at(RegistrationState.Registered(3600.seconds))
        failed       <- at(
          RegistrationState.Failed(RegistrationFailure.TimedOut, 5.seconds)
        )
        back <- at(RegistrationState.Registered(3600.seconds))
        gone <- at(RegistrationState.Unregistered)
        seen <- collect
      } yield {
        assertEquals(
          List(
            unregistered,
            registering,
            registered,
            again,
            failed,
            back,
            gone
          ),
          List(0L, 0L, 1L, 1L, 0L, 1L, 0L)
        )
        assertEquals(
          points(seen.get("softphone4s.registration.changes")),
          Map(
            Map("state" -> "unregistered") -> 2L,
            Map("state" -> "registering")  -> 1L,
            Map("state" -> "registered")   -> 3L,
            Map("state" -> "failed")       -> 1L
          )
        )
      }
    }
  }

  test("retransmissions and timeouts are counted by side and method") {
    measured { (metrics, collect) =>
      for {
        _ <- metrics.transaction(
          TransactionEvent.Retransmitted(
            TransactionSide.Client,
            SipMethod.Invite
          )
        )
        _ <- metrics.transaction(
          TransactionEvent.Retransmitted(
            TransactionSide.Client,
            SipMethod.Invite
          )
        )
        _ <- metrics.transaction(
          TransactionEvent.Retransmitted(
            TransactionSide.Server,
            SipMethod.Options
          )
        )
        _ <- metrics.transaction(
          TransactionEvent.TimedOut(TransactionSide.Client, SipMethod.Register)
        )
        seen <- collect
      } yield {
        assertEquals(
          points(seen.get("softphone4s.sip.retransmissions")),
          Map(
            Map("side" -> "client", "method" -> "INVITE")  -> 2L,
            Map("side" -> "server", "method" -> "OPTIONS") -> 1L
          )
        )
        assertEquals(
          points(seen.get("softphone4s.sip.timeouts")),
          Map(Map("side" -> "client", "method" -> "REGISTER") -> 1L)
        )
      }
    }
  }
}
