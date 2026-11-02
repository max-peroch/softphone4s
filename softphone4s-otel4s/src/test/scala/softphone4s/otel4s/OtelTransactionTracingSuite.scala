package softphone4s.otel4s

import cats.effect.IO
import io.opentelemetry.api.trace.{SpanKind, StatusCode}
import io.opentelemetry.sdk.trace.data.SpanData
import munit.CatsEffectSuite
import org.typelevel.otel4s.oteljava.testkit.trace.TracesTestkit
import softphone4s.model.{Branch, CallId, SipMethod}
import softphone4s.transaction.*

import scala.jdk.CollectionConverters.*

class OtelTransactionTracingSuite extends CatsEffectSuite {

  private def info(
      side: TransactionSide,
      method: SipMethod = SipMethod.Options
  ) =
    TransactionInfo(
      side,
      method,
      Branch("z9hG4bKabc").fold(e => sys.error(e), identity),
      CallId("call-1@host").fold(e => sys.error(e), identity)
    )

  /** The tracing, and the spans that have finished so far. */
  private def traced[A](
      use: (TransactionTracing[IO], IO[List[SpanData]]) => IO[A]
  ): IO[A] =
    TracesTestkit.inMemory[IO]().use { kit =>
      kit.tracerProvider
        .get("softphone4s")
        .flatMap(tracer =>
          use(OtelTransactionTracing[IO](tracer), kit.finishedSpans)
        )
    }

  private def attributes(span: SpanData): Map[String, String] =
    span.getAttributes.asMap.asScala.map((k, v) => k.getKey -> v.toString).toMap

  test(
    "a client transaction answered with 200 is one finished span, a root, with its attributes"
  ) {
    traced { (tracing, finished) =>
      for {
        span   <- tracing.began(info(TransactionSide.Client))
        during <- finished
        _      <- span.end(TransactionOutcome.Answered(200))
        after  <- finished
      } yield {
        assertEquals(during, Nil, "a span that has not ended is not finished")
        val s = after.head
        assertEquals(after.size, 1)
        assertEquals(s.getName, "SIP OPTIONS")
        assertEquals(s.getKind, SpanKind.CLIENT)
        assert(!s.getParentSpanContext.isValid, "the span has a parent")
        assertEquals(s.getStatus.getStatusCode, StatusCode.UNSET)
        assertEquals(
          attributes(s),
          Map(
            "sip.method"              -> "OPTIONS",
            "sip.transaction.side"    -> "client",
            "sip.call_id"             -> "call-1@host",
            "sip.branch"              -> "z9hG4bKabc",
            "sip.transaction.outcome" -> "answered",
            "sip.status_code"         -> "200"
          )
        )
      }
    }
  }

  test(
    "a refusal is an error for the client that receives it, not for the server that sends it"
  ) {
    traced { (tracing, finished) =>
      for {
        client <- tracing.began(info(TransactionSide.Client, SipMethod.Invite))
        server <- tracing.began(info(TransactionSide.Server, SipMethod.Invite))
        _      <- client.end(TransactionOutcome.Answered(486))
        _      <- server.end(TransactionOutcome.Responded(486))
        spans  <- finished
      } yield {
        val byKind = spans.map(s => s.getKind -> s).toMap
        assertEquals(
          byKind(SpanKind.CLIENT).getStatus.getStatusCode,
          StatusCode.ERROR
        )
        assertEquals(
          byKind(SpanKind.CLIENT).getStatus.getDescription,
          "refused with 486"
        )
        assertEquals(
          byKind(SpanKind.SERVER).getStatus.getStatusCode,
          StatusCode.UNSET
        )
        assertEquals(
          attributes(byKind(SpanKind.SERVER))("sip.status_code"),
          "486"
        )
      }
    }
  }

  test("a redirect is not an error") {
    traced { (tracing, finished) =>
      for {
        span  <- tracing.began(info(TransactionSide.Client, SipMethod.Invite))
        _     <- span.end(TransactionOutcome.Answered(302))
        spans <- finished
      } yield assertEquals(spans.head.getStatus.getStatusCode, StatusCode.UNSET)
    }
  }

  test(
    "a transaction that timed out, or could not send, is an error with no status code"
  ) {
    traced { (tracing, finished) =>
      for {
        a     <- tracing.began(info(TransactionSide.Client, SipMethod.Register))
        b     <- tracing.began(info(TransactionSide.Server, SipMethod.Invite))
        _     <- a.end(TransactionOutcome.TimedOut)
        _     <- b.end(TransactionOutcome.SendFailed)
        spans <- finished
      } yield {
        val byName =
          spans.map(s => attributes(s)("sip.transaction.outcome") -> s).toMap
        assertEquals(
          byName("timed_out").getStatus.getStatusCode,
          StatusCode.ERROR
        )
        assertEquals(byName("timed_out").getStatus.getDescription, "no answer")
        assertEquals(
          byName("send_failed").getStatus.getStatusCode,
          StatusCode.ERROR
        )
        assert(spans.forall(s => !attributes(s).contains("sip.status_code")))
      }
    }
  }
}
