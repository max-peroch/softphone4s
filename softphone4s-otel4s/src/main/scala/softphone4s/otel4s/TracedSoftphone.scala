package softphone4s.otel4s

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import fs2.{Pipe, Stream}
import io.opentelemetry.context.ContextStorage
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.{SpanKind, SpanOps, Tracer}
import softphone4s.{Call, Softphone}
import softphone4s.model.{Callee, DtmfDigit}

private final class TracedCall[F[_]: Async](
    underlying: Call[F],
    tracer: Tracer[F]
) extends Call[F] {
  def source: Stream[F, Byte] =
    Stream
      .resource(tracer.spanBuilder("softphone4s.call.source").build.resource)
      .flatMap(_ => underlying.source)

  def sink: Pipe[F, Byte, Unit] =
    in =>
      Stream
        .resource(tracer.spanBuilder("softphone4s.call.sink").build.resource)
        .flatMap(_ => in.through(underlying.sink))

  def dtmfEvents: Stream[F, DtmfDigit] =
    underlying.dtmfEvents.evalTap { digit =>
      tracer
        .spanBuilder("softphone4s.dtmf.received")
        .addAttribute(Attribute("telephony.dtmf.digit", digit.code.toString))
        .build
        .surround(Async[F].unit)
    }

  def awaitPickup: F[Unit] =
    tracer.span("softphone4s.call.awaitPickup").surround(underlying.awaitPickup)

  def awaitHangup: F[Unit] =
    tracer.span("softphone4s.call.awaitHangup").surround(underlying.awaitHangup)
}

/** `Softphone` decorator adding OpenTelemetry spans around call setup, media,
  * and DTMF, and propagating the active trace context as a `traceparent` header
  * on outbound INVITEs.
  */
final class TracedSoftphone[F[_]: Async](
    underlying: Softphone[F],
    tracer: Tracer[F]
) extends Softphone[F] {

  // `res.trace` is the F ~> F that installs the span in the IOLocal — but only for the
  // duration of a single F[A].  To hold it active for a Resource's lifetime we:
  //   1. read the Java context *inside* res.trace (where the IOLocal already has the span)
  //   2. attach that context via Java OTel's ContextStorage, which with
  //      IOLocalContextStorage routes through to the fiber's IOLocal state
  //   3. release restores the previous context via Scope.close
  private def activateSpanContext(res: SpanOps.Res[F]): Resource[F, Unit] =
    Resource
      .eval(res.trace(Async[F].delay(ContextStorage.get().current())))
      .flatMap { jCtx =>
        Resource
          .make(Async[F].delay(ContextStorage.get().attach(jCtx)))(sc =>
            Async[F].delay(sc.close())
          )
          .void
      }

  def call(
      callee: Callee,
      extraHeaders: Map[String, List[String]] = Map.empty
  ): Resource[F, Call[F]] =
    tracer
      .spanBuilder("softphone4s.call")
      .withSpanKind(SpanKind.Client)
      .addAttribute(Attribute("telephony.callee", callee.value))
      .build
      .resource
      .flatMap { res =>
        val ctx                                     = res.span.context
        val traceHeaders: Map[String, List[String]] =
          if ctx.isValid then
            Map(
              "traceparent" -> List(
                s"00-${ctx.traceIdHex}-${ctx.spanIdHex}-${
                    if ctx.isSampled then "01" else "00"
                  }"
              )
            )
          else Map.empty
        for {
          _    <- activateSpanContext(res)
          call <- underlying.call(callee, traceHeaders ++ extraHeaders)
        } yield new TracedCall(call, tracer)
      }
}

object TracedSoftphone {

  /** Wraps `underlying` so every call is traced via `tracer`. */
  def apply[F[_]: Async](
      underlying: Softphone[F],
      tracer: Tracer[F]
  ): Softphone[F] = new TracedSoftphone(underlying, tracer)
}
