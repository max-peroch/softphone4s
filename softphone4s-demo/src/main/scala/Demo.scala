import cats.effect.*
import org.typelevel.log4cats.StructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.oteljava.context.*
import pureconfig.*
import softphone4s.{Softphone, SoftphoneStreaming}
import softphone4s.config.SoftphoneConfig
import softphone4s.model.Callee
import softphone4s.otel4s.TracedSoftphone
import sound4s.{AudioFile, SoundStreaming, ResourceName}
import fs2.Stream

import javax.sound.sampled.AudioFormat

object Demo extends IOApp {

  given LocalContextProvider[IO] = IOLocalContextStorage.localProvider

  ScribeConfig.configure()

  private case class DemoConfig(
      softphone: SoftphoneConfig,
      callTo: Callee,
      hangupDtmfCode: Int
  ) derives ConfigReader

  private lazy val config: DemoConfig =
    ConfigSource.default.at("demo").loadOrThrow[DemoConfig]

  private val audioFormat: AudioFormat =
    AudioFormat(8000f, 16, 1, true, false)

  private def progressSoundStream: Stream[IO, Byte] =
    Stream
      .eval(
        IO.fromEither(
          ResourceName("progress.wav").left.map(new IllegalArgumentException(_))
        )
      )
      .flatMap(AudioFile[IO](_, audioFormat).source)
      .repeat

  private val awaitPressToHangUp: IO[Unit] =
    IO.println("Press enter to hang up...") >> IO.readLine.void

  private def onDtmf(
      logger: StructuredLogger[IO],
      hangupSignal: Deferred[IO, Unit]
  )(code: Int): IO[Unit] =
    logger.info(Map("code" -> code.toString))("DTMF received") >>
      (if code == config.hangupDtmfCode then hangupSignal.complete(()).void
       else IO.unit)

  private def makeCall(
      logger: StructuredLogger[IO],
      softphone: Softphone[IO],
      jss: SoundStreaming[IO]
  ): IO[Unit] =
    for {
      _ <- logger.info(Map("callee" -> config.callTo.value))(
        "Placing call"
      )
      hangupSignal <- Deferred[IO, Unit]
      _            <- softphone.call(config.callTo).use { call =>
        call.awaitPickup
          .race(
            progressSoundStream
              .through(jss.sink)
              .compile
              .drain
          ) >>
          jss.source
            .through(call.sink)
            .concurrently(
              call.source.through(jss.sink)
            )
            .concurrently(
              call.dtmfEvents.evalTap(d => onDtmf(logger, hangupSignal)(d.code))
            )
            .interruptWhen(
              awaitPressToHangUp
                .race(hangupSignal.get)
                .void
                .race(call.awaitHangup)
                .void
                .attempt
            )
            .compile
            .drain
      }
      _ <- logger.info(Map("callee" -> config.callTo.value))("Call ended")
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    (for {
      otel         <- OtelJava.autoConfigured[IO]()
      tracer       <- Resource.eval(otel.tracerProvider.get("softphone4s-demo"))
      logger       <- Resource.eval(Slf4jLogger.create[IO])
      jss          <- SoundStreaming.resource[IO](audioFormat, logger)
      rawSoftphone <- SoftphoneStreaming.resource(
        config.softphone,
        audioFormat,
        logger
      )
      softphone = TracedSoftphone(rawSoftphone, tracer)
      _ <- Resource.eval(makeCall(logger, softphone, jss))
    } yield ()).use_.as(ExitCode.Success)
}
