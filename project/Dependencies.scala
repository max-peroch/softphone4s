import sbt._

object Dependencies {

  private val catsEffectVersion    = "3.7.0"
  private val fs2Version           = "3.13.0"
  private val pureconfigVersion    = "0.17.10"
  private val log4catsVersion      = "2.8.0"
  private val otel4sVersion        = "1.1.0"
  private val opentelemetryVersion = "1.65.0"
  private val scribeVersion        = "3.19.0"

  // Audio
  val sound4s = "io.github.max-peroch" %% "sound4s" % "0.2.0"

  // Effects & streaming
  val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
  val fs2Core    = "co.fs2"        %% "fs2-core"    % fs2Version
  val fs2Io      = "co.fs2"        %% "fs2-io"      % fs2Version

  // Testing
  val munit           = "org.scalameta" %% "munit"             % "1.3.5" % Test
  val munitCatsEffect = "org.typelevel" %% "munit-cats-effect" % "2.2.0" % Test

  // Config
  val pureconfigCore =
    "com.github.pureconfig" %% "pureconfig-core" % pureconfigVersion
  val pureconfigGeneric =
    "com.github.pureconfig" %% "pureconfig-generic-scala3" % pureconfigVersion

  // Logging
  val log4catsCore  = "org.typelevel" %% "log4cats-core"  % log4catsVersion
  val log4catsSlf4j = "org.typelevel" %% "log4cats-slf4j" % log4catsVersion
  val scribe        = "com.outr"      %% "scribe"         % scribeVersion
  val scribeSlf4j   = "com.outr"      %% "scribe-slf4j"   % scribeVersion

  // Observability (tracing / OpenTelemetry)
  val otel4sCoreTrace = "org.typelevel" %% "otel4s-core-trace" % otel4sVersion
  val otel4sOteljava  = "org.typelevel" %% "otel4s-oteljava"   % otel4sVersion
  val otel4sOteljavaContextStorage =
    "org.typelevel" %% "otel4s-oteljava-context-storage" % otel4sVersion
  val opentelemetryApi =
    "io.opentelemetry" % "opentelemetry-api" % opentelemetryVersion
  val opentelemetryExporterOtlp =
    "io.opentelemetry" % "opentelemetry-exporter-otlp" % opentelemetryVersion
}
