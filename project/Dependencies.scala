import sbt._

object Dependencies {

  private val catsEffectVersion    = "3.7.1"
  private val fs2Version           = "3.14.0"
  private val pureconfigVersion    = "0.17.11"
  private val log4catsVersion      = "2.8.0"
  private val otel4sVersion        = "1.1.0"
  private val opentelemetryVersion = "1.64.0" // what otel4s 1.1.0 is built against
  private val scribeVersion        = "3.19.0"
  private val dnsjavaVersion       = "3.6.3"

  // Audio
  val sound4s = "io.github.max-peroch" %% "sound4s" % "0.2.0"

  // Effects & streaming
  val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
  val fs2Core    = "co.fs2"        %% "fs2-core"    % fs2Version
  val fs2Io      = "co.fs2"        %% "fs2-io"      % fs2Version

  // XML, for the bodies of presence (PIDF). Reads in Scala, never fetches an
  // external entity or DTD.
  val fs2DataXml = "org.gnieh" %% "fs2-data-xml" % "1.14.1"

  // DNS (SRV and NAPTR records, with their TTLs)
  val dnsjava = "dnsjava" % "dnsjava" % dnsjavaVersion

  // Testing
  val munit           = "org.scalameta" %% "munit"             % "1.3.6" % Test
  val munitCatsEffect = "org.typelevel" %% "munit-cats-effect" % "2.2.1" % Test
  val munitScalaCheck = "org.scalameta" %% "munit-scalacheck" % "1.3.1" % Test
  val catsEffectTestkit =
    "org.typelevel" %% "cats-effect-testkit" % catsEffectVersion % Test

  // Config
  val pureconfigCore =
    "com.github.pureconfig" %% "pureconfig-core" % pureconfigVersion
  val pureconfigGeneric =
    "com.github.pureconfig" %% "pureconfig-generic-scala3" % pureconfigVersion

  // Logging
  val log4catsCore  = "org.typelevel" %% "log4cats-core"  % log4catsVersion
  val log4catsSlf4j = "org.typelevel" %% "log4cats-slf4j" % log4catsVersion
  val log4catsTesting =
    "org.typelevel" %% "log4cats-testing" % log4catsVersion % Test
  val scribe        = "com.outr"      %% "scribe"         % scribeVersion
  val scribeSlf4j   = "com.outr"      %% "scribe-slf4j"   % scribeVersion

  // Observability (tracing / OpenTelemetry)
  val otel4sCoreTrace   = "org.typelevel" %% "otel4s-core-trace"   % otel4sVersion
  val otel4sCoreMetrics = "org.typelevel" %% "otel4s-core-metrics" % otel4sVersion
  val otel4sOteljavaTestkit =
    "org.typelevel" %% "otel4s-oteljava-testkit" % otel4sVersion % Test
  val otel4sOteljava  = "org.typelevel" %% "otel4s-oteljava"   % otel4sVersion
  val otel4sOteljavaContextStorage =
    "org.typelevel" %% "otel4s-oteljava-context-storage" % otel4sVersion
  val opentelemetryApi =
    "io.opentelemetry" % "opentelemetry-api" % opentelemetryVersion
  val opentelemetryExporterOtlp =
    "io.opentelemetry" % "opentelemetry-exporter-otlp" % opentelemetryVersion
}
