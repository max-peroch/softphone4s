ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "io.github.max-peroch"
ThisBuild / homepage   := Some(uri("https://github.com/max-peroch/softphone4s"))
ThisBuild / licenses   := List(License.Apache2)
ThisBuild / developers := List(
  Developer(
    "max-peroch",
    "Maxime Perocheau",
    "max.peroch@hotmail.fr",
    uri("https://perocheau.com")
  )
)
ThisBuild / scalafmtOnCompile := true
ThisBuild / scalacOptions     := Seq(
  "-Wunused:all",
  "-Werror"
)

import Dependencies._

lazy val softphone4s = project
  .in(file("softphone4s"))
  .settings(
    name                := "softphone4s",
    scalacOptions       ++= Seq("-release", "17"),
    // Many suites bind real sockets on fixed ports, and the ports of one are
    // another's. They run one after the other.
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      munit,
      munitCatsEffect,
      munitScalaCheck,
      catsEffectTestkit,
      catsEffect,
      fs2Core,
      fs2Io,
      fs2DataXml,
      pureconfigCore,
      pureconfigGeneric,
      log4catsCore,
      dnsjava
    )
  )

lazy val softphone4sOtel4s = project
  .in(file("softphone4s-otel4s"))
  .dependsOn(softphone4s)
  .settings(
    name                := "softphone4s-otel4s",
    scalacOptions       ++= Seq("-release", "17"),
    libraryDependencies ++= Seq(
      otel4sCoreTrace,
      otel4sCoreMetrics,
      opentelemetryApi,
      munit,
      munitCatsEffect,
      otel4sOteljavaTestkit
    )
  )

// Opus, through libopus; needs --enable-native-access=ALL-UNNAMED and libopus
lazy val softphone4sOpus = project
  .in(file("softphone4s-opus"))
  .dependsOn(softphone4s)
  .settings(
    name := "softphone4s-opus",
    libraryDependencies ++= Seq(munit, munitCatsEffect, catsEffectTestkit),
    // Forked, with native access allowed. The tests that need libopus use the
    // system one, or the path in softphone4s-opus/.opus-library
    Test / fork := true,
    Test / javaOptions ++= Seq("--enable-native-access=ALL-UNNAMED")
  )

// Interop tests against real servers in Docker. Not part of the root, so that
// `sbt testFull` does not need Docker: sbt softphone4sInterop/test
lazy val softphone4sInterop = project
  .in(file("softphone4s-interop"))
  .dependsOn(softphone4s)
  .settings(
    name           := "softphone4s-interop",
    publish / skip := true,
    // Each suite starts its own Asterisk, and one at a time is plenty.
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(munit, munitCatsEffect)
  )

// JMH benchmarks: sbt "softphone4sBench/Jmh/run"
lazy val softphone4sBench = project
  .in(file("softphone4s-bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(softphone4s)
  .settings(
    name           := "softphone4s-bench",
    publish / skip := true,
    // The load and soak tests are programs, run in a JVM of their own
    run / fork         := true,
    run / javaOptions ++= Seq("-Xmx2g"),
    run / connectInput := true
  )

lazy val softphone4sDemo = project
  .in(file("softphone4s-demo"))
  .dependsOn(softphone4s, softphone4sOtel4s)
  .settings(
    name                := "softphone4s-demo",
    mainClass           := Some("Demo"),
    run / fork          := true,
    run / connectInput  := true,
    publish / skip      := true,
    libraryDependencies ++= Seq(
      sound4s,
      scribe,
      scribeSlf4j,
      log4catsSlf4j,
      pureconfigCore,
      otel4sOteljava,
      otel4sOteljavaContextStorage,
      opentelemetryExporterOtlp
    ),
    javaOptions ++= Seq(
      "--sun-misc-unsafe-memory-access=allow",
      "-Dcats.effect.trackFiberContext=true",
      "-Dotel.service.name=demo",
      "-Dotel.exporter.otlp.endpoint=http://localhost:4317",
      "-Dotel.metrics.exporter=none",
      "-Dotel.logs.exporter=none"
    )
  )

// The modules that are built, tested and published together. The interop tests
// need Docker and are run on their own.
lazy val root = (project in file("."))
  .aggregate(
    softphone4s,
    softphone4sOtel4s,
    softphone4sOpus,
    softphone4sBench,
    softphone4sDemo
  )
  .settings(
    name           := "softphone4s-root",
    publish / skip := true
  )
