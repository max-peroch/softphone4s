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
    libraryDependencies ++= Seq(
      munit,
      munitCatsEffect,
      munitScalaCheck,
      catsEffectTestkit,
      catsEffect,
      fs2Core,
      fs2Io,
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
    libraryDependencies ++= Seq(
      otel4sCoreTrace,
      opentelemetryApi
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

// JMH benchmarks: sbt "softphone4sBench/Jmh/run"
lazy val softphone4sBench = project
  .in(file("softphone4s-bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(softphone4s)
  .settings(
    name           := "softphone4s-bench",
    publish / skip := true
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
