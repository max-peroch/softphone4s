ThisBuild / scalaVersion := "3.3.8"
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
  "-Xfatal-warnings"
)

import Dependencies._

lazy val softphone4s = project
  .in(file("softphone4s"))
  .settings(
    name                := "softphone4s",
    libraryDependencies := Seq(
      munit,
      munitCatsEffect,
      catsEffect,
      fs2Core,
      fs2Io,
      pureconfigCore,
      pureconfigGeneric,
      log4catsCore
    )
  )

lazy val softphone4sOtel4s = project
  .in(file("softphone4s-otel4s"))
  .dependsOn(softphone4s)
  .settings(
    name                := "softphone4s-otel4s",
    libraryDependencies := Seq(
      otel4sCoreTrace,
      opentelemetryApi
    )
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
    libraryDependencies := Seq(
      sounds4s,
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
