# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `softphone4s` — outbound SIP calls (RFC 3261) over UDP: INVITE / ACK / BYE
  / CANCEL, Digest authentication (RFC 2617), RTP audio with the G.711
  μ-law codec, DTMF send/receive, and automatic DNS resolution for SIP
  destinations.
- `softphone4s-otel4s` — `TracedSoftphone` wrapper adding OpenTelemetry
  spans around call setup, media, and DTMF events.
- `softphone4s-demo` — runnable example app dialing a configured SIP
  destination.
- Logging via `org.typelevel.log4cats.Logger[F]`.

[Unreleased]: https://github.com/max-peroch/softphone4s/commits/main
