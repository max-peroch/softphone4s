# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
Before 1.0 a release may break compatibility; [MIGRATION.md](MIGRATION.md) says
what to change when it does.

## [Unreleased]

### Added

- **Signalling.** Registration with automatic refresh; incoming calls
  (`phone.incoming`, with `accept`, `reject`, `ringing`); hold and `UPDATE`;
  early media on outgoing calls; forking; several accounts on one phone (`callAs`, `registerAs`).
  SIP over UDP, TCP and TLS, with RFC 3263 resolution (NAPTR, SRV, A/AAAA) and
  failover to the next target; a request too big for UDP goes over TCP.
- **NAT.** `rport`, keep-alives, STUN on the SIP socket and, for each call, on
  the RTP port (so the SDP advertises the mapping the NAT gives it, port
  included), RTP latching.
- **Media.** SDP offer/answer; G.711 (`PCMU`, `PCMA`) and G.722, and Opus in the
  new `softphone4s-opus` module (libopus through the Foreign Function & Memory
  API, Java 22 and later); RTCP; an adaptive jitter buffer with concealment;
  SRTP and SRTCP with SDES; a pool of RTP ports.
- **Messaging, subscriptions and publication.** `MESSAGE` (RFC 3428),
  `SUBSCRIBE`/`NOTIFY` (RFC 6665) with the `message-summary`, `presence` (PIDF)
  and `dialog` (RFC 4235, with `DialogInfo.track` for partial notifications)
  event packages, and `PUBLISH` (RFC 3903); presence carries the activities of
  rich presence (RFC 4480: Busy, Away, On the phone), read and published, as `phone.messages`,
  `phone.subscriptions` and `phone.publications`, each with a traced decorator in
  `softphone4s-otel4s`.
- **Observability.** A `Metrics[F]` hook (calls, registrations, SIP
  retransmissions and timeouts, what the jitter buffer did) with
  `OtelMetrics` in `softphone4s-otel4s`; a `TransactionTracing[F]` hook with
  `OtelTransactionTracing`, one span per SIP transaction from the request to the
  final answer; log lines carry named fields
  (`call-id`, `direction`, `account`, `branch`, `method`...), documented in
  `LogFields`.
- **Tests and tools.** Fuzzing of every decoder and hostile-input suites; two
  softphones calling each other over real UDP, including through a relay that
  loses, repeats and reorders SIP messages; interop tests against Asterisk 20 in
  a container (`softphone4s-interop`); a load and soak program
  (`softphone4s-bench`).
- A binary-compatibility report against the last release (`mimaReportBinaryIssues`).

### Changed

- **Breaking:** `SoftphoneConfig` uses `User`, `Secret` and `Host` where it used
  strings, is `final`, and has many new fields; see [MIGRATION.md](MIGRATION.md).
- **Breaking:** `Softphone` and `Call` have more members.
- **Breaking:** `SoftphoneError.RtpPortExhausted(port)` is now
  `RtpPortsExhausted(first, last)`, and `SoftphoneError` has many more cases.
- A call that is up costs less when nobody is speaking, and about half the
  allocation when somebody is: the jitter-buffer playout is a loop of effects that
  makes a frame every 20 ms, not a merge of a tick stream and a packet stream
  (`JitterPlayout.run` replaces the stream-returning `JitterPlayout.apply`).
  One process now holds about 100 calls with all the audio arriving, where it held
  about 40.
- Timers, durations and sizes in the configuration are typed (`Attempts`,
  `ByteLimit`, `MaxConnections`); a value below one is a load error.
- `softphone4s-otel4s` is built against OpenTelemetry Java 1.64.0, which otel4s
  1.1.0 needs.

### Removed

- `SoftphoneError.InvalidHostname`: a host is validated when the configuration
  is built.

### Security

- What comes off the network is bounded: stream framing is linear in the bytes
  read, a body for an event package is read up to 128 KiB, and every decoder
  answers any input with a value, in time that grows with the input.

## [0.1.0] - 2026-08-13

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

[Unreleased]: https://github.com/max-peroch/softphone4s/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/max-peroch/softphone4s/releases/tag/v0.1.0
