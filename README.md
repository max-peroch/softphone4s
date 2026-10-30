# softphone4s

A purely functional SIP/VoIP client for the Typelevel ecosystem, built on
[Cats Effect](https://typelevel.org/cats-effect/) and [fs2](https://fs2.io).

| softphone4s | Scala | Cats Effect | fs2    |
|-------------|-------|-------------|--------|
| \<latest\>    | 3.x   | 3.7.x       | 3.13.x |

## Modules

| Module               | Description |
|----------------------|-------------|
| `softphone4s`        | Core SIP/RTP client (this README) |
| `softphone4s-otel4s` | OpenTelemetry tracing wrapper around `Softphone[F]` |
| `softphone4s-demo`   | Runnable example app (not published) |
| `softphone4s-bench`  | JMH benchmarks for the SIP codec (not published) |

## Installation

```scala
libraryDependencies += "io.github.max-peroch" %% "softphone4s" % "<latest version>"

// optional: tracing
libraryDependencies += "io.github.max-peroch" %% "softphone4s-otel4s" % "<latest version>"
```

See the [releases page](https://github.com/max-peroch/softphone4s/releases)
for the latest published version, and [`build.sbt`](build.sbt) for the exact
Scala, Cats Effect, and fs2 versions this release was built against.

## Overview

softphone4s handles outbound SIP calls end-to-end:

- **SIP** (RFC 3261) over UDP — INVITE / ACK / BYE / CANCEL
- **Digest authentication** (RFC 2617)
- **RTP audio** with G.711 μ-law codec
- **DTMF** reception and transmission
- Automatic DNS resolution for SIP destinations

Call lifecycle is modelled as `Resource[F, Call[F]]` — acquiring the resource
dials the number, releasing it sends BYE and drains the event loop.

softphone4s places outbound calls and can register the account at its
registrar. It does not handle inbound INVITEs yet, and has no SDP offer/answer
beyond what's needed to set up a call it initiated. It's not a full Scala port
of a SIP stack.

`softphone4s` takes an explicit
[`org.typelevel.log4cats.Logger[F]`](https://typelevel.org/log4cats/) — this
library depends only on `log4cats-core`, so you choose the backend: wire in
`log4cats-slf4j` for real logging, or pass a `log4cats-noop` logger to
discard log output.

## Quick Start

### Configuration

softphone4s reads config via [pureconfig](https://pureconfig.github.io/).
Add the required fields to your `application.conf`:

```hocon
softphone {
  user   = "alice"
  password = "s3cr3t"
  realm  = "sip.example.com"
}
```

```scala
import pureconfig.*
import softphone4s.config.SoftphoneConfig

val config = ConfigSource.default.at("softphone").loadOrThrow[SoftphoneConfig]
```

Any missing or malformed field throws at startup — fail fast.

### Making a call

```scala
import cats.effect.*
import fs2.Stream
import softphone4s.SoftphoneStreaming
import org.typelevel.log4cats.slf4j.Slf4jLogger
import javax.sound.sampled.AudioFormat

val audioFormat = AudioFormat(8000f, 16, 1, true, false)

val program: IO[Unit] =
  Slf4jLogger.create[IO].flatMap { logger =>
    SoftphoneStreaming.resource[IO](config, audioFormat, logger).use { phone =>
      phone.call(callee).use { call =>
        call.awaitPickup >>
          micStream                             // Stream[IO, Byte] of PCM
            .through(call.sink)
            .concurrently(call.source.through(speakerPipe))
            .interruptWhen(call.awaitHangup.attempt)
            .compile
            .drain
      }
    }
  }
```

Releasing the `call` resource sends BYE and waits up to `hangup-timeout`
for the event loop to drain, then cancels the fiber.

### Registering

Holding `phone.register` keeps the account registered at its registrar: the
binding is refreshed at about half its granted lifetime, a failed attempt is
retried with backoff (5 s, doubling, up to 5 minutes), a 401 or 407 is answered
once, and releasing it removes the binding with `Expires: 0`.

```scala
phone.register.use { registration =>
  registration.awaitRegistered >>
    registration.state.discrete          // Registering, Registered(3600 s), Failed(..), ...
      .evalMap(state => IO.println(state))
      .compile
      .drain
}
```

`register` does not fail if the registrar is unreachable: the state reports
`Failed(reason, retryIn)` and the agent keeps trying.

### Receiving DTMF

```scala
call.dtmfEvents
  .evalMap(digit => IO.println(s"Digit: $digit"))
  .compile
  .drain
```

`dtmfEvents` and `source` terminate when the call ends. `sink` does not — use
`.interruptWhen(call.awaitHangup.attempt)` on any pipeline that includes it to
avoid the stream hanging on remote hangup.

The [`softphone4s-demo`](softphone4s-demo) module is a complete runnable
example wiring in tracing and audio hardware.

## Configuration reference

| Key                 | Type        | Default      | Description                                        |
|---------------------|-------------|--------------|-----------------------------------------------------|
| `user`              | `String`    | required     | SIP username                                       |
| `password`            | `String`    | required     | SIP password                                       |
| `realm`             | `String`    | required     | SIP realm / server hostname                        |
| `local-sip-port`    | `Port`      | `5060`       | Local UDP port for SIP                             |
| `server-port`       | `Port`      | `5060`       | Remote SIP server port                             |
| `base-rtp-port`     | `Port`      | `8000`       | Base RTP port; each concurrent call uses two ports |
| `bind-address`      | `IpAddress` | `0.0.0.0`    | Local bind address                                 |
| `local-ip-fallback` | `IpAddress` | `127.0.0.1`  | Advertised when no IPv4 route to the SIP server is found |
| `hangup-timeout`    | `Duration`  | `5 seconds`  | Grace period for event loop drain on release       |
| `invite-timeout`    | `Duration`  | `32 seconds` | Max wait for remote to answer (RFC 3261 Timer B)   |
| `register-expires`  | `Duration`  | `1 hour`     | Binding lifetime asked for when registering        |

## Errors

All failures surface as `SoftphoneError` (a sealed `RuntimeException` hierarchy):

| Error | Cause |
|-------|-------|
| `InvalidSipUri(input, reason)` | The callee, or the configured `user`/`realm`, is not a valid SIP URI |
| `InvalidHeader(name, reason)` | An `extraHeaders` entry has an invalid header name |
| `HostResolutionFailed(host)` | DNS lookup returned no result |
| `CallRejected(code, reason)` | Remote returned a 4xx / 5xx / 6xx response |
| `CallTimedOut` | No answer within `invite-timeout` |
| `CallCancelled` | BYE received from remote, or local hangup |
| `TransportFailed` | The SIP transport could not send a request of the call |
| `RtpPortExhausted(port)` | Computed RTP port exceeds 65535 (too many concurrent calls) |

`awaitPickup` raises the error as an effect if the call is rejected or
times out before being answered.

## Architecture

```
SoftphoneStreaming
├─ SipTransport           — shared UDP socket
├─ TransactionManager     — RFC 3261 §17 client and server transactions
│                           (retransmission, matching, ACK for failures)
├─ routes in-dialog requests and strays to calls by Call-ID
└─ per call
   └─ OutboundCall
      ├─ CallFsm          — pure state machine (cats State)
      ├─ SipMessages      — constructs INVITE / ACK / BYE / CANCEL
      └─ MediaSession
         ├─ RtpTransport  — per-call UDP socket
         └─ G711Codec     — μ-law ↔ 16-bit PCM
```

`CallFsm` is pure: given an event and the current state it returns a new
state and a list of `SipAction` values. `OutboundCall` interprets those
actions against `F`, keeping effects out of the FSM. The INVITE, BYE and
CANCEL go out as client transactions; the ACK of a failure is the
transaction's, the ACK of a 2xx is the call's. The four transaction state
machines (`InviteClientTransaction` and its three siblings) are pure too and
emit timer actions that `Timers` runs.

## Inspirations

This project initially started as a wrapper around the following projects before evolving into a full rewrite:

- [mjSIP](https://github.com/mjsip/mjSIP) and the
  [haumacher/mjSIP](https://github.com/haumacher/mjSIP) fork — Java SIP
  stacks softphone4s' protocol handling drew on.
- [peers](https://github.com/ymartineau/peers) — a Java softphone whose
  approach to wiring SIP signaling to RTP media was a useful reference.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
