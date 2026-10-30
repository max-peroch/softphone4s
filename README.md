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
- **Mid-call requests from the remote party**: OPTIONS is answered with our
  `Allow` list; a re-INVITE or UPDATE with SDP is answered with our SDP, media
  follows the new address, and a hold (`sendonly`, `inactive` or `0.0.0.0`)
  stops our outgoing RTP until the call resumes. A re-INVITE without SDP is
  refused `488`.
- **Dialogs** (RFC 3261 §12): requests within a call follow the Record-Route
  set, with loose or strict routing, so a BYE goes to the first route hop and
  not straight to the remote's Contact. A request whose tags do not match the
  dialog is answered `481`, and one that is not newer than the last is
  answered `500`. If a next hop cannot be resolved the request goes to the
  server the call was placed to, or came from.
- **Digest authentication** (RFC 2617, RFC 7616, RFC 8760): MD5, MD5-sess,
  SHA-256 and SHA-256-sess, with `qop=auth` or `auth-int`. When a server offers
  several challenges the strongest supported algorithm is used. A registration
  keeps the credentials that last worked and sends them on the next REGISTER
  with a higher `nc`, so a refresh needs no 401 round trip.
- **RTP audio** with G.711 μ-law codec
- **DTMF** reception and transmission
- Automatic DNS resolution for SIP destinations

Call lifecycle is modelled as `Resource[F, Call[F]]` — acquiring the resource
dials the number, releasing it sends BYE and drains the event loop.

softphone4s places and receives calls and can register the account at its
registrar. An incoming call is answered with a single SDP answer (G.711 μ-law,
plus RFC 4733 DTMF if the caller offered it); a call whose offer has no PCMU, or
no SDP at all, is refused with `488`. There is no re-INVITE or UPDATE yet, and
it is not a full Scala port of a SIP stack.

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

### Receiving calls

`phone.incoming` is a stream of calls placed to the softphone. Ring, then accept
or reject; a call nobody answers is turned away with `480` after
`answer-timeout`.

```scala
phone.incoming.evalMap { call =>
  call.ringing >>
    call.accept.use { accepted =>
      accepted.awaitPickup >>            // the caller's ACK arrived
        micStream.through(accepted.sink)
          .concurrently(accepted.source.through(speakerPipe))
          .interruptWhen(accepted.awaitHangup.attempt)
          .compile.drain
    }
}.compile.drain
```

`call.caller` and `call.callee` are the From and To; `call.cancelled` completes
if the caller gives up first; `call.reject(FailureStatus.BusyHere)` turns the
call away. Releasing `accept` hangs up with a BYE. The 200 OK is repeated until
the ACK arrives (RFC 3261 §13.3.1.4), and a request that is not newer than the
last one in the dialog is answered `500`.

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
| `answer-timeout`    | `Duration`  | `60 seconds` | How long an incoming call may ring before `480`     |

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
