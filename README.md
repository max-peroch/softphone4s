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

- **SIP** (RFC 3261) over UDP, TCP or TLS — INVITE / ACK / BYE / CANCEL. A call, or
  an account's registration, uses the transport you configure; a TCP
  connection is made when first needed, shared by everything sent to that
  destination, and closed after it has been idle for `tcp.idle-timeout`.
  Retransmission is left to TCP, so TCP and TLS transactions skip their timers.
- **Locating the server** (RFC 3263): the domain of a callee or registrar is
  looked up as the RFC says. A port in the URI, or a `server-port` other than
  5060, is used as it is. Otherwise a transport parameter or `sips:` goes
  straight to that transport's SRV records (`_sip._udp`, `_sip._tcp`,
  `_sips._tcp`); with neither, NAPTR records say which transports the domain
  offers and in what order; with no NAPTR each transport's SRV name is tried,
  yours first; and with no SRV records the domain's own addresses are used at
  the default port. Targets of one priority are picked at random in proportion
  to their weights (RFC 2782). SRV and NAPTR answers are cached for their TTLs
  (dnsjava); host addresses come from the system resolver, so the hosts file
  and split DNS still apply, and the DNS servers can be set with `dns.servers`.
  The next hop of a dialog (the Contact, or the first Route) and Record-Route
  hosts are located the same way, over the dialog's own transport; one that
  resolves to nothing falls back to the server that was dialled. When a call's
  first INVITE gets no answer (the transaction times out) or the transport
  fails (a refused connection, say), it is sent again to the next target, up to
  four, with a new branch and the Via and Contact of that target's transport;
  an answer of any kind, a refusal included, ends the search. The call's
  `invite-timeout` is allowed for each target. A registration moves to the next
  target each time an attempt fails, and stays on the one that works.
- **TLS** (RFC 3261 §26, RFC 5922): the server is identified by the host the
  request is addressed to (its first Route, else its Request-URI), which is sent
  as the SNI name and, by default, checked against the certificate. Trust comes
  from your own trust store or the JVM's; a client certificate can be presented
  (mutual TLS), the oldest protocol version is yours to set (1.2 by default),
  and a handshake that fails is a failed connection, not a lost message.
  Verification fails closed: a destination given as an IP address has no name
  to check, so use a host name, or switch verification off knowingly. A
  `sips:` URI selects TLS.
- **Behind a NAT** (RFC 3581): our requests over UDP carry `rport`, and the
  servers' answers carry `received` and `rport`, which is how we learn what we
  look like from outside. That address is believed only from a response to one of
  our own requests that came from the server it was sent to, and only over UDP.
  Once known, our Contact and the media address in our SDP are the public ones,
  for calls and for registrations (the Via keeps our own address, since answers
  come back by `rport`); a registration that is up moves its binding to the new
  Contact at once. When we answer a request we fill in `received` and `rport` in
  the same way for the sender. `nat.observed-address = "ignore"` keeps to the
  address we bound. Once the new Contact is registered, the old binding is
  removed with a REGISTER of its own (`expires=0`, a new Call-ID, answered once
  if challenged; best effort, as it expires anyway). Media follows the remote
  party: when their RTP keeps arriving from an address other than the one their
  SDP named (a NAT has remapped the port), five packets in a row of one stream
  from one new source move our media there (symmetric RTP, RFC 4961); stray
  packets do not, and nothing moves while a call is on hold.
  `nat.rtp-latching = "ignore"` sends only where the SDP says. Our own media
  port is assumed not to be remapped on the way out, which latching makes
  harmless when the other side does the same.
- **Codecs**: G.711 μ-law (`pcmu`) and A-law (`pcma`). `codecs` lists the ones
  to use, most wanted first (default `["pcmu", "pcma"]`): our offers list them
  in that order, and an answer takes the first of them that the offer has,
  whatever order the offer lists theirs in. The codec an answer settled on, and
  its payload type, are what media is sent and received in; packets of any
  other payload type are ignored. Within a call the codec stays: a re-INVITE or
  UPDATE that lacks it is refused with `488`. The codecs have to work at the
  sample rate of the audio format you give (8 kHz for G.711); anything else is
  an error when the softphone starts, not a conversion.
- **STUN** (RFC 8489): with `nat.stun.host` set, the SIP UDP socket itself
  sends a Binding request to the server (repeated, waiting twice as long each
  time) and the answer is used like an address a SIP server reported, so the
  public Contact is registered before the first answer comes back. It is asked
  again every `nat.stun.refresh`. An answer counts only if it comes from the
  server that was asked. Nothing is asked when `nat.observed-address` is
  `ignore`.
- **Accepting connections**: with `listen = [tcp, tls]` we also listen for TCP
  (on `local-sip-port`) and TLS (on `tls-port`, which needs
  `tls.server-certificate`; `tls.client-authentication` is `off`, `request` or
  `require`). A connection that comes in is served like one we opened, and the
  responses to its requests go back on it. Our Via on TCP and TLS carries
  `alias` (RFC 5923), and so does a peer's: a request with `alias` makes its
  connection the one to use for requests to that address, provided the address
  is on the IP the connection came from, so a peer cannot claim someone else's
  traffic. Requests to a Contact then reuse the connection a request arrived on.
  Over TLS that is only for a host the peer's certificate names (a DNS or IP
  subject alternative name, matched exactly, wildcards left out), which needs
  `tls.client-authentication` to be `request` or `require` so that the peer
  shows one, checked against `tls.trust-store`; a request for any other host
  gets a connection of its own, verified for that host. A connection that cannot
  be framed is dropped without affecting others.
- **Mid-call requests from the remote party**: OPTIONS is answered with our
  `Allow` list; a re-INVITE or UPDATE with SDP is answered with our SDP, media
  follows the new address, and a hold (`sendonly`, `inactive` or `0.0.0.0`)
  stops our outgoing RTP until the call resumes. A re-INVITE without SDP is
  answered with our offer in the `200`, and the answer in the ACK moves the
  media. The `200` to a re-INVITE is repeated until its ACK, as for the first
  INVITE.
- **Forked calls** (RFC 3261 §13.2.2.4): when a proxy forks an outgoing call,
  the first branch to answer is the call. A `200` from any other branch is
  ACKed, so it stops retransmitting, and then ended with a BYE.
- **Hold and resume**: `Call.hold` and `Call.resume` send a re-INVITE offering
  `sendonly` or `sendrecv`; `Call.changeSession` can send the same change in an
  UPDATE (RFC 3311) instead, which needs no ACK. A clash with the remote party's own re-INVITE is
  settled as RFC 3261 §14 says: the remote's gets `491`, and if ours gets a
  `491` it is sent again after a random delay (2.1–4 s for the party that
  placed the call, 0–2 s for the other). A `408` or `481` ends the call.
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

A callee is `1234`, `alice` or `alice@sip.example.com` (dialled at your realm
when no host is given), or a whole SIP URI. A URI is how you ask for a
transport: `sips:alice@example.com` for TLS, or
`sip:alice@example.com;transport=tcp`; without one the account's `transport`
and the domain's DNS records decide. An INVITE too big for a UDP datagram
(`max-udp-message`, 1300 bytes by default, RFC 3261 §18.1.1) is sent over TCP
instead, and over UDP after all if that connection cannot be made. The TCP
server is looked up as RFC 3263 says (`_sip._tcp` SRV records), so a domain
that serves TCP on another port is found. The same goes for the ACK, BYE and
re-INVITE or UPDATE of a dialog, whose size grows with its route set: they go
over TCP to the dialog's next hop, with the Via and Contact of TCP, and over
UDP after all if the connection fails. REGISTER cannot get near the limit (its
content is fixed), and responses go back on the transport of their request, as
the RFC says. A call to a `sips:` URI uses a `sips:` Contact (RFC 5630), and so
does an incoming `sips:` call; a `sips:` INVITE that arrives over UDP or TCP is
refused `403 Forbidden`, since every hop of it was to be TLS.

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

### Several accounts

The `user`, `password` and `realm` above are the default account, which `call`
and `register` use. More accounts go under `additional-accounts`; all of them
share the one SIP socket and RTP port range, and each has its own credentials,
Contact and registration:

```hocon
additional-accounts = [
  { user = "bob", password = "secret", realm = "other.example.org", server-port = 5070 }
]
```

```scala
phone.accounts                       // the default account first
phone.callAs(phone.accounts.last, callee)
phone.registerAs(phone.accounts.last)
phone.incoming.evalMap(call => IO.println(call.account))  // which one was called
```

An incoming call is matched to an account by the user in its Request-URI; with
several accounts, a call to a user none of them has is refused `404`. With one
account every call is taken. Two accounts cannot have the same user.

## Configuration reference

| Key                 | Type        | Default      | Description                                        |
|---------------------|-------------|--------------|-----------------------------------------------------|
| `user`              | `String`    | required     | SIP username                                       |
| `password`            | `String`    | required     | SIP password                                       |
| `realm`             | `String`    | required     | SIP realm / server hostname                        |
| `local-sip-port`    | `Port`      | `5060`       | Local UDP port for SIP                             |
| `server-port`       | `Port`      | `5060`       | Remote SIP server port. 5060 leaves the port to SRV records and the transport's default; any other value is used as it is and skips SRV |
| `base-rtp-port`     | `Port`      | `8000`       | Base RTP port; each concurrent call uses two ports |
| `bind-address`      | `IpAddress` | `0.0.0.0`    | Local bind address                                 |
| `local-ip-fallback` | `IpAddress` | `127.0.0.1`  | Advertised when no IPv4 route to the SIP server is found |
| `hangup-timeout`    | `Duration`  | `5 seconds`  | Grace period for event loop drain on release       |
| `invite-timeout`    | `Duration`  | `32 seconds` | Max wait for remote to answer (RFC 3261 Timer B)   |
| `register-expires`  | `Duration`  | `1 hour`     | Binding lifetime asked for when registering        |
| `answer-timeout`    | `Duration`  | `60 seconds` | How long an incoming call may ring before `480`     |
| `transport`         | `udp`/`tcp`/`tls` | `udp`  | Transport of the default account                  |
| `tcp.connect-timeout` | `Duration` | `5 seconds` | How long to wait for a TCP connection           |
| `tcp.idle-timeout`  | `Duration`  | `5 minutes`  | A TCP connection with nothing sent or received for this long is closed |
| `tcp.keep-alive.interval` / `timeout` | `Duration` | 2 minutes / 10 seconds (off unless set) | Ping a quiet TCP connection with a double CRLF (RFC 5626) and close it if nothing at all comes back within the timeout; pings from the other side are always answered with a pong |
| `tcp.framing.max-header-bytes` / `max-body-bytes` | `Int` | 16 KiB / 1 MiB | Largest message a TCP connection may carry before it is dropped |
| `tls.trust-store` / `tls.client-certificate` | `{ path, password, store-type }` | JVM trust / none | Key stores for the certificates we trust and the one we present (`store-type` is `PKCS12` or `JKS`) |
| `nat.stun.host` / `port` / `retransmit` / `attempts` / `refresh` | name, `Port`, `Duration`, `Int`, `Duration` | none / 3478 / 500 ms / 5 / 5 minutes | A STUN server to ask what address we have outside a NAT |
| `nat.rtp-latching` | `follow`/`ignore` | `follow` | Send media to where the remote party's media comes from, when that is not where its SDP said |
| `codecs` | list of `pcmu`/`pcma` | `[pcmu, pcma]` | The audio codecs to offer and accept, most wanted first |
| `nat.observed-address` | `use`/`ignore` | `use` | Advertise the address servers say they see us at (Contact, SDP) |
| `max-udp-message`   | `Int`       | `1300`       | An INVITE longer than this many bytes goes over TCP instead of UDP |
| `listen`            | list of `tcp`/`tls` | empty | Transports we also accept connections on (UDP is always on) |
| `tls-port`          | `Port`      | `5061`       | Port for the TLS listener                           |
| `tls.server-certificate` | `{ path, password, store-type }` | none | The key and certificate we show to clients that connect to us |
| `tls.client-authentication` | `off`/`request`/`require` | `off` | Whether, as a server, we ask clients for a certificate (checked against `tls.trust-store`) |
| `dns.servers`       | list of `address[:port]` | system | DNS servers to ask for SRV and NAPTR records |
| `dns.timeout`       | `Duration`  | `5 seconds`  | How long to wait for one DNS server                 |
| `tls.min-version`   | `1.2`/`1.3` | `1.2`        | Oldest TLS version accepted                         |
| `tls.cipher-suites` | list        | JVM default  | Cipher suites to allow, by Java name                |
| `tls.hostname-verification` | `verify`/`skip` | `verify` | Whether the certificate must name the host we meant |
| `tls.connection`    | as `tcp`    | as `tcp`     | Connect and idle timeouts and size limits for TLS    |
| `additional-accounts` | list      | empty        | More accounts: each has `user`, `password`, `realm`, and optionally `server-port` and `register-expires` |

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
| `TransportUnavailable(transport)` | A transport other than UDP, TCP or TLS was asked for |
| `TlsConfigurationInvalid(reason)` | A TLS key store cannot be read or has the wrong password |
| `UnknownAccount(account)` | `callAs` or `registerAs` named an account the softphone does not hold |
| `DuplicateAccount(user)` | Two configured accounts share a user |
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
