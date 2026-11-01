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

// optional: the Opus codec (needs libopus installed on the machine)
libraryDependencies += "io.github.max-peroch" %% "softphone4s-opus" % "<latest version>"
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
- **Codecs**: G.711 μ-law (`pcmu`) and A-law (`pcma`), and G.722 wideband
  (`g722`, 64 kbit/s). `codecs` lists the ones
  to use, most wanted first (default `["pcmu", "pcma"]`): our offers list them
  in that order, and an answer takes the first of them that the offer has,
  whatever order the offer lists theirs in. The codec an answer settled on, and
  its payload type, are what media is sent and received in; packets of any
  other payload type are ignored. Within a call the codec stays: a re-INVITE or
  UPDATE that lacks it is refused with `488`. The codecs have to work at the
  sample rate of the audio format you give (8 kHz for G.711, 16 kHz for
  G.722); anything else is an error when the softphone starts, not a conversion,
  so G.722 is a choice made for the whole softphone, with a 16 kHz format and
  `codecs = ["g722"]`, and cannot be listed beside G.711. Our G.722 agrees with
  a reference implementation bit for bit.
- **Adding a codec**: a codec the library does not ship, Opus for instance,
  is an implementation of `softphone4s.media.AudioCodec` that you pass to
  `SoftphoneStreaming.resource(..., extraCodecs = List(myCodec))` and name in
  `codecs`. It says what SDP calls it (`description`, with a static payload
  type or none, in which case the caller's or a dynamic number is used), the
  `token` the configuration calls it by, the sample rate of the PCM it works at,
  how many bytes a 20 ms packet is in PCM and in payload, and an `encoder` and a
  `decoder` pipe (`AudioCodec.stateless` and `AudioCodec.stateful` make them from
  a function over a packet, or a `Resource` of one, which is how a codec with
  memory or native state makes it for each stream and releases it; a packet is a
  chunk of its own, so packets may differ in length). A name that neither we nor
  you supplied, and two codecs of one name, are errors when the softphone
  starts.
- **Opus** (RFC 6716, RFC 7587): the `softphone4s-opus` module, which calls
  libopus through the JDK's Foreign Function & Memory API (JDK 22 or later): no
  JNI to build and no extra dependency, but libopus has to be installed
  (`apt install libopus0`, `brew install opus`) or its path given, and the JVM is
  best started with `--enable-native-access=ALL-UNNAMED`. It is mono, with
  in-band FEC, an optional bit rate and complexity, and one of 8, 12, 16, 24 or
  48 kHz for the audio, which has to be the audio format's (8 kHz lets it share a
  softphone with G.711, 16 kHz with G.722). SDP names it `opus/48000/2` as
  RFC 7587 requires, whatever the audio rate, and asks for mono. A packet that
  cannot be decoded is dropped rather than ending the call.

  ```scala
  OpusCodec.resource[IO](OpusSettings(sampleRate = OpusSampleRate.Hz16000)).flatMap { opus =>
    SoftphoneStreaming.resource[IO](
      config.copy(codecs = List(CodecChoice.Named("opus"))),
      new AudioFormat(16000f, 16, 1, true, false),
      logger,
      List(opus)
    )
  }
  ```
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
- **Jitter buffer**: what arrives is not played at once but put in order. A
  buffer holds `jitter-buffer.depth` frames of 20 ms (3 by default,
  so audio starts about 60 ms after the first packet) and room for `capacity`
  (12): packets that come in a different order are put right, a duplicate is
  dropped, and one that comes after its turn is dropped too. A packet that never
  comes is concealed by repeating the frame before it, quieter each time and
  silent after 60 ms. When the buffer runs dry, as at the end of speech, it
  fills again before playing on. Telephone events and comfort noise take their
  place in it too, so that they are neither taken for lost packets nor applied
  before the speech ahead of them; a digit is still reported the moment it
  arrives. A depth of 1 is the least delay, 20 ms. The cushion adapts to the
  network: a packet that comes after its turn, or a pause in the packets that
  the next packet does not announce as the start of speech (no marker bit),
  shows it was too small, and it grows by a frame, up to `max-depth` (twice the
  depth, or as far as `capacity` allows, if not said), the next time the buffer
  fills. After 10 seconds of smooth playing it gives a frame back, down to
  `depth`. `max-depth = depth` keeps it fixed. `MediaSession.jitterStats`
  tells the cushion held now.
- **RTCP** (RFC 3550): a call sends a report every 5 seconds or so (each wait
  between half and one and a half times that, the first after half of it) to the
  port after the other side's RTP port, and reads theirs from the port after
  ours. The report is a sender's once we have sent audio and a receiver's until
  then, with a block for what we heard (the highest sequence number, packets
  lost, interarrival jitter, counted as the packets arrive and before the
  jitter buffer reorders any), who we are (`softphone4s@address`), and a goodbye
  when the call ends. `MediaSession.peerReport` is what the other side last
  said of what we send. Reports are believed only from the host we send media
  to, and none are sent while the call is on hold. RTCP is best effort: a
  failure to send one, or an RTCP port that is taken, leaves the call without
  reports and does not end it. `a=rtcp` and `rtcp-mux` are not read yet.
- **SRTP** (RFC 3711, keys by SDES, RFC 4568): `srtp = off | optional | required`
  (off by default). Each call makes its own keys, one per suite
  (`AES_CM_128_HMAC_SHA1_80`, then `_32`), and keeps them over its session
  changes. With `required` the INVITE offers `RTP/SAVP` and only an encrypted
  answer is taken: an answer in plain RTP, or in secure RTP with no key we can
  use, ends the call. With `optional` the offer is plain `RTP/AVP` with the keys
  beside it, and the call is encrypted if the answer takes one. We answer an
  offer the same way, in the profile it used, with the first of its keys in a
  suite we speak. Everything RTP carries is encrypted and authenticated,
  telephone events included; a packet that does not authenticate, or is a
  replay, is dropped before the jitter buffer sees it. Keys never appear in a
  log. Not supported: master key identifiers, key lifetimes (read and ignored),
  `KDR=` and other session parameters (a line with one is not used), and a key
  change in mid-call (the other side must keep its key over a re-INVITE). RTCP
  is encrypted too (SRTCP, with keys of its own derived from the same master key; reports
  that do not authenticate are ignored); AES-GCM and DTLS-SRTP are not
  offered.
- **Comfort noise** (RFC 3389), received: we offer `CN/8000` (payload type 13)
  beside the codecs, and answer it when it is offered (on the number it was
  offered on; other rates are not taken). A party that stops sending speech to
  save bandwidth sends the level of its background noise instead, and we play
  white noise of that level into `call.source` so that the pauses do not sound
  like a dead line. The noise ends when speech comes, when the other side stops
  sending (a hold), or thirty seconds after the last news of it. The spectrum a
  packet may carry is ignored, and we do not send comfort noise ourselves: we
  do not suppress silence yet.
- **Early media** (RFC 3960): a `180`, `183` or other provisional response
  that carries an SDP answer to our offer starts the media before the call is
  answered, so ringback or an announcement the other side plays comes out of
  `call.source`, and what is put into `call.sink` is heard by it. The `200` then
  carries on with the same media, pointed at the address and direction it gives;
  if it chooses another codec than the early answer did, which cannot be
  switched to in mid-stream, the early codec is kept and a warning is logged. A
  provisional response from another party than the first (a fork) has its media
  ignored. A call that ends without being answered lets go of its media socket.
  Provisional responses are not sent reliably (PRACK, RFC 3262, is not done), so
  a `183` lost on the way is lost.
- **Digits (DTMF)**: `call.sendDtmf(digit, duration)` sends a digit lasting
  from 20 ms to 8 s (160 ms by default) and says how: in the media as
  telephone events (RFC 4733) when the other side agreed to receive them and the
  call is not on hold, otherwise in a SIP INFO (`application/dtmf-relay`). As
  events, a packet goes out every 20 ms with the time so far, the first marked,
  and the last packet three times with the end bit so that losing one does not
  lose the digit; they share the audio's SSRC and sequence numbers and carry
  the timestamp the digit began at, and the audio is held back while a digit
  is on the wire. A digit by INFO is sent only when the call is not changing
  its session; one asked for then is not sent. It fails with `CallNotEstablished`
  before the call is answered. Digits received, from events (once each, however
  many of the repeated end packets arrive, and also when only those get
  through) or INFO (`Signal=5`, `*`, `#`, `A` to `D`, or the event number),
  come on `call.dtmfEvents`.
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

### Sending a message

`phone.messages.send(callee, MessageContent.plain("hello"))` sends an instant
message (RFC 3428) as a MESSAGE outside any dialog and says whether it was
delivered. A 2xx is delivery; anything else is a `MessageFailure`, a value and
not an exception:

```scala
phone.messages.send(callee, MessageContent.plain("hello")).flatMap {
  case Right(())                                  => IO.println("delivered")
  case Left(MessageFailure.Rejected(404, _))      => IO.println("no such user")
  case Left(MessageFailure.UnsupportedContent(t)) => IO.println(s"takes only $t")
  case Left(other)                                => IO.println(other.message)
}
```

A challenge (401 or 407) is answered once with the account's credentials; if
that is refused too the result is `Unauthorized`. When a way of reaching the
recipient gets no answer or cannot be used, the next one found by DNS is tried; a
refusal is final. The content is text, sent as UTF-8; `MessageContent` takes any
type (`text/plain` is the one every phone reads). `messages.sendAs` sends as one of
several accounts.

### Receiving messages

`phone.messages.incoming` is the stream of instant messages that arrive for any of
the accounts, in order; `IncomingMessage.account` says which, `from` who sent it
and `content` what it says:

```scala
phone.messages.incoming
  .evalMap(m => IO.println(s"${m.from.uri.render}: ${m.content.text}"))
  .compile
  .drain
```

A message is answered `200` once the stream holds it, so a sender that sees it
delivered knows the program has it. The others are answered without reaching the
program: `404` if no account is the one it is for, `400` if it has a body and
does not say what it is, `415` (listing `text/plain`) for any other type or for a
character set other than UTF-8 or US-ASCII, and `503` if 64 are already waiting
for a reader. A retransmission is answered again and appears once.

### Subscribing

`phone.subscriptions.subscribe(target, event)` subscribes to the state of a
resource (RFC 6665) for as long as the resource is held. Acquiring it sends the
SUBSCRIBE and returns at once; `awaitActive` says when the notifier has agreed:

```scala
val presence = EventHeader("presence")            // Either[String, EventHeader]

phone.subscriptions.subscribe(bob, presence, accept = List(pidf)).use { sub =>
  sub.awaitActive >>
    sub.notifications                             // what each NOTIFY brings
      .evalMap(n => IO.println(s"${n.state}: ${n.body}"))
      .compile
      .drain
}
```

The subscription is refreshed at half its lifetime (a refresh that fails is
retried while the lifetime runs). When it ends it is begun again, in a new
dialog, if the way it ended allows: at once after `deactivated`, `timeout`, an
expired lifetime or a lost dialog; after the `retry-after` for `probation` and
`giveup`; after a growing wait (5 s, doubling) when it could not be had; never
after a refusal, `rejected` or `noresource`. `sub.status` says where it stands,
including `Retrying(because, in)`, and `awaitActive` gives `Left(ending)` if it
ended for good first. `notifications` keeps the 64 most recent for a slow reader
and ends when the subscription does. Releasing the resource ends the
subscription with a SUBSCRIBE for nothing and waits for the last NOTIFY.

`subscribeAs` subscribes as one of several accounts, and `expires` asks for a
lifetime other than an hour. The body of a NOTIFY is text that an event
package reads.

#### Event packages

An `EventPackage[A]` is a value that names the `Event`, the `Accept` types and
how a NOTIFY becomes an `A`; `phone.subscriptions.watch(target, pkg)` subscribes
to it and gives the states:

```scala
phone.subscriptions.watch(mailbox, MessageSummary.Package).use { watched =>
  watched.states.evalMap {
    case Right(summary) => IO.println(s"waiting: ${summary.waiting}, ${summary.counts}")
    case Left(error)    => IO.println(error.message)   // e.g. the final NOTIFY, with no body
  }.compile.drain
}
```

A NOTIFY that cannot be read is a `Left(PackageError)` and does not end the
stream. `MessageSummary.Package` (RFC 3842, message waiting) is the first; a
program can define its own.

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
| `base-rtp-port`     | `Port`      | `8000`       | First RTP port of the range media is received on; a call takes a pair (RTP on an even port, RTCP on the next) for as long as it lasts |
| `max-rtp-port`      | `Port`      | base + 9999  | Last port of the range (5 000 calls at once by default, as far as ports go) |
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
| `srtp` | `off` / `optional` / `required` | `off` | Whether media is encrypted with SRTP (keys exchanged in the SDP, so the signalling should be on TLS) |
| `jitter-buffer.depth` / `max-depth` / `capacity` | `Int` | 3 / 6 / 12 | Frames of 20 ms held before playing, how far that grows when the network is bad (`max-depth = depth` for a fixed cushion), and the most ever held |
| `codecs` | list of names: `pcmu`, `pcma`, `g722`, or one you add | `[pcmu, pcma]` | The audio codecs to offer and accept, most wanted first |
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
| `RtpPortsExhausted(first, last)` | Every pair of RTP ports in the range is in use or cannot be bound (too many concurrent calls) |
| `RtpPortsInvalid(reason)` | `base-rtp-port` and `max-rtp-port` leave no pair of ports |

`awaitPickup` raises the error as an effect if the call is rejected or
times out before being answered.

## Architecture

```
SoftphoneStreaming          — the public API; a thin facade over the parts below
├─ IncomingRouter           — what arrives: to its call, to a new call (INVITE),
│                             to the message queue (MESSAGE), or refused
├─ Uplinks                  — where a name leads (RFC 3263), the next way to try,
│                             the way over TCP for a big request
├─ EndpointFactory          — the endpoint of a request: local address as the
│                             network sees it, RTP ports, SRTP keys
├─ RoutedTransport          — UDP / TCP / TLS, chosen by the top Via
├─ TransactionManager       — RFC 3261 §17 client and server transactions
├─ Registration             — RegistrationFsm, refresh, backoff, NAT follow
├─ MessageSender            — MESSAGE, over StandaloneRequest (one challenge)
└─ per call
   ├─ OutboundCall / InboundCall — CallFsm / InboundCallFsm
   └─ MediaSession
      ├─ RtpTransport, RtpSender  — RTP (SRTP when negotiated), digits
      ├─ JitterBuffer, Concealment — playout; comfort noise
      ├─ RtpReception, reports     — RTCP (SRTCP when negotiated)
      └─ AudioCodec                — G.711, G.722, Opus (plug-in)
```

The packages `model`, `codec`, `fsm`, `sdp`, `srtp` and `messaging` are pure:
they import no `cats.effect` and no `fs2` stream, pipe or I/O (`fs2.Chunk` is
data and is allowed). A test (`PuritySuite`) scans them and fails if that
changes.

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

The G.722 codec follows ITU-T G.722; its test vectors were made with the
public-domain implementation by Steve Underwood, itself based on the Carnegie
Mellon ADPCM program (Copyright 1993 Chengxiang Lu and Alex Hauptmann), whose
authors ask for it to be acknowledged. None of that code is in this repository.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
