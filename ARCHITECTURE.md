# Architecture

How softphone4s is built, and why. For what it does see [README.md](README.md);
for what changed, [CHANGELOG.md](CHANGELOG.md) and [MIGRATION.md](MIGRATION.md);
for how fast it is, [softphone4s-bench/LOAD.md](softphone4s-bench/LOAD.md). The
rules for working on it are in [AGENTS.md](AGENTS.md).

## Modules

| Module | Contents |
|---|---|
| `softphone4s` | The library: protocol machines, transports, DNS, media, and the public `Softphone[F]` API |
| `softphone4s-otel4s` | OpenTelemetry tracing and metrics (`TracedSoftphone`, `OtelMetrics`, `OtelTransactionTracing`) |
| `softphone4s-opus` | The Opus codec, through libopus and the Foreign Function & Memory API (Java 22 and later) |
| `softphone4s-demo` | An example application |
| `softphone4s-bench` | JMH benchmarks and the load and soak test (not published) |
| `softphone4s-interop` | Tests against Asterisk in Docker (not part of the default build) |

## Packages of `softphone4s`

The package names say what is in them. The pure ones, which import no effect,
are marked.

| Package | What |
|---|---|
| `model` (pure) | SIP messages, URIs, headers and the typed fields of both |
| `codec` (pure) | Encoding and decoding of SIP, SDP, RTP, RTCP, STUN, G.711, G.722 and digest authentication |
| `fsm` (pure) | The state machines of calls |
| `transaction` | The transaction layer of RFC 3261 §17: pure machines, and the engine that runs them |
| `transport` | UDP, TCP and TLS, stream framing, the RTP sender and receiver |
| `dns` | Name resolution of RFC 3263 |
| `registration`, `ua` | Registration, calls, the media session, subscriptions and publications as they run |
| `media` | The jitter buffer, concealment, comfort noise, RTCP bookkeeping, RTP ports, codec registry |
| `sdp`, `srtp` (pure) | Offer and answer, and SRTP and SRTCP |
| `messaging`, `subscription`, `publication` (pure) | MESSAGE, SUBSCRIBE and NOTIFY, and PUBLISH as machines and types |
| `presence`, `dialog`, `xml` | The documents of the `presence` (PIDF, RPID) and `dialog` event packages, and the XML reading they share |
| `stun` | The STUN client, and the probe that asks about a call's RTP port |
| `metrics`, `logging` | The metrics hook, and the names of the log fields |
| `config`, `runtime` | Typed configuration, and timers |
| (root) | `Softphone`, `SoftphoneStreaming` and its services and routing |

## Design principles

### Purely functional

1. **Pure core, effectful shell.** Every protocol machine (transaction, dialog,
   call, registration, subscription, publication) is a pure function
   `(State, Event) => (State, List[Action])`. Interpreters in `F` perform the
   actions. No `var`, no mutable collections, and no `unsafe*` outside tests,
   except where marked (the hot loops of a codec, and a codec's own predictor).
2. **Time and randomness are inputs.** Timers are `StartTimer` and `CancelTimer`
   actions and `TimerFired` events. Branches, tags, Call-IDs and nonces come from
   an `Entropy[F]`, and clocks from `Clock[F]`. A machine's tests are
   deterministic and need no virtual time; the code around them is tested with
   `TestControl`.
3. **Shared state only through `Ref`, `Deferred`, `Queue` and the like.** Where
   order matters, the state lives in one fiber that owns it (the transaction
   engine is a loop over a queue), and is held in a `Ref` only where something
   else, such as a finalizer, must reach it.
4. **Resource safety.** Sockets, fibers, registrations and calls are
   `Resource`s. Cancelling releases everything, and the tests check that nothing
   is left behind.
5. **Typed everything.** `SipUri`, `Via`, `CSeq`, `Tag`, `Branch`, `CallId`,
   `Secret`, `Attempts` and the rest are opaque types, enums or case classes with
   smart constructors. Failures are values (`Either`, enums of errors), never
   thrown.
6. **Tagless final at the edges only.** The public API is `Softphone[F]` and
   `Call[F]`; inside, concrete algebras (`Transport[F]`, `Entropy[F]`,
   `Timers[F]`, `Metrics[F]`) are what tests swap.

### Performance

1. **Bytes on the wire.** SIP is parsed from `Chunk[Byte]` without copies into
   `String`s where that is practical.
2. **Cheap routing.** Transaction and dialog tables are maps keyed by opaque keys,
   owned by one fiber. One fiber per call and per transaction, not per message.
3. **Streams for data, loops of effects for ticks.** Media moves as chunks.
   Where a stream cost more than the work (merging a tick stream with a packet
   stream made every frame pay a handshake between fibers), the code is a loop of
   effects, as `JitterPlayout.run` is.
4. **Bounds everywhere.** Queues, bodies, connections and what a decoder will
   read are bounded, and a datagram that is too large or malformed is dropped. The
   decoders are fuzzed and fed hostile input (`softphone4s/.../robustness`).
5. **Measure before changing, and measure the right thing.** `softphone4s-bench`
   has JMH benchmarks and a load test whose exit code can gate a change. CI does
   not run the load test. Most of the CPU of a call is the scheduler waking
   threads, not the work of a packet: see `LOAD.md`, which also says how to size
   the compute pool.

## Why there is no `softphone4s-core`

A split of the pure packages into a module of their own was tried and reverted.
The aim was a boundary the compiler enforces, but `ip4s-core` (which provides
`Host`, `Port` and `IpAddress`) depends on `cats-effect` at compile scope, so such
a module would still have it on its classpath. The pure packages therefore stay in
`softphone4s`. They import no `cats.effect` and no `fs2` stream, pipe or I/O
(`fs2.Chunk` is data and is allowed), and `PuritySuite` scans them and fails if one
does. Revisit it if something besides the runtime wants the protocol on its own.

## Left out on purpose

PRACK (`100rel`), REFER and call transfer, session timers, a WebSocket
transport, DTLS-SRTP, AES-GCM in SRTP, a peer changing its SRTP key in mid-call,
early media on incoming calls, video, MSRP, and full ICE. ICE-lite was scoped
and parked: it needs a peer that requires it, and it would not give us our own NAT
traversal (STUN on the SIP socket and on each call's RTP port, and RTP latching,
do that).

## History

The phased plan this grew from, with a log of each piece, was `PLAN.md`. It is in
the history of the repository: `git log --follow -- PLAN.md`, and
`git show <commit>:PLAN.md`.
