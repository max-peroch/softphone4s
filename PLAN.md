# PLAN: softphone4s as a complete softphone

Goal: registration, inbound and outbound calls, TLS, SRTP, and robust media, in a purely functional and performant way.
In scope for v1.0, in addition to calls: SUBSCRIBE/NOTIFY (events, MWI), presence (PUBLISH), MESSAGE (instant messaging) and forking (Phases 2 and 5).

No target server is chosen yet. Integration tests use Asterisk and FreeSWITCH in Docker as a stand-in (Phase 6).

## Current state

- Transport: one UDP socket, routed by Call-ID. No transaction layer, no retransmits, no TCP/TLS.
- `CallFsm` is pure but covers only the UAC INVITE/BYE/CANCEL path plus INFO DTMF.
- `SipHeaders` is a fixed record (single Via, single Contact, string URIs).
- `SipDialog` has no remote CSeq and no UAS role.
- Media: G.711 μ-law only, minimal SDP, no offer/answer.
- Auth: digest MD5 only.

## Design principles

### Purely functional

1. **Pure core, effectful shell.** Every protocol machine (transaction, dialog, call, registration) is a pure function `(State, Event) => (State, List[Action])`, as `CallFsm` is today. Interpreters in `F` perform the actions. No `var`, no mutable collections, no `unsafe*` outside tests.
2. **Time and randomness are inputs.** Timers are modelled as `StartTimer(id, duration)` / `CancelTimer(id)` actions and `TimerFired(id)` events. Branch, tag, Call-ID and nonce generation go through a small `Entropy[F]` algebra (backed by `Random[F]`/`SecureRandom`), and clocks through `Clock[F]`. FSM tests are deterministic with no `TestControl` needed for logic.
3. **Shared state only via `Ref`/`Deferred`/`Queue`/`Topic`.** Per-entity state lives in a fiber that owns it (an actor-style loop over a `Queue`) rather than shared `Ref`s where ordering matters.
4. **Resource safety.** Sockets, fibers and registrations are `Resource`s. Cancellation must release everything (tested).
5. **Typed everything.** `SipUri`, `Via`, `CSeq`, `Tag`, `Branch`, `CallId` are opaque types or case classes. No string-typed protocol fields in the public model.
6. **Tagless final at the edges only.** The public API stays `Softphone[F]`/`Call[F]`; internals use concrete algebras (`Transport[F]`, `Entropy[F]`, `Timers[F]`) so tests can swap them.

### Performance

1. **Bytes, not Strings, on the wire.** Parse SIP from `Chunk[Byte]`/`ByteVector` without intermediate `String` copies where practical; encode directly into a pre-sized buffer. Benchmark with JMH (parse/encode throughput, allocation per message).
2. **Cheap routing.** Transaction and dialog tables are `Map`s keyed by case-class/opaque keys, held in a single owning fiber or sharded `Ref`s (no global lock). One fiber per call/transaction, not per message.
3. **Streaming media.** RTP receive/send is an fs2 pipeline over `Chunk[Byte]`, with codecs working on whole chunks (arrays) rather than per-byte `Stream` ops. Reuse buffers where safe. Jitter buffer is a bounded structure with a fixed memory ceiling.
4. **Backpressure and bounds.** All queues are bounded. Overload drops (with a metric) rather than growing: malformed or excess datagrams are dropped before parsing past a size limit (e.g. 64 KiB).
5. **Dedicated blocking pool** for anything blocking (audio device, DNS if the JDK resolver is used).
6. **Measure before optimising.** JMH benchmarks and a load test (N concurrent calls with loopback UAS) gate each phase; regressions fail CI above a threshold.

## Phases

Each phase ships on its own with tests, and updates README and CHANGELOG.

### Phase 0: Foundation refactor (blocks everything) — done

Delivered, with where it differs from the list below:

- Items 1, 3 and 5 as written, plus the headers the stack acts on are typed (`Via`, `NameAddr`, `Tag`, `Branch`, `CallId`, `CSeqNumber`, `MaxForwards`, `MediaType`, Digest challenges and credentials). Unmodelled headers are carried in order as `ExtensionHeader`s. The `Update`, `Prack` and `Refer` methods are still to add, with their phases.
- Item 2: the parser works on bytes, frames streams by `Content-Length`, and reports typed `ParseError`s. It is covered by property-based round trips and by a suite modelled on RFC 4475 (written for this project, not copied, so it is not the RFC's own message set). The JMH baseline is in `softphone4s-bench/BASELINE.md`: parsing an INVITE took about 56 µs and 38 KB, and a first round of profile-guided optimisation brought it to about 28 µs and 30 KB (a response parses in 16 µs, framing in 2 µs).
- Item 4: `Entropy[F]`, `Transport[F]` and `Timers[F, Id]` with `TimerAction` exist, and `CallFsm` now emits timer actions for its invite and BYE timers.
- `softphone4s-core` was **not** split out; see the decision under "Open decisions".

Original list:

1. **Message model:** replace `SipHeaders` with an ordered multi-map plus typed accessors (Via list, Contact list, Route, Record-Route, CSeq, and so on). Add methods: Register, Options, Info, Update, Prack (reserved), Refer (reserved). Keep unknown methods and headers lossless.
2. **Parser/encoder (byte-based):** compact header forms (`v f t i m c l`), folding, multi-value lines, correct `Content-Length` (required for TCP framing), and tolerance for malformed input. Return typed `ParseError`s. Cover with property-based round-trip tests, RFC 4475 torture messages, and JMH benchmarks.
3. **`SipUri` type** with parse/print, params and headers. Remove string URIs from the model.
4. **Algebras:** introduce `Entropy[F]`, `Timers`-as-actions, and a `Transport[F]` trait that the UDP socket implements.
5. **Bump `CallFsm`** onto the new model without behaviour change (existing tests stay green).

### Phase 1: Transaction layer (RFC 3261 §17) — done

Delivered as listed, with these notes:

- The call layer now runs on the transaction layer: INVITE, BYE and CANCEL are client transactions, in-dialog BYE and INFO are server transactions, and `SoftphoneStreaming` routes `incoming` and `unmatched` to calls by Call-ID, answering `481` for unknown dialogs.
- The ACK for a failure left `CallFsm`; a retransmitted 2xx is now ACKed again; the call-level BYE timer is gone (the transaction's Timer F replaces it).
- The CANCEL/200 race used to be dead code, since the event loop stopped at the hangup; the loop now waits for the INVITE's final response after a CANCEL, so a 200 that crosses it is ACKed and ended with a BYE.
- RFC 2543 (no magic-cookie) matching is not supported: such messages go to `unmatched`.
- The transport's reliability is no longer a parameter of `TransactionManager`: each transaction takes it from the transport its top Via names (`Reliability.of`), so UDP and TCP transactions can run side by side.

Original list:

1. Four pure FSMs: INVITE client, non-INVITE client, INVITE server, non-INVITE server. Timers A–K with T1/T2/T4 as configuration.
2. Transaction manager: one owner fiber, matches responses by Via branch plus sent-by plus method, and requests per RFC 3261 §17.2.3 (including RFC 2543 compatibility only if cheap).
3. UDP retransmission is driven by timer actions. Reliable transports (Phase 2) skip retransmits.
4. Move ACK-for-non-2xx and the CANCEL/200 race out of `CallFsm` into this layer.
5. Tests: scripted event sequences against each FSM, plus loss/duplication/reordering simulation with `TestControl`.

### Phase 2: Registration and the UAS role (the core "softphone" phase)

Progress: item 2 (the UAS role) is done too: `InboundCallFsm`, `InboundCall`, `Softphone.incoming` with `IncomingCall` (`ringing`, `accept`, `reject`, `cancelled`), 100/180/200 with the 200 repeated until the ACK, CANCEL (487), BYE and INFO, an SDP answer within the offer, `FailureStatus`, and `DialogRole` with CSeq tracking on the UAS side. Item 7 (multi-account) is done: `AccountConfig`, `additional-accounts` in the configuration, `AccountId`, `Softphone.accounts`/`callAs`/`registerAs`, `IncomingCall.account`, incoming calls matched by Request-URI user (404 when none matches and there are several accounts), and `UnknownAccount`/`DuplicateAccount` errors. Each account has its own credentials, Contact and registration over the one shared socket. Item 6 (forking) is done for the caller: the first 2xx wins, and 2xx from other branches are ACKed and BYEd, including while a re-INVITE is pending or the call is hanging up. Early dialogs are not tracked separately, since only the answering branch matters without PRACK. Item 4 (mid-dialog) is done: the answering side (OPTIONS, re-INVITE and UPDATE from the remote: hold, resume, media move, a delayed-offer re-INVITE answered with our offer and the answer taken from the ACK, our 200 repeated until the ACK) and the offering side (`Call.changeSession`, `hold` and `resume` as a re-INVITE or an UPDATE, `491` glare with the §14.1 retry delays, `408`/`481` ending the call) both work on both call roles. Item 3 (dialogs) is done: `SipDialog.route` computes the request-URI, Route headers and next hop for loose and strict routing (the caller reverses the Record-Route set, the answerer keeps it), `matches` checks the tags, and the caller now validates remote CSeq as the answerer already did; both answer `481` and `500` as RFC 3261 §12.2.2 requires. Contact management beyond our own Contact is not needed yet. Item 5 (authentication) is done: SHA-256 and SHA-256-sess alongside MD5, `qop=auth` and `auth-int`, the strongest algorithm among the offered challenges, `stale`, and a credential cache that lets a registration refresh carry its credentials at once (SHA-512-256 and `userhash` are not supported). Item 1 (registration) is done: a pure `RegistrationFsm`, a `Registration` agent with a state signal, `Softphone.register`, `Expires`/`Min-Expires` typed, and `buildRegister`. Authentication already handles `stale`; SHA-256 (item 5), the UAS role (2), dialog work (3), mid-dialog (4), forking (6) and multi-account (7) remain.

1. **Registration:** `Resource[F, Registration[F]]` with a background refresh fiber (refresh at ~½ expiry with jitter), re-register on failure with exponential backoff, 401/407 retry, `Expires`/`expires=` handling, unregister on release. Expose `registrationState: Signal[F, RegState]` (Unregistered / Registering / Registered / Failed).
2. **Inbound INVITE:** `UasCall` FSM with 100/180/200, `ACK` matching, CANCEL (487), BYE, and 2xx retransmit until ACK (RFC 3261 §13.3.1.4). Public API: `Softphone.incoming: Stream[F, IncomingCall[F]]` with `ringing`, `accept(offer)`, `reject(code, reason)`; an accepted `IncomingCall` yields the same `Call[F]` interface as an outbound call.
3. **Dialog:** remote CSeq tracking and validation, UAC/UAS roles, in-dialog request routing via Route/Record-Route/remote target (loose routing), `Contact` management.
4. **Mid-dialog:** re-INVITE and UPDATE (hold/resume, codec change), glare handling (491 / retry timers), and OPTIONS (reply with capabilities and use for keep-alive).
5. **Auth:** SHA-256 and SHA-256-sess digest (RFC 8760) alongside MD5, `qop=auth`/`auth-int`, `nc`/`cnonce`, `stale`, and a credential cache per realm.
6. **Forking (RFC 3261 §13, §16.7):** a UAC INVITE may receive provisional and final responses from several UASs, each with its own To-tag. Track one early dialog per To-tag, adopt the first 2xx as the confirmed dialog, and ACK then BYE any later 2xx from other forks. Early media from multiple forks is arbitrated by the media layer (Phase 4, item 4): play the first fork that sends RTP, mute the others. The `CallFsm` state `Ringing(ctx, dialog)` becomes `Ringing(ctx, Map[Tag, SipDialog])`.
7. **Multi-account:** `Softphone` holds N accounts, each with its own registration, Contact and credentials. Single-account configuration stays supported.

### Phase 3: Transports and routing

Progress: item 1 is done for TCP and TLS. A message goes out on the transport its top Via names (`RoutedTransport`), `TcpTransport` keeps one connection per destination (reused, idle timeout, retry on a fresh connection, connect timeout), `StreamFraming` cuts the stream on `Content-Length` and skips keep-alive CRLFs, and the endpoint's Via and Contact say `TCP`. The protocol enum is `TransportProtocol` (it used to clash with the `Transport` trait). TLS is a `StreamTransport` over TLS sockets (`TlsTransport`): the server name comes from the message (first Route, else the Request-URI), SNI and HTTPS-style hostname verification are on by default and fail closed for IP literals, `TlsConfig` is typed (trust store, client certificate, protocol floor, cipher suites, `HostnameVerification`), and a connection counts as made only when the handshake has finished, since fs2 starts it lazily and a failure would otherwise be a silently lost message. Item 1 now also accepts connections (`listen`, `StreamListener`; TLS needs a server certificate and can ask for client certificates), and reuses a connection for the responses to what arrives on it and, for TCP, for requests to the address a Via `alias` names (RFC 5923). Still open in item 1: reuse of a TLS connection for requests (it needs the peer's certificate checked against its name). Item 2 (resolution) is done up to the first target: `DnsRecords` (dnsjava for SRV and NAPTR with TTL caching, the system resolver for addresses), `SrvOrdering` (RFC 2782 priority and weights as a pure function of random draws), `Rfc3263` (SRV names, NAPTR service tags, probing order) and `TargetResolver`, which `SoftphoneStreaming` uses for the target of a call and of a registration; `resourceWith` takes a `DnsRecords` for tests. DNS servers are configurable (`dns.servers`), and a dialog's next hop and Record-Route hosts are located the same way as a call's target, over the dialog's own transport. Failover is done: `OutboundCall` holds its endpoint and peer as a swappable `Uplink` with a list of alternatives, and the first INVITE (before any answer) moves to the next on a transaction timeout or transport failure, rebuilt with a new branch and that uplink's Via and Contact (`LocalEndpoint.via`); the INVITE timer allows for every target; `Registration` rotates its uplinks on each failed attempt. Not done: failing over a retried (authenticated) INVITE, and updating the registration context's Contact when the transport changes. Item 4 is done for calls: `Callee` accepts a whole SIP URI (`sips:`, `;transport=`), and a first INVITE over UDP that is longer than `max-udp-message` (1300 bytes) goes over TCP to the same address, with the UDP way as the fallback if the connection fails (the failover machinery). The limits are addressed: the TCP server is looked up for TCP (`_sip._tcp` SRV) rather than assumed to share the UDP port; the ACK, BYE, re-INVITE and UPDATE of a dialog get the same switch (`StreamSwitch` rewrites a built request's top Via and Contact, and a connection that fails falls back to UDP as a new transaction); a `sips:` Request-URI, outgoing or incoming, gets a `sips:` Contact (RFC 5630, `LocalEndpoint.forScheme`). REGISTER has no switch because its content is fixed and a test pins it well under the limit, and responses are not switched because the RFC sends them on the request's transport. A `sips:` INVITE arriving over UDP or TCP is refused `403` in `InboundCallFsm.start` (RFC 3261 §26.2.2 requires TLS on every hop and gives no status code; 403 is our choice) and never reaches the application. Item 3 (NAT) is under way: our UDP requests ask for `rport` and the responses we send fill in `received`/`rport` (`Via.seenFrom`, applied by the transaction layer); the transaction layer reports the address a server says it saw us at (`Via.observedAddress`, believed only from the server a request went to, over UDP); endpoints can be `mappedTo` that address, which changes the Contact and the SDP media address but not the Via; a registration follows the address (`AddressChanged`, plus a reconcile after each success because the address is learned from the same answer the agent is about to process). CRLF keep-alives are done (`tcp.keep-alive`: pings on quiet connections, closed when nothing comes back; pings from the peer are always ponged; a ping sent does not count as activity, anything received does). STUN is done (`stun/Stun` codec, answered on the SIP socket, refreshed, `nat.stun`). The binding at the old Contact is removed once the new one is registered (a REGISTER with `expires=0` and its own Call-ID, only on the same way to the registrar). Requests to a peer reuse the TLS connection it made, for the hosts its validated client certificate names (`Connected.identities`). Phase 3 is complete. Media follows the remote party's RTP source (`RtpLatch`: five packets in a row of one SSRC from one new source; `nat.rtp-latching`), which covers a NAT that remaps the media port; we do not learn our own mapped media port by STUN.  the UDP-to-TCP switch for large messages belongs to item 4.

1. **TCP and TLS** via fs2-io: stream framing with `Content-Length`, a connection pool keyed by destination with reuse (RFC 3261 §18), idle timeout, and reconnect on failure. TLS config is typed (trust store, client cert, SNI, cipher/protocol floor) and defaults to hostname verification.
2. **Resolution (RFC 3263):** NAPTR → SRV → A/AAAA, with failover to the next target on timeout or transport error, TTL-aware cache. Replaces the single-host lookup.
3. **NAT:** `rport` (RFC 3581) and `received`, Contact/Via rewriting from observed addresses, CRLF keep-alive ping/pong and `Flow`-style outbound (RFC 5626) over TCP/TLS, and optional STUN for server-reflexive addresses.
4. **Transport selection** from the URI (`transport=` parameter, `sips:`) and configuration, with the UDP→TCP switch on large messages (>MTU−200, RFC 3261 §18.1.1).
5. WebSocket (RFC 7118) is deferred to an optional module.

### Phase 4: SDP and media

Progress: item 1 is done. Item 2 is under way: `media/AudioCodec` is a codec that makes its own encoder and decoder pipes (so one with memory can keep it there), `G711` has PCMU and the new PCMA, `AudioCodecs` is the set in use (`codecs` in the configuration). What an offer or answer settled on (`MediaPlan`) now decides the payload type, packet size and timestamp step of RTP, and which payload types are received; the dialogs use `Capabilities` that sit on the endpoint, a session change keeps to the codec in use (`MediaSession.restrict`), and an answer we cannot use ends the call. G.722 is done (`codec/G722Codec`, 64 kbit/s, 16 kHz audio: it agrees byte for byte with the public-domain reference implementation on test vectors in `src/test/resources/g722`, kept as data; it holds its state in arrays updated in place so that a call does not load the garbage collector). Mixing it with G.711 in one softphone is refused rather than resampled. Still to do for item 2: Opus as a pluggable codec outside the core. Item 1 reads: `sdp/` has a parser and printer (`SessionDescription`: session and media sections, `c=` per media, IPv4 and IPv6, `rtpmap`/`fmtp`, direction, the `0.0.0.0` hold, declined streams; a description is read even without `o=`, `s=` or `t=`, but not without `v=`) and the pure offer/answer of RFC 3264 (`OfferAnswer.offer`, `answer`, `accept`, with `Capabilities`, `MediaPlan` and a typed `Reject`). It picks our first codec among those offered (found by `rtpmap` or static number), answers telephone events only on the payload type we receive them on, keeps the order of the `m=` lines declining the ones it does not use, and mirrors the direction.

1. **SDP:** full parser and printer (session and media sections, `a=` attributes, `c=` per media), and an offer/answer engine (RFC 3264) as a pure function: `answer(offer, localCaps) => Either[Reject, (Sdp, MediaPlan)]`.
2. **Codec registry** with negotiation: PCMU, PCMA, then G.722 and Opus. Codecs work on `Chunk[Byte]`/`Array[Short]` blocks. Dynamic payload types come from `rtpmap`. Opus may use a JNI-free implementation or a pluggable module. Decide at the start of this phase.
3. **DTMF:** RFC 4733 telephone-event send and receive (including end-of-event retransmission and de-duplication), SIP INFO kept as a fallback.
4. **Hold/resume:** `sendonly`/`recvonly`/`inactive` handling, early media on 183, and comfort-noise handling.
5. **RTP/RTCP:** sequence/timestamp/SSRC handling, SR/RR, jitter and loss statistics, and a bounded adaptive jitter buffer. Reordering and loss concealment are covered by tests with `TestControl`.
6. **SRTP:** SDES key exchange (`a=crypto`) with AES-CM-128/HMAC-SHA1-80 (RFC 3711) and AES-GCM if available, enforced when the transport is TLS. DTLS-SRTP is deferred.
7. **ICE-lite / STUN:** enough for NAT'd peers; full ICE is deferred.
8. **Port allocation:** replace the "base-port + 2n" arithmetic with a pooled allocator in a `Resource` that releases ports on call end.

### Phase 5: Events, presence and messaging

1. **SUBSCRIBE/NOTIFY (RFC 6665):** a generic subscription dialog as a pure FSM (pending / active / terminated), with a background refresh fiber, the NOTIFY server side (200 response, `Subscription-State` handling), and subscribe-side fetch/poll. Public API: `Resource[F, Subscription[F]]` exposing `notifications: Stream[F, Notification]`. A subscription created in response to an INVITE-less SUBSCRIBE and one created implicitly (e.g. by REFER, later) share the same dialog machinery.
2. **Event packages:** `message-summary` (MWI, RFC 3842), `presence` (RFC 3856, PIDF RFC 3863 parsing) and `dialog` (RFC 4235, optional). Packages are pluggable via a typed `EventPackage[A]` that defines the `Event` header value, `Accept` types and body decoder.
3. **Presence publication (RFC 3903):** PUBLISH with entity-tags, refresh, modify and remove, plus own-presence state (`Available`, `Busy`, `Away`, ...) encoded to PIDF.
4. **MESSAGE (RFC 3428):** send and receive pager-mode instant messages as non-INVITE transactions, with content-type negotiation, `Accept` handling, and a `Stream[F, IncomingMessage]` in the API. Delivery results surface as typed errors. Session-mode messaging (MSRP) is not planned.
5. **Auth:** all of the above reuse the Phase 2 digest cache and the Phase 1 transaction layer; no new transaction types are needed.
6. Tests: pure FSM tests for subscription lifecycle, expiry and refresh with `TestControl`, PIDF/message-summary round-trips, and Docker interop for MWI, presence and MESSAGE.

### Phase 6: Hardening, API and docs

1. **Interop CI:** Docker-based Asterisk and FreeSWITCH scenarios: register, outbound and inbound calls, hold, DTMF, transfer-less basics, TLS/SRTP. Add an in-process loopback UAS for fast tests.
2. **Robustness:** fuzz the parser; test packet loss, duplication and reordering; leak tests (no leaked sockets or fibers after cancel or error); long-running soak test.
3. **Performance gates:** JMH for parse/encode and codecs; a load test with ≥500 concurrent loopback calls tracking CPU, allocation and p99 latency; compare against baselines stored in the repo.
4. **Observability:** structured log fields (Call-ID, branch, account), a metrics hook (registrations, active calls, retransmits, packet loss, jitter) with an otel4s implementation in `softphone4s-otel4s`, and traces per call and per transaction.
5. **API stabilisation:** keep `Softphone.call(...)` source-compatible, add `register`, `incoming` and account APIs, adopt MiMa from 1.0, and write a migration guide in `CHANGELOG.md`.
6. **README:** remove the "outbound only" caveat and document registration, inbound calls, TLS and SRTP.

## Module layout (target)

| Module | Contents |
|---|---|
| `softphone4s-core` (pure) | `SipUri`, message model, parser/encoder, FSMs (transaction, dialog, call, registration), SDP and offer/answer, digest auth. No I/O, no `F[_]`. |
| `softphone4s` | Interpreters, transports, DNS, media, public `Softphone[F]` API |
| `softphone4s-otel4s` | Tracing and metrics |
| `softphone4s-demo` | Example app |
| `softphone4s-bench` | JMH benchmarks (not published) |

A split of the pure core was tried and reverted. The idea was to make the "no effects" boundary compiler-enforced, but `ip4s-core` (which provides `Host`, `Port` and `IpAddress`) depends on `cats-effect` at compile scope, so a core module would still have it on its classpath and the rule would rest on a source-scan test. Until a second consumer needs the protocol without the runtime, the pure packages (`model`, `codec`, `fsm`) stay in `softphone4s` and are kept free of `cats.effect` and `fs2` imports (the exceptions are the media codecs, which are being rewritten in Phase 4).

## Order and sizing

- 0 → 1 → 2 is the critical path; 3 (transports), 4 (media) and 5 (events, presence, messaging) can run in parallel once 2 lands.
- Phases 0 and 1 are the riskiest and replace most of the existing ~2k lines. Phase 2 is the largest user-visible win.
- v1.0 = Phases 0–6.

## Deferred (post-1.0, optional modules)

PRACK/100rel, REFER transfer, session timers, WebSocket transport, DTLS-SRTP, full ICE, video, MSRP.

## Open decisions

- Opus implementation (pure Scala/Java port vs. optional native module).
- Whether and when to split `softphone4s-core` (tried and reverted at the end of Phase 0, see "Module layout"); revisit when something besides the runtime wants the pure protocol, or if the parser moves to `scodec.bits.ByteVector`.
- Parser performance (see `softphone4s-bench/BASELINE.md`): what is left after the first round is ip4s hostname validation, `cats` traversal overhead and per-header allocation; decide a target for Phase 6's load test before optimising further.
- Which Docker images and versions pin the interop suite.
