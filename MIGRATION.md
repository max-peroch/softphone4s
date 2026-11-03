# Migrating from 0.1.0

0.1.0 placed calls. What came after it receives them too, registers, sends and
receives messages, subscribes and publishes, and talks over TCP, TLS and SRTP,
so a good part of the API had to change. Before 1.0 a release may break
compatibility; this is what a program written for 0.1.0 has to change, taken
from `mimaReportBinaryIssues` against the published 0.1.0 and checked against the
code.

Both published modules are affected differently: `softphone4s-otel4s` has no
binary incompatibility with its 0.1.0 (`TracedSoftphone` is where it was), but it
wraps a `Softphone`, so what follows applies to it too.

## Configuration

`SoftphoneConfig` holds validated types where it held strings:

| 0.1.0               | now                                                     |
|---------------------|---------------------------------------------------------|
| `user: String`      | `user: User`, from `User("alice")`, an `Either[String, User]` |
| `password: String`  | `password: Secret`, from `Secret("...")`; prints as `***`, `.reveal` gives the text |
| `realm: String`     | `realm: Host`, from `host"sip.example.com"` (`import com.comcast.ip4s.*`) |

```scala
// 0.1.0
SoftphoneConfig(user = "alice", password = "secret", realm = "sip.example.com")

// now
User("alice").map(user =>
  SoftphoneConfig(user = user, password = Secret("secret"), realm = host"sip.example.com")
)
```

A configuration file keeps its keys (`user`, `password`, `realm`, ...) and its
text; a value that is not valid now fails when the file is loaded, with a message,
instead of failing later. Many keys are new and all have defaults.

`hangupTimeout` and `inviteTimeout` were `FiniteDuration`. They, and every timeout
and wait added since, are now `PositiveDuration`: a length of time above zero.
Zero would make a timeout fire at once, and a refresh that waits for it spin.

```scala
// 0.1.0
SoftphoneConfig(..., inviteTimeout = 30.seconds)

// now: an Either, since the value may be zero or negative
PositiveDuration(30.seconds).map(t => SoftphoneConfig(..., inviteTimeout = t))
// read it back with `.value`, a FiniteDuration
```

A configuration file is unchanged (`invite-timeout = 30 seconds`); a zero or a
negative value is a load error. `StunClient.bindingRequest` and
`SipTransport.bindingRequest` take `attempts: Attempts` where they took an `Int`.

The class is now `final`, and fields were added between the old ones
(`maxRtpPort` after `baseRtpPort`, and others), so build it with named arguments:
`copy`, `apply` and `unapply` have different shapes.

## Using a phone

- `SoftphoneStreaming.resource` takes two more parameters, `extraCodecs` and
  `metrics`, both with defaults; a call that passes the first three arguments
  compiles as before.
- `Softphone` and `Call` have more members (`incoming`, `registerAs`, `callAs`,
  `accounts`, `messages`, `subscriptions`, `publications`; `sendDtmf` and
  `changeSession` on `Call`). Using them changes nothing; a class that
  *implements* either (a fake in a test) has to implement them.
- `call(callee, extraHeaders)` and the members of `Call` that existed are
  unchanged.

## Errors

- `SoftphoneError.InvalidHostname` is gone: a host is validated when the
  configuration is built, and a name that does not resolve is
  `HostResolutionFailed`, as before.
- `SoftphoneError.RtpPortExhausted(port)` is now `RtpPortsExhausted(first, last)`:
  RTP ports come from a pool between `baseRtpPort` and `maxRtpPort` (by default
  `baseRtpPort + 9999`), are given back when the call ends, and a call fails with
  this error when all are in use.
- Many cases were added (`TransportFailed`, `ConnectTimedOut`, `UnknownAccount`,
  ...). A `match` on `SoftphoneError` that was exhaustive is not any more.

## Behaviour

- A phone now accepts calls: one made to its address is offered on
  `phone.incoming`, and turned away with `480` if it is not answered within
  `answerTimeout` (60 s by default). A phone with one account takes calls for any
  user at its address.
- Nothing registers until `phone.register` (or `registerAs`) is used.
- The default transport is still UDP; TCP and TLS come with `transport`,
  `listen`, a `sips:` URI or a `transport=` parameter.

## What is not API

`model.SipHeaders`, `model.SipRequest`, `model.SipDialog`, `model.InviteCtx`,
`fsm.*`, `codec.*` and `transport.*` are public in the code but are the
machinery, not the interface, and changed freely (a hundred of the reported
incompatibilities are there). A program that used them directly has to be read
against the source; they are not listed here one by one.

## Dependencies

The library is compiled for Java 17 and later. It uses `dnsjava` for DNS and
`fs2-data-xml` for presence documents. `softphone4s-otel4s` is built against
OpenTelemetry Java 1.64.0, which is what otel4s 1.1.0 needs: a newer
`opentelemetry-api` (1.67.0 for one) removes a class it uses, so do not pin it
above that.
