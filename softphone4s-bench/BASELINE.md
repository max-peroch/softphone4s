# Benchmark baseline

JMH benchmarks for the SIP codec. Run them with:

```
sbt "softphone4sBench/Jmh/run -wi 5 -w 2 -i 5 -r 2 -f 1 -prof gc"
```

`-prof gc` adds allocation per operation (`gc.alloc.rate.norm`). Numbers depend
on the machine, so compare runs from the same one.

## Baseline: end of Phase 0

Intel Core i7-4870HQ @ 2.50 GHz (2014 laptop), OpenJDK 25.0.1, Scala 3.9.0.
Messages: a 779-byte INVITE with a 10-line SDP body and a 200 OK to a BYE.

| Benchmark        | µs/op | ± (99.9%) | B/op   |
|------------------|------:|----------:|-------:|
| `parseInvite`    | 56.4  | 10.2      | 38,584 |
| `parseOk`        | 55.6  | 154.7     | 26,096 |
| `frameInvite`    | 9.9   | 10.3      | 6,608  |
| `encodeInvite`   | 6.0   | 7.9       | 6,096  |
| `encodeOk`       | 3.1   | 1.4       | 3,784  |
| `roundTripInvite`| 52.9  | 7.7       | 45,144 |

## Reading it

- Parsing is about 18,000 messages a second on one core of this machine, and an
  800-byte message allocates around 38 KB, so parsing is the cost to attack
  first. Encoding is roughly ten times cheaper.
- `frameInvite` decodes the header section just to find `Content-Length`, which
  is more than framing needs.
- Hypotheses, not yet profiled: per-token regex validation (`UriSyntax.check`),
  regex-based `split`/`replaceAll`, and building a `String` and `List` per
  header line. Profile before changing anything.
- The `parseOk` error interval is wide; rerun before drawing conclusions from
  it.
