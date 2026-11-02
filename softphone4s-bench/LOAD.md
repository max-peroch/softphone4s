# Load and soak baseline

`LoadTest` puts two softphones in one process, calling each other over real UDP
on the loopback interface, with 20 ms audio going both ways in every call. It
needs no server. Run it in a JVM of its own (`run` forks, with `-Xmx2g`):

```
# a burst of calls, held with audio, then hung up
sbt "softphone4sBench/runMain softphone4s.bench.LoadTest load <calls> <hold s> [calls/s [setup p99 limit ms [least fraction heard]]]"

# calls kept going, each ended and replaced, sampled every 30 s
sbt "softphone4sBench/runMain softphone4s.bench.LoadTest soak <concurrent> <minutes> [s per call]"
```

`load` reports, for the calls that were established: setup time (INVITE to ACK)
and hang-up time (BYE to 200) as p50, p95, p99 and max; the fraction of the audio
sent that arrived, per call; the process's CPU (cores busy on average),
collections and allocation; and open files, threads and retained heap. It exits
with 1 if a call fails or a limit given on the command line is broken, so it can
gate a change: `load 20 10 50 500 0.95` fails if any of 20 calls fails, if the
slowest percent take over 500 ms to set up, or if the median call hears under 95%
of the audio.

`soak` prints a line every 30 s and ends with the drift of open files, threads and
retained heap (after a collection) between its first sample and its last; it
exits with 1 if a call fails or one of them drifts more than 20 files, 20 threads
or the larger of 64 MB and the heap at the start.

Both phones share the one JVM, so every call is handled twice: a phone that only
dialled, or only answered, would take about half as much. Thirty calls are made
first, and not counted, to warm the JIT.

## Baseline: end of Phase 5 (commit `262c04c`)

Intel Core i7-4870HQ @ 2.50 GHz (4 cores, 8 threads, a 2014 laptop), OpenJDK 25.0.1,
Scala 3.9.0, `-Xmx2g`, calls started at 50 a second and held 10 s.

| Calls | Cores busy | Setup p50 / p99 (ms) | Hang-up p99 (ms) | Audio heard (median) | Allocated |
|------:|-----------:|---------------------:|-----------------:|---------------------:|----------:|
| 10    | 2.1        | 14 / 16              | 11               | 0.99                 | 150 MB/s  |
| 20    | 3.6        | 15 / 108             | 9                | 0.99                 | 290 MB/s  |
| 40    | 5.2        | 82 / 462             | 8                | 0.98                 | 530 MB/s  |
| 60    | 6.0        | 406 / 1 226          | 267              | 0.92 to 0.94         | 710 MB/s  |
| 80    | 5.7        | 696 / 3 311          | 69               | 0.89 to 0.90         | 760 MB/s  |
| 100   | 4.8        | 8 900 / 15 030       | 5 100            | 0.74 to 0.78         | 505 MB/s  |

Every call was established in every run. The machine is saturated between 40 and
60 calls: setup times grow, and audio is lost, though nothing fails.

Soak, 15 calls at a time of 6 s for 2 minutes: 300 calls, none failed; open files
130 throughout, threads 29 then 17, retained heap 21 to 22 MB. No drift.

## What the numbers say

- **A media packet costs about 0.7 ms of CPU and about 75 KB of allocation.** From
  10 to 20 calls the process takes 1.5 more cores, for 10 more calls of 200 packets
  a second each (two legs, 50 sent and 50 received). An RTP path should cost a few
  tens of microseconds a packet. So this machine holds about 40 calls with both
  ends in the process, and the target of 500 calls would need about 36 cores at
  this efficiency. It is the main thing to do for performance, and nothing in it
  says it is a limit of the protocol.
- **It is not cats-effect's tracing.** `-Dcats.effect.tracing.mode=none` against
  the default gave 5.37 against 5.43 cores and 5.75 against 6.07 GB allocated for
  40 calls.
- **A Flight Recorder profile does not show where it is.** 86% of the CPU samples
  have no frame of ours in the 60 deepest: a fiber has no logical stack, so the
  time shows as the cats-effect interpreter (`IOFiber.runLoop` 15%) and fs2's
  `Pull`. The next step is to measure the media path in pieces (JMH of receive,
  jitter buffer, playout tick and send, each with its streams and refs) and to cut
  what is done per packet.
- Nothing is left behind: open files return to 70 after every run, and the heap
  retained after a collection stays at 12 to 13 MB.
