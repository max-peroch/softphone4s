# Load and soak baseline

`LoadTest` puts two softphones in one process, calling each other over real UDP
on the loopback interface, with 20 ms audio going both ways in every call. It
needs no server. Run it in a JVM of its own (`run` forks, with `-Xmx2g`):

```
# a burst of calls, held with audio, then hung up
sbt "softphone4sBench/runMain softphone4s.bench.LoadTest load <calls> <hold s> [calls/s [setup p99 limit ms [least fraction heard]]] [silent]"

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

`silent` holds the calls open and sends no audio: what is left is the cost of a call
apart from its media packets (the playout ticks, the timers, the reports).

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

## What was ruled out, and what an allocation profile says (2 November 2026)

**fs2's datagram read buffer is not the cost.** `SelectingDatagramSocket.read`
(the default `Network[IO]`) allocates a zero-filled 64 KiB buffer, and a JMH
benchmark of calling `read` once per datagram measured 70,739 B/op against 3,344
with a reused buffer. The media code does not call `read` per datagram, though: it
uses `socket.reads`, which is `Stream.repeatEval(read)`, and `repeatEval` takes the
effect by value, so the buffer is allocated once per stream and reused. Moving the
RTP and RTCP sockets to `Network.forAsync` (fs2's other implementation, with one
buffer per selector thread) made no difference where it matters: with 5 calls held
20 s, everything heard, the process allocated 1 523 and 1 525 MB before and 1 517
and 1 488 MB after, and used 1.21 to 1.26 cores before and 1.42 to 2.15 after. A
JFR profile of the unchanged code puts `ByteBuffer.allocate` at 0.13% of sampled
allocation. (That benchmark measured an API the code does not use; it was dropped.
Benchmark the call that is made.)

**Where the allocation is.** JFR's allocation samples of the 40-call run (6.2 GB
sampled): the classes are `IO$FlatMap` 7.8%, `IO$Delay` 7.0%, `Object[]` 5.3%,
`byte[]` 3.3%, `IO$Pure` 3.0%, `IO$Uncancelable` 2.5%, then more of the same
(`IOFiber`, `ArrayStack`, `Tuple2`, `Right`, fs2 `Pull` lambdas). At least 43% of
the sampled allocation has an `IOFiber` frame, and at least 16% an fs2 `Stream` or
`Pull` frame (the stack is cut at 64 frames, so these are lower bounds). None of
our own code is above 1%: `MediaSession` 1.0%, the jitter buffer 0.7%, `RtpSender`
0.7%. So the cost is in the number of effect steps and stream combinators per
packet, not in any one thing a packet does. A 5-call run receives about ten
thousand packets (a probe counted 10 568 reads, for 1.5 GB allocated), so about
140 KB per packet sent and received, or 70 KB per packet-op as above.

A profile cannot attribute CPU to our code, because a fiber has no logical stack,
so the next step was an experiment that can: the `silent` option, which holds calls
open without sending audio. That is the next section.

**Measuring on a busy machine.** The numbers in the table above need an idle
machine. On 2 November the load average was 25 to 40 (other programs at 97% CPU),
and two identical 40-call runs gave a setup p50 of 805 and of 158 ms and 0.75 to
0.85 of the audio heard, against 82 ms and 0.98 at the baseline. Allocation (5 486
and 5 514 MB) and CPU time (5.54 and 5.56 cores) did not move, so compare those,
at a load light enough for everything to be heard (`load 5 20 50`), and look at
`uptime` first.

## An idle call was the cost, not the packets (2 November 2026)

Holding 10 calls for 20 s with nobody speaking (`load 10 20 50 silent`) cost
0.84 to 0.96 cores and 1 068 to 1 079 MB, against 1.80 to 1.93 cores and 3 011 to
3 040 MB with audio. Half the CPU and a third of the allocation had nothing to do
with media packets. Divided by the number of 20 ms playout ticks (two phones per
call), an idle call cost about 0.9 ms of CPU and 48 KB per tick, all its costs
counted, not the tick alone. The old reading of this file, "a media packet costs
0.7 ms", had put that on the packets.

The tick was `packets.mergeHaltL(ticks).mapAccumulate(...)`. fs2's `merge` runs both
streams in fibers of their own and hands every element across, so each tick, packet
or not, paid for a handshake. `JitterPlayout` now keeps the buffer in a `Ref`: the
receive side pushes into it, and the tick pulls from it. Same behaviour (the
playout tests are unchanged and pass; it still ends when the packets do, an error
in them is its error, and stopping it stops the feed: two tests added), no merge.

Taking the merge out cut an idle call's CPU by 35 to 40% and its allocation by 60%,
and a call with audio's CPU by 25 to 30% and its allocation by half. Before and
after on the same machine and method (quiet enough for everything to be
heard, load average about 7):

| Calls | Before: cores, MB allocated | After: cores, MB allocated |
|------:|-----------------------------|----------------------------|
| 10 with audio | 1.80 to 1.93, 3 011 to 3 040 | 1.39, 1 498 |
| 10 silent     | 0.84 to 0.96, 1 068 to 1 079 | 0.56, 413 |

And the table of the end of Phase 5, run again (calls at 50 a second, held 10 s):

| Calls | Cores busy | Setup p50 / p99 (ms) | Audio heard (median) | Allocated |
|------:|-----------:|---------------------:|---------------------:|----------:|
| 40    | 4.6 to 4.7 | 16 to 22 / 206 to 237 | 0.99                | 282 MB/s  |
| 60    | 5.7        | 81 / 1 067           | 0.95 to 0.96         | 379 MB/s  |
| 80    | 5.5        | 187 / 2 670          | 0.92 to 0.94         | 421 MB/s  |
| 100   | 6.2        | 1 599 / 2 725        | 0.95                 | 502 MB/s  |

Every call was established in every run. At 40 calls the setup p50 went from 82 to
16 to 22 ms and the allocation rate from 530 to 282 MB/s; at 100, where setup took
9 s and a quarter of the audio was lost, calls now set up in under 3 s and 95% of
the audio arrives. The machine still saturates, between 60 and 80 calls with both
ends in the process, against 40 before. The target of 500 is still several times
away.

What is left: a silent call still costs about 0.55 cores per 10 calls and 19 KB per
tick, so the idle path has more in it (the tick's `evalMap`s, `fill`, `fixedRate`,
the sender and receiver loops, the reports), and each packet still costs about
50 KB over an idle call's. The `silent` option is how to tell which of the two a
change helps.

Two things about measuring, learned the hard way. First, a test run in sbt that
times out can leave work spinning in the sbt server's JVM: after a mutation check
that broke the playout's end, the server sat at 650% CPU for half an hour and
every number after it was wrong, so check `ps` and restart sbt before measuring.
Second, `uptime` first: another program at 97% CPU, which was there for hours, was
enough to move a result by several times.

## The playout as a loop of effects (2 November 2026)

The silent calls still cost 0.48 to 0.52 cores and 408 to 417 MB for 10 calls, and
the fix above had left the tick as a stream. To see how much of that was the tick,
a throwaway change made it ten times slower (200 ms): the allocation fell to 76 to
87 MB and the CPU to 0.31 to 0.38 cores. So the stream of ticks (`fixedRate`, two
`evalMap`s, `concurrently`, `interruptWhen`, and the `evalMap` that takes the frames)
cost about 16 KB and 0.18 ms a tick, and the rest of an idle call about 0.3 cores.
Taking `interruptWhen` out by itself changed nothing (437 to 449 MB), so it was not
that stage but the stream machinery as a whole.

`JitterPlayout.run(config, period, packets)(onFrame)` replaces the stream: a loop of
effects that sleeps to the next tick, takes a frame from the buffer and gives it to
`onFrame`, raced against the fiber that puts the packets in. It returns when the
packets end; an error in either the packets or `onFrame` is its error; stopping it
stops both. Its schedule is `Stream.fixedRate`'s, including what it does when a
frame takes longer than a period (one tick at once for the periods missed, then back
on the grid): a test runs the same slow frames through both and requires the same
times, and it fails if the loop merely advances by one period.

Ten calls for 20 s, at a load average of 6 to 10:

| 10 calls | Before (merge removed) | Now |
|----------|------------------------|-----|
| silent   | 0.48 to 0.52 cores, 408 to 417 MB | 0.34 to 0.46 cores, 69 to 78 MB |
| audible  | 1.39 cores, 1 498 MB              | 1.42 to 1.46 cores, 1 151 to 1 159 MB |

An idle call's allocation is down by 83% and its CPU is at the floor the slow-tick
experiment showed. Larger loads, each started on a quiet machine (load average
below 6), calls at 50 a second, held 10 s:

| Calls | Cores | Setup p50 / p99 (ms) | Audio heard (median) | Allocated |
|------:|------:|---------------------:|---------------------:|----------:|
| 40    | 3.7   | 15 / 76              | 0.99                 | 219 MB/s  |
| 100   | 5.9   | 20 / 313             | 0.99                 | 481 MB/s  |
| 150   | 4.7   | 2 750 / 11 133       | 0.54 to 0.62         | 238 MB/s  |
| 200   | 4.8   | 6 694 / 13 429       | 0.23 to 0.36         | 214 MB/s  |

Every call was established. With both ends in the process the knee is now between
100 and 150 calls, where it was about 40 at the end of Phase 5 (82 / 462 ms at 40,
and 9 s of setup with a quarter of the audio lost at 100). The same runs started at
a load average of 10 or more were over thirty times worse in setup time (a 40-call
run: p50 708 ms, 0.89 heard), so only compare runs that began quiet.

What is left is the packet: from the 10-call rows, a packet-op costs about 0.5 ms and
25 KB over an idle call's. Its path is `reads`, `evalMapFilter` (open),
`flatMap` into `Stream.exec ++ Stream.emit`, `evalTap` (observe) and `evalMap` (push
into the buffer): five stream stages, and the same on the send side.

