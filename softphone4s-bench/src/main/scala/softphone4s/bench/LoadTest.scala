package softphone4s.bench

import cats.effect.{ExitCode, IO, IOApp, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.{Chunk, Stream}
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.config.SoftphoneConfig
import softphone4s.model.{Callee, Secret, User}
import softphone4s.{Call, Softphone, SoftphoneStreaming}

import java.lang.management.ManagementFactory
import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** Load and soak tests: two softphones in one process, calling each other over
  * real UDP on the loopback interface, with audio going both ways.
  *
  * {{{
  * sbt "softphone4sBench/runMain softphone4s.bench.LoadTest load 500 20"
  * sbt "softphone4sBench/runMain softphone4s.bench.LoadTest soak 50 30"
  * }}}
  *
  * `load <calls> <hold seconds> [calls per second]` starts the calls at a rate,
  * holds them all with audio, and hangs them up. `soak <concurrent> <minutes>
  * [seconds per call]` keeps that many calls going, each ended and replaced,
  * and says whether file descriptors, threads or retained memory drift. Both
  * exit with 1 if a call fails or a limit is broken.
  */
object LoadTest extends IOApp {

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  /** One frame of 20 ms of loud audio, as a microphone would give. */
  private val frame: Chunk[Byte] =
    Chunk.array(
      Array.tabulate[Byte](320)(i => if i % 2 == 0 then 0x40 else 0x1f)
    )

  private val FrameMillis = 20L

  // ---- The two phones ---------------------------------------------------------

  private final case class Pair(alice: Softphone[IO], bob: Softphone[IO])

  private def config(name: String, sip: Port, rtp: Port): SoftphoneConfig =
    SoftphoneConfig(
      user = User(name).fold(e => sys.error(e), identity),
      password = Secret("secret"),
      realm = host"127.0.0.1",
      localSipPort = sip,
      baseRtpPort = rtp,
      bindAddress = ip"127.0.0.1",
      hangupTimeout = 5.seconds,
      answerTimeout = 120.seconds,
      inviteTimeout = 64.seconds
    )

  private val pair: Resource[IO, Pair] =
    (
      SoftphoneStreaming.resource[IO](
        config("alice", port"19420", port"20000"),
        audio,
        NoOpLogger.impl[IO]
      ),
      SoftphoneStreaming.resource[IO](
        config("bob", port"19421", port"30000"),
        audio,
        NoOpLogger.impl[IO]
      )
    ).mapN(Pair(_, _))

  private val bobAddress: Callee =
    Callee("bob@127.0.0.1:19421").fold(e => sys.error(e), identity)

  // ---- What a call does -----------------------------------------------------------

  /** What came of one call, as the caller saw it. */
  private final case class Outcome(
      setupMillis: Option[Long],
      hangupMillis: Option[Long],
      heardByCaller: Long,
      heardByCallee: Long,
      failure: Option[String]
  )

  /** Whether the phones send audio. `Silent` holds the calls open and sends
    * nothing, which shows what a call costs apart from its media packets: the
    * playout ticks, the timers and the reports.
    */
  private enum Talking {
    case Speaking
    case Silent
  }

  /** Talks into `call` a frame every 20 ms (unless `talking` is `Silent`) and
    * counts the bytes that come out of it, until `until` completes. Gives the
    * count.
    */
  private def talk(
      call: Call[IO],
      until: IO[Unit],
      talking: Talking
  ): IO[Long] =
    Ref.of[IO, Long](0L).flatMap { heard =>
      val speak = talking match {
        case Talking.Silent   => IO.never[Unit]
        case Talking.Speaking =>
          Stream
            .fixedRate[IO](FrameMillis.millis)
            .flatMap(_ => Stream.chunk(frame))
            .through(call.sink)
            .compile
            .drain
      }
      val listen = call.source.chunks
        .evalMap(c => heard.update(_ + c.size))
        .compile
        .drain
      IO.race(until, (speak, listen).parTupled).void >> heard.get
    }

  private def millisSince(start: FiniteDuration): IO[Long] =
    IO.monotonic.map(now => (now - start).toMillis)

  /** The callee's side of every call that arrives, until the caller hangs up.
    */
  private def answerAll(
      bob: Softphone[IO],
      results: Ref[IO, Map[String, Long]],
      talking: Talking
  ): IO[Unit] =
    bob.incoming
      .parEvalMapUnordered(Int.MaxValue) { incoming =>
        incoming.accept
          .use { call =>
            call.awaitPickup.attempt >>
              talk(call, call.awaitHangup, talking).flatMap(n =>
                results.update(
                  _ + (incoming.caller.toString + System.nanoTime -> n)
                )
              )
          }
          .attempt
          .void
      }
      .compile
      .drain

  /** A call from `alice`, held for `hold`. */
  private def oneCall(
      alice: Softphone[IO],
      hold: FiniteDuration,
      talking: Talking
  ): IO[Outcome] =
    for {
      begun   <- IO.monotonic
      outcome <- alice.call(bobAddress).allocated.attempt.flatMap {
        case Left(e) =>
          IO.pure(Outcome(None, None, 0, 0, Some(String.valueOf(e))))
        case Right((call, release)) =>
          call.awaitPickup.attempt.flatMap {
            case Left(e) =>
              release.attempt >> IO.pure(
                Outcome(None, None, 0, 0, Some(String.valueOf(e)))
              )
            case Right(_) =>
              for {
                setup <- millisSince(begun)
                heard <- talk(call, IO.sleep(hold), talking)
                hung  <- IO.monotonic
                _     <- release.attempt
                hang  <- millisSince(hung)
              } yield Outcome(Some(setup), Some(hang), heard, 0, None)
          }
      }
    } yield outcome

  // ---- What is measured of the process ---------------------------------------------

  /** CPU time taken: by the whole process, and by its Java threads (ours and
    * the libraries'), the difference being the JVM's own threads, which compile
    * code and collect garbage.
    */
  private final case class Cpu(
      process: Long,
      javaThreads: Long,
      javaUser: Long
  )

  private object Process {
    def cpu: Cpu = Cpu(cpuNanos, javaThreadsNanos, javaUserNanos)

    /** The part of the Java threads' CPU time spent in our code and the JVM's,
      * not in the operating system: system calls, and waking and parking
      * threads.
      */
    private def javaUserNanos: Long = {
      val threads = ManagementFactory.getThreadMXBean
      threads.getAllThreadIds.iterator
        .map(threads.getThreadUserTime)
        .filter(_ > 0)
        .sum
    }

    private def javaThreadsNanos: Long = {
      val threads = ManagementFactory.getThreadMXBean
      threads.getAllThreadIds.iterator
        .map(threads.getThreadCpuTime)
        .filter(_ > 0)
        .sum
    }

    def cpuNanos: Long = ManagementFactory.getOperatingSystemMXBean match {
      case os: com.sun.management.OperatingSystemMXBean => os.getProcessCpuTime
      case _                                            => 0L
    }
    def openFiles: Long = ManagementFactory.getOperatingSystemMXBean match {
      case os: com.sun.management.UnixOperatingSystemMXBean =>
        os.getOpenFileDescriptorCount
      case _ => -1L
    }
    def threads: Int     = ManagementFactory.getThreadMXBean.getThreadCount
    def heapUsedMb: Long = {
      val r = Runtime.getRuntime
      (r.totalMemory - r.freeMemory) / (1024 * 1024)
    }

    /** Heap in use once what can be collected has been. */
    def retainedMb: IO[Long] = IO.delay { System.gc(); heapUsedMb }
    def allocatedBytes: Long = ManagementFactory.getThreadMXBean match {
      case t: com.sun.management.ThreadMXBean =>
        t.getThreadAllocatedBytes(t.getAllThreadIds).sum
      case _ => 0L
    }
    def gc: (Long, Long) = {
      val beans = scala.jdk.CollectionConverters
        .ListHasAsScala(ManagementFactory.getGarbageCollectorMXBeans)
        .asScala
      (beans.map(_.getCollectionCount).sum, beans.map(_.getCollectionTime).sum)
    }
  }

  private def percentile(sorted: Vector[Long], p: Double): Long =
    if sorted.isEmpty then 0L
    else sorted(math.max(0, math.ceil(p * sorted.size).toInt - 1))

  private def line(label: String, values: Vector[Long]): String = {
    val s = values.sorted
    f"$label%-22s p50 ${percentile(s, 0.5)}%6d  p95 ${percentile(s, 0.95)}%6d  p99 ${percentile(s, 0.99)}%6d  max ${s.lastOption.getOrElse(0L)}%6d"
  }

  // ---- load -----------------------------------------------------------------------------

  /** What a `load` must meet to pass, besides every call being established.
    *
    * @param setupP99Millis
    *   the most the slowest one in a hundred may take from INVITE to ACK.
    * @param heard
    *   the least fraction of the audio that was sent that reaches the other
    *   side, in the median call.
    */
  private final case class Limits(
      setupP99Millis: Option[Long],
      heard: Option[Double]
  )

  private def load(
      calls: Int,
      hold: FiniteDuration,
      perSecond: Int,
      limits: Limits,
      talking: Talking
  ): IO[ExitCode] =
    pair.use { p =>
      for {
        bobResults <- Ref.of[IO, Map[String, Long]](Map.empty)
        report <- answerAll(p.bob, bobResults, talking).background.use { _ =>
          for {
            _ <- IO.println(
              s"== load: $calls calls, held ${hold.toSeconds} s, started at $perSecond a second =="
            )
            // Warm the JIT with real calls, which are not counted.
            _ <- (1 to 30).toList.parTraverse_(_ =>
              oneCall(p.alice, 1.second, talking)
            )
            _ <- IO.sleep(1.second)
            _ <- bobResults.set(Map.empty)
            cpu0 = Process.cpu
            gc0  = Process.gc
            mem0 = Process.allocatedBytes
            start <- IO.monotonic
            peak  <- Ref.of[IO, (Long, Int)]((0L, 0))
            // sampled when the last call has been started and all are held
            sampling <- (IO.sleep(
              (calls.toDouble / perSecond).seconds + (hold / 2)
            ) >> IO
              .delay((Process.openFiles, Process.threads))
              .flatMap(peak.set)).start
            outcomes <- (0 until calls).toList.parTraverse(i =>
              IO.sleep((i * 1000L / perSecond).millis) >> oneCall(
                p.alice,
                hold,
                talking
              )
            )
            elapsed    <- millisSince(start)
            _          <- sampling.cancel
            _          <- IO.sleep(2.seconds)
            heardByBob <- bobResults.get.map(_.values.toVector)
            retained   <- Process.retainedMb
            files      <- IO.delay(Process.openFiles)
            peaked     <- peak.get
          } yield (
            outcomes,
            elapsed,
            heardByBob,
            retained,
            files,
            peaked,
            cpu0,
            gc0,
            mem0
          )
        }
      } yield {
        val (
          outcomes,
          elapsed,
          heardByBob,
          retained,
          files,
          peaked,
          cpu0,
          gc0,
          mem0
        )                            = report
        val (peakFiles, peakThreads) = peaked
        val done                     = outcomes.filter(_.failure.isEmpty)
        val failures                 = outcomes.flatMap(_.failure)
        val expected                 = hold.toMillis / FrameMillis * frame.size
        val cpuNow                   = Process.cpu
        val cores                    =
          (cpuNow.process - cpu0.process).toDouble / (elapsed * 1000000L)
        val javaCores =
          (cpuNow.javaThreads - cpu0.javaThreads).toDouble / (elapsed * 1000000L)
        val userCores =
          (cpuNow.javaUser - cpu0.javaUser).toDouble / (elapsed * 1000000L)
        val userCoresSystem = javaCores - userCores
        val (gcN, gcMs)     = Process.gc
        val allocated       = (Process.allocatedBytes - mem0) / (1024 * 1024)
        println(
          f"established ${done.size}/$calls (failed ${failures.size}) in ${elapsed / 1000.0}%.1f s"
        )
        failures
          .groupBy(identity)
          .view
          .mapValues(_.size)
          .toList
          .sortBy(-_._2)
          .take(5)
          .foreach((m, n) => println(s"  $n x $m"))
        println(
          line("setup ms (INVITE->ACK)", done.flatMap(_.setupMillis).toVector)
        )
        println(
          line("hang-up ms (BYE->200)", done.flatMap(_.hangupMillis).toVector)
        )
        def sortedFractions(heard: Vector[Long]): Vector[Double] =
          heard.map(_.toDouble / expected).sorted
        def median(sorted: Vector[Double]): Double =
          sorted.lift(sorted.size / 2).getOrElse(0.0)
        def fractions(sorted: Vector[Double]): String =
          f"min ${sorted.headOption.getOrElse(0.0)}%.2f median ${median(sorted)}%.2f"
        val atCaller = sortedFractions(done.map(_.heardByCaller).toVector)
        val atCallee = sortedFractions(heardByBob)
        println(
          s"audio heard, per call, as a fraction of what was sent: caller ${fractions(atCaller)}; callee ${fractions(atCallee)} (${heardByBob.size} calls)"
        )
        println(
          f"process: $cores%.2f cores busy on average (Java threads $javaCores%.2f, of which in the system $userCoresSystem%.2f; JIT and GC ${cores - javaCores}%.2f); ${gcN - gc0._1} collections, ${gcMs - gc0._2} ms; allocated $allocated MB (${allocated * 1000.0 / elapsed}%.0f MB/s)"
        )
        println(
          s"held: $peakFiles open files, $peakThreads threads at the peak; after: $files open files, retained heap $retained MB"
        )
        val setupP99 =
          percentile(done.flatMap(_.setupMillis).toVector.sorted, 0.99)
        val broken = List(
          Option.when(failures.nonEmpty || done.size < calls)(
            "not every call was established"
          ),
          limits.setupP99Millis
            .filter(setupP99 > _)
            .map(m => s"setup p99 is $setupP99 ms, over the limit of $m ms"),
          limits.heard
            .filter(h => math.min(median(atCaller), median(atCallee)) < h)
            .map(h => f"less than $h%.2f of the audio is heard")
        ).flatten
        broken.foreach(m => println(s"FAILED: $m"))
        if broken.nonEmpty then ExitCode.Error else ExitCode.Success
      }
    }

  // ---- soak -------------------------------------------------------------------------------

  private final case class Sample(
      second: Long,
      files: Long,
      threads: Int,
      retainedMb: Long,
      calls: Long,
      failed: Long
  )

  private def soak(
      concurrent: Int,
      minutes: Int,
      callSeconds: Int
  ): IO[ExitCode] =
    pair.use { p =>
      for {
        bobResults <- Ref.of[IO, Map[String, Long]](Map.empty)
        completed  <- Ref.of[IO, Long](0L)
        failed     <- Ref.of[IO, Long](0L)
        samples    <- Ref.of[IO, Vector[Sample]](Vector.empty)
        _          <- IO.println(
          s"== soak: $concurrent calls at a time, each ${callSeconds} s, for $minutes minutes =="
        )
        start <- IO.monotonic
        deadline = start + minutes.minutes
        // A slot starts no call once the time is up, and lets the one it is in end.
        slot = Stream
          .repeatEval(IO.monotonic)
          .takeWhile(_ < deadline)
          .evalMap(_ =>
            oneCall(p.alice, callSeconds.seconds, Talking.Speaking).flatMap(o =>
              if o.failure.isEmpty then completed.update(_ + 1)
              else
                failed.update(_ + 1) >>
                  IO.println(s"  failure: ${o.failure.getOrElse("")}")
            )
          )
          .compile
          .drain
        sampling = Stream
          .awakeEvery[IO](30.seconds)
          .evalMap { _ =>
            for {
              second   <- millisSince(start).map(_ / 1000)
              retained <- Process.retainedMb
              c        <- completed.get
              f        <- failed.get
              sample = Sample(
                second,
                Process.openFiles,
                Process.threads,
                retained,
                c,
                f
              )
              _ <- samples.update(_ :+ sample)
              _ <- IO.println(
                f"  ${sample.second}%5d s: ${sample.calls}%6d calls (${sample.failed} failed), ${sample.files}%4d open files, ${sample.threads}%3d threads, retained heap ${sample.retainedMb}%4d MB"
              )
            } yield ()
          }
          .compile
          .drain
        _ <- answerAll(p.bob, bobResults, Talking.Speaking).background.use {
          _ =>
            sampling.background.use(_ =>
              (1 to concurrent).toList.parTraverse_(_ => slot)
            )
        }
        all    <- samples.get
        total  <- completed.get
        errors <- failed.get
      } yield {
        println(s"completed $total calls, $errors failed")
        // the first sample is of a warmed-up phone; drift is what grew after it
        val steady   = all.drop(1)
        val verdicts = steady.headOption.zip(steady.lastOption).toList.flatMap {
          (first, last) =>
            List(
              (
                "open files",
                last.files - first.files,
                last.files - first.files <= 20L
              ),
              (
                "threads",
                (last.threads - first.threads).toLong,
                last.threads - first.threads <= 20
              ),
              (
                "retained heap MB",
                last.retainedMb - first.retainedMb,
                last.retainedMb - first.retainedMb <= math
                  .max(64L, first.retainedMb)
              )
            )
        }
        verdicts.foreach((what, drift, ok) =>
          println(f"drift of $what%-18s ${drift}%+6d  ${
              if ok then "ok" else "TOO MUCH"
            }")
        )
        val bad = errors > 0 || verdicts.exists(!_._3)
        if bad then println("FAILED")
        if bad then ExitCode.Error else ExitCode.Success
      }
    }

  def run(args: List[String]): IO[ExitCode] =
    args match {
      case "load" :: calls :: hold :: more =>
        // `silent` anywhere after the hold time: the calls send no audio.
        val talking =
          if more.contains("silent") then Talking.Silent else Talking.Speaking
        val rest = more.filterNot(_ == "silent")
        (
          calls.toIntOption,
          hold.toIntOption,
          rest.headOption.fold(Some(100))(_.toIntOption)
        ).tupled match {
          case Some((n, h, rate)) =>
            load(
              n,
              h.seconds,
              rate,
              Limits(
                rest.lift(1).flatMap(_.toLongOption),
                rest.lift(2).flatMap(_.toDoubleOption)
              ),
              talking
            )
          case None => usage
        }
      case "soak" :: concurrent :: minutes :: rest =>
        (
          concurrent.toIntOption,
          minutes.toIntOption,
          rest.headOption.fold(Some(10))(_.toIntOption)
        ).tupled match {
          case Some((c, m, s)) => soak(c, m, s)
          case None            => usage
        }
      case _ => usage
    }

  private val usage: IO[ExitCode] =
    IO.println(
      "usage: load <calls> <hold seconds> [calls per second [setup p99 limit ms [least fraction heard]]] [silent]\n       soak <concurrent calls> <minutes> [seconds per call]"
    ).as(ExitCode.Error)
}
