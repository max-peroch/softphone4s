package softphone4s.bench

import cats.effect.unsafe.implicits.global
import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{Datagram, DatagramSocket}
import fs2.{Chunk, Pipe, Stream}
import org.openjdk.jmh.annotations.*
import softphone4s.codec.RtpCodec
import softphone4s.media.{
  G711,
  JitterBuffer,
  JitterConfig,
  JitterPacket,
  Playout,
  RtpReception
}
import softphone4s.model.{PayloadType, Ssrc}
import softphone4s.transport.{RtpSender, RtpTransport}

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

/** What each stage of the packet path costs, from memory: no socket, so none of
  * the operating system's share. Each benchmark is cumulative (the send path:
  * the frames, then the encoder, then `RtpSender.audio`, then `interruptWhen`),
  * so a stage is the difference between two. `-prof gc` gives the allocation.
  * Each is per frame of 20 ms.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@OperationsPerInvocation(1000)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
class PacketPathBenchmark {

  private val frames = 1000

  private val frame: Chunk[Byte] =
    Chunk.array(
      Array.tabulate[Byte](320)(i => if i % 2 == 0 then 0x40 else 0x1f)
    )

  /** A socket that takes what is written and counts it. */
  private final class CountingSocket(incoming: Stream[IO, Datagram])
      extends DatagramSocket[IO] {
    val written = new AtomicLong

    private def unsupported[A]: IO[A] =
      IO.raiseError(new UnsupportedOperationException("a stub socket"))

    def address: GenSocketAddress = SocketAddress(ip"127.0.0.1", port"1")
    def localAddress: IO[SocketAddress[IpAddress]] =
      IO.pure(SocketAddress(ip"127.0.0.1", port"1"))
    def supportedOptions                                      = unsupported
    def getOption[A](key: java.net.SocketOption[A])           = unsupported
    def setOption[A](key: java.net.SocketOption[A], value: A) = unsupported
    def readGen                                               = unsupported
    def connect(address: GenSocketAddress)                    = unsupported
    def disconnect                                            = unsupported
    def read                                                  = unsupported
    def reads: Stream[IO, Datagram]                           = incoming
    def write(bytes: Chunk[Byte]): IO[Unit]                   = IO.delay {
      written.incrementAndGet(); ()
    }
    def write(bytes: Chunk[Byte], address: GenSocketAddress): IO[Unit] = write(
      bytes
    )
    override def write(datagram: Datagram): IO[Unit] = write(datagram.bytes)
    def writes: Pipe[IO, Datagram, Nothing]          = _.evalMap(write).drain
    def join(
        join: MulticastJoin[IpAddress],
        interface: NetworkInterface
    ) = unsupported
    def join(
        join: MulticastJoin[IpAddress],
        interface: java.net.NetworkInterface
    ) = unsupported
  }

  private var rtp: RtpSender[IO] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =
    rtp = RtpSender
      .make[IO](
        new CountingSocket(Stream.empty),
        IO.pure(Some(SocketAddress(ip"127.0.0.1", port"9"))),
        PayloadType(0).fold(e => sys.error(e), identity),
        None,
        160
      )
      .unsafeRunSync()

  private def source: Stream[IO, Byte] =
    Stream.range(0, frames).flatMap(_ => Stream.chunk(frame))

  private def encoder: Pipe[IO, Byte, Chunk[Byte]] = G711.Pcmu.encoder[IO]

  /** The frames as a program makes them: a stream of bytes, drained. */
  @Benchmark
  def send0_frames(): Unit = source.compile.drain.unsafeRunSync()

  /** ...through the encoder. */
  @Benchmark
  def send1_encoder(): Unit =
    source.through(encoder).compile.drain.unsafeRunSync()

  /** ...and `RtpSender.audio`, to a socket that is not there. */
  @Benchmark
  def send2_rtp(): Unit =
    source.through(encoder).through(rtp.audio).compile.drain.unsafeRunSync()

  /** ...and `interruptWhen`, as `Call.sink` has it. */
  @Benchmark
  def send3_interruptWhen(): Unit =
    Deferred[IO, Unit]
      .flatMap(stop =>
        source
          .through(encoder)
          .through(rtp.audio)
          .interruptWhen(stop.get.attempt)
          .compile
          .drain
      )
      .unsafeRunSync()

  // ---- The receive path ---------------------------------------------------------

  private val pcmu: PayloadType =
    PayloadType(0).fold(e => sys.error(e), identity)
  private val peer = Ssrc.fromBits(7)
  private val from = SocketAddress(ip"127.0.0.1", port"9")

  private def rtpPacket(i: Int): Chunk[Byte] =
    RtpCodec.encode(
      pcmu,
      i & 0xffff,
      i * 160L,
      peer,
      Chunk.array(Array.fill[Byte](160)(0x55))
    )

  private val datagrams: Vector[Datagram] =
    Vector.tabulate(frames)(i => Datagram(from, rtpPacket(i)))

  private def reads: Stream[IO, Datagram] = Stream.emits(datagrams).covary[IO]

  private def received(
      socket: DatagramSocket[IO]
  ): IO[Stream[IO, JitterPacket]] =
    Ref
      .of[IO, (Long, Int)]((0L, 0))
      .map(last =>
        RtpTransport.receiveBytes[IO](
          socket,
          last,
          _ => IO.unit,
          (_, _) => IO.unit,
          pcmu,
          None,
          None,
          bytes => IO.pure(Some(bytes))
        )
      )

  /** What the datagrams are, as a stream from memory, drained. */
  @Benchmark
  def recv0_reads(): Unit = reads.compile.drain.unsafeRunSync()

  /** ...through `RtpTransport.receiveBytes` (open, decode, who it is from). */
  @Benchmark
  def recv1_receiveBytes(): Unit =
    received(new CountingSocket(reads))
      .flatMap(_.compile.drain)
      .unsafeRunSync()

  /** ...counted as `MediaSession.observe` counts, and put in the jitter buffer.
    */
  @Benchmark
  def recv2_countAndPush(): Unit =
    (
      received(new CountingSocket(reads)),
      Ref.of[IO, Map[Ssrc, RtpReception]](Map.empty),
      Ref.of[IO, JitterBuffer](JitterBuffer.empty(JitterConfig.Default))
    ).flatMapN { (incoming, receptions, buffer) =>
      incoming
        .evalTap(packet =>
          IO.monotonic.flatMap(now =>
            receptions.update(known =>
              known.get(packet.ssrc) match {
                case Some(seen) =>
                  known.updated(
                    packet.ssrc,
                    seen.hear(packet.sequence, packet.timestamp, now)
                  )
                case None =>
                  Map(
                    packet.ssrc -> RtpReception.start(
                      packet.ssrc,
                      8000,
                      packet.sequence,
                      packet.timestamp,
                      now
                    )
                  )
              }
            )
          )
        )
        .evalMap(packet => buffer.update(_.push(packet)._1))
        .compile
        .drain
    }.unsafeRunSync()

  // ---- The tick -----------------------------------------------------------------

  private def jitterPacket(i: Int): JitterPacket =
    JitterPacket(
      i & 0xffff,
      i * 160L,
      peer,
      marker = false,
      pcmu,
      Chunk.array(Array.fill[Byte](160)(0x55))
    )

  private val packets: Vector[JitterPacket] =
    Vector.tabulate(frames)(jitterPacket)

  /** A packet in and a frame out, as at steady state: the buffer's own work. */
  @Benchmark
  def tick1_pushAndPull(): Unit =
    Ref
      .of[IO, JitterBuffer](JitterBuffer.empty(JitterConfig.Default))
      .flatMap(buffer =>
        Stream
          .emits(packets)
          .covary[IO]
          .evalMap(packet =>
            buffer.update(_.push(packet)._1) >>
              buffer.modify { current =>
                val (next, out) = current.pull
                (next, (out, next.stats))
              }
          )
          .compile
          .drain
      )
      .unsafeRunSync()

  /** ...and the frame decoded and handed on to what reads the audio. */
  @Benchmark
  def tick2_decodeAndSend(): Unit =
    G711.Pcmu
      .decoderSession[IO]
      .use(decode =>
        (
          Ref.of[IO, JitterBuffer](JitterBuffer.empty(JitterConfig.Default)),
          Queue.unbounded[IO, Option[Chunk[Byte]]]
        ).flatMapN { (buffer, out) =>
          val consumer = Stream
            .fromQueueNoneTerminated(out)
            .flatMap(Stream.chunk)
            .compile
            .drain
          consumer.background.use { done =>
            Stream
              .emits(packets)
              .covary[IO]
              .evalMap(packet =>
                buffer.update(_.push(packet)._1) >>
                  buffer.modify(_.pull).flatMap {
                    case Playout.Frame(p) =>
                      out.offer(Some(Chunk.array(decode(p.payload.toArray))))
                    case _ => IO.unit
                  }
              )
              .compile
              .drain >> out.offer(None) >> done.void
          }
        }
      )
      .unsafeRunSync()
}
