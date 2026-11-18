package softphone4s.ua

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{Datagram, DatagramSocket, Network}
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.SipLiterals.pt
import softphone4s.codec.{RtcpCodec, RtpCodec}
import softphone4s.config.RtpLatching
import softphone4s.media.RtcpPacket.ReportBlock
import softphone4s.media.{
  AudioCodecs,
  JitterConfig,
  LossFraction,
  RtcpPacket,
  RtcpSchedule
}
import softphone4s.model.{MediaDirection, Ssrc}
import softphone4s.sdp.{Codec, MediaPlan}
import softphone4s.srtp.*
import softphone4s.transport.RawPeer

import scala.concurrent.duration.*

/** The reports of an encrypted call, over real UDP: protected with the RTCP
  * keys of the call, which are not the keys of its media.
  */
class MediaSrtcpSuite extends CatsEffectSuite {

  private val loopback = ip"127.0.0.1"
  private val Suite    = SrtpSuite.AesCm128HmacSha1_80
  private val theirs   = Ssrc.fromBits(7)

  private def key(n: Int): MasterKey =
    MasterKey(
      Chunk.array(Array.fill[Byte](16)(n.toByte)),
      Chunk.array(Array.fill[Byte](14)((n + 1).toByte))
    ).fold(e => fail(e.message), identity)

  /** We send with key 1 and receive with key 2. */
  private val keys = SrtpPlan(Suite, key(1), key(2))

  private val interval = RtcpSchedule(100.millis).fold(e => fail(e), identity)

  /** A port whose successor is free as well, found by trying. */
  private def freePair: IO[Port] =
    Network[IO]
      .bindDatagramSocket(address = SocketAddress(loopback, port"0"))
      .use(socket => RawPeer.addressOf(socket.address))
      .flatMap { address =>
        Port.fromInt(address.port.value + 1).fold(freePair) { next =>
          Network[IO]
            .bindDatagramSocket(address = SocketAddress(loopback, next))
            .use_
            .as(address.port)
            .handleErrorWith(_ => freePair)
        }
      }

  private def socketAt(port: Port): Resource[IO, DatagramSocket[IO]] =
    Network[IO].bindDatagramSocket(address = SocketAddress(loopback, port))

  private def next(port: Port): Port =
    Port.fromInt(port.value + 1).getOrElse(fail("no port after"))

  /** The media, its RTP port, and the peer's RTP and RTCP sockets. */
  private def call =
    for {
      peerPort <- Resource.eval(freePair)
      ours     <- Resource.eval(freePair)
      peerRtp  <- socketAt(peerPort)
      peerRtcp <- socketAt(next(peerPort))
      sv       <- Supervisor[IO](await = false)
      media    <- Resource.make(
        MediaSession.create[IO](
          loopback,
          ours,
          AudioCodecs.Default,
          RtpLatching.Ignore,
          JitterConfig.Default,
          interval
        )
      )(_.stop())
      _ <- Resource.eval(
        media.start(
          MediaPlan(
            SocketAddress(loopback, peerPort),
            Codec.Pcmu,
            pt(0),
            None,
            MediaDirection.SendRecv,
            MediaDirection.SendRecv,
            None,
            Some(keys)
          ),
          sv,
          _ => IO.unit
        ) >> IO.sleep(500.millis) // the sockets are bound in the background
      )
    } yield (media, ours, peerRtp, peerRtcp)

  /** The RTCP packets that arrive, opened with the key we send with. */
  private def opened(rtcp: DatagramSocket[IO]): Stream[IO, RtcpPacket] =
    Stream.eval(Ref.of[IO, SrtcpInbound](SrtcpInbound(keys.send))).flatMap {
      state =>
        rtcp.reads
          .evalMapFilter { datagram =>
            state.modify { in =>
              in.unprotect(datagram.bytes) match {
                case Right((nextState, plain)) => (nextState, Some(plain))
                case Left(_)                   => (in, None)
              }
            }
          }
          .flatMap(plain =>
            Stream.emits(RtcpCodec.decode(plain).getOrElse(Nil))
          )
    }

  private def aboutUs(ssrc: Ssrc): ReportBlock =
    ReportBlock(ssrc, LossFraction.fromBits(25), 3, 1234L, 9L, 0L, 0L)

  private def audio(sequence: Int): Chunk[Byte] =
    RtpCodec.encode(
      pt(0),
      sequence,
      sequence * 160L,
      theirs,
      Chunk.array(Array.fill[Byte](160)(0x80.toByte))
    )

  test("a report goes out encrypted, and opens with the key we send with") {
    call.use { (_, _, _, peerRtcp) =>
      for {
        datagram <- peerRtcp.read.timeout(5.seconds)
        report   <- opened(peerRtcp)
          .collectFirst { case p: RtcpPacket.SourceDescription => p }
          .compile
          .lastOrError
          .timeout(5.seconds)
      } yield {
        // The name of the sender is not readable on the wire.
        assert(
          !new String(datagram.bytes.toArray, "ISO-8859-1")
            .contains("softphone4s@"),
          "the report is in the clear"
        )
        assert(report.names.head.name.startsWith("softphone4s@"))
        // Read as plain RTCP what is on the wire does not give the report.
        assertNotEquals(RtcpCodec.decode(datagram.bytes), Right(List(report)))
      }
    }
  }

  test("what we heard is in an encrypted report") {
    call.use { (_, ours, peerRtp, peerRtcp) =>
      for {
        out <- Ref.of[IO, SrtpOutbound](SrtpOutbound(Suite, keys.receive))
        _   <- (1 to 5).toList.traverse_(n =>
          out
            .modify(o =>
              o.protect(audio(n)).fold(e => fail(e.message), (a, b) => (a, b))
            )
            .flatMap(bytes =>
              peerRtp.write(Datagram(SocketAddress(loopback, ours), bytes))
            )
        )
        block <- opened(peerRtcp)
          .collect {
            case RtcpPacket.ReceiverReport(_, blocks)  => blocks
            case RtcpPacket.SenderReport(_, _, blocks) => blocks
          }
          .flatMap(Stream.emits)
          .find(b => b.ssrc == theirs && b.highestSequence == 5)
          .compile
          .lastOrError
          .timeout(5.seconds)
      } yield assertEquals(block.cumulativeLost, 0)
    }
  }

  test("an encrypted report from the other side is kept") {
    call.use { (media, ours, peerRtp, peerRtcp) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(Array.fill[Byte](320 * 2)(1)))
          .through(media.sink)
          .compile
          .drain
        first <- peerRtp.read.timeout(5.seconds)
        header = RtpCodec
          .decode(
            SrtpInbound(Suite, keys.send)
              .unprotect(first.bytes)
              .fold(e => fail(e.message), (_, p) => p)
          )
          .getOrElse(fail("not RTP"))
          ._1
        mine   = aboutUs(header.ssrc)
        report = RtcpCodec.encode(
          List(RtcpPacket.ReceiverReport(theirs, List(mine)))
        )
        wire = SrtcpOutbound(keys.receive)
          .protect(report)
          .fold(e => fail(e.message), (_, bytes) => bytes)
        _ <- peerRtcp.write(Datagram(SocketAddress(loopback, next(ours)), wire))
        kept <- (IO.sleep(100.millis) >> media.peerReport)
          .iterateUntil(_.isDefined)
          .timeout(5.seconds)
      } yield assertEquals(kept, Some(mine))
    }
  }

  test(
    "a report in the clear from the other side is ignored on an encrypted call"
  ) {
    call.use { (media, ours, peerRtp, peerRtcp) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(Array.fill[Byte](320)(1)))
          .through(media.sink)
          .compile
          .drain
        first <- peerRtp.read.timeout(5.seconds)
        header = RtpCodec
          .decode(
            SrtpInbound(Suite, keys.send)
              .unprotect(first.bytes)
              .fold(e => fail(e.message), (_, p) => p)
          )
          .getOrElse(fail("not RTP"))
          ._1
        plain = RtcpCodec.encode(
          List(RtcpPacket.ReceiverReport(theirs, List(aboutUs(header.ssrc))))
        )
        _ <- peerRtcp.write(
          Datagram(SocketAddress(loopback, next(ours)), plain)
        )
        _    <- IO.sleep(400.millis)
        kept <- media.peerReport
      } yield assertEquals(kept, None)
    }
  }

  test("a report sealed with another key is ignored") {
    call.use { (media, ours, peerRtp, peerRtcp) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(Array.fill[Byte](320)(1)))
          .through(media.sink)
          .compile
          .drain
        first <- peerRtp.read.timeout(5.seconds)
        header = RtpCodec
          .decode(
            SrtpInbound(Suite, keys.send)
              .unprotect(first.bytes)
              .fold(e => fail(e.message), (_, p) => p)
          )
          .getOrElse(fail("not RTP"))
          ._1
        report = RtcpCodec.encode(
          List(RtcpPacket.ReceiverReport(theirs, List(aboutUs(header.ssrc))))
        )
        wire = SrtcpOutbound(key(9))
          .protect(report)
          .fold(e => fail(e.message), (_, bytes) => bytes)
        _ <- peerRtcp.write(Datagram(SocketAddress(loopback, next(ours)), wire))
        _ <- IO.sleep(400.millis)
        kept <- media.peerReport
      } yield assertEquals(kept, None)
    }
  }

  // Failed once in about 5,550 runs; the cause was not found. If it fails again,
  // read the packets the peer recorded before changing anything.
  test("the goodbye when the call ends is encrypted too") {
    call.use { (media, ours, peerRtp, peerRtcp) =>
      for {
        out <- Ref.of[IO, SrtpOutbound](SrtpOutbound(Suite, keys.receive))
        _   <- (1 to 3).toList.traverse_(n =>
          out
            .modify(o =>
              o.protect(audio(n)).fold(e => fail(e.message), (a, b) => (a, b))
            )
            .flatMap(bytes =>
              peerRtp.write(Datagram(SocketAddress(loopback, ours), bytes))
            )
        )
        _   <- IO.sleep(300.millis)
        _   <- media.stop()
        bye <- opened(peerRtcp)
          .find { case RtcpPacket.Goodbye(_, _) => true; case _ => false }
          .compile
          .lastOrError
          .timeout(5.seconds)
      } yield assert(bye.isInstanceOf[RtcpPacket.Goodbye])
    }
  }
}
