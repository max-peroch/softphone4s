package softphone4s.ua

import cats.effect.std.Supervisor
import cats.effect.{IO, Resource}
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
import softphone4s.transport.RawPeer

import scala.concurrent.duration.*

/** Reports over real UDP: our RTCP socket is the port after our RTP one, and
  * the peer's is the port after theirs.
  */
class MediaRtcpSuite extends CatsEffectSuite {

  private val loopback = ip"127.0.0.1"

  private val theirs = Ssrc.fromBits(7)

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

  private val interval =
    RtcpSchedule(100.millis).fold(e => fail(e), identity)

  /** The media under test, its RTP port, and the peer's RTP and RTCP sockets.
    */
  private def call(rtcpBlockedAt: Option[Port] = None) =
    for {
      peerPort <- Resource.eval(freePair)
      ours     <- Resource.eval(freePair)
      _ <- rtcpBlockedAt.fold(Resource.unit[IO])(_ => socketAt(next(ours)).void)
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
            MediaDirection.SendRecv
          ),
          sv,
          _ => IO.unit
        ) >> IO.sleep(500.millis) // the sockets are bound in the background
      )
    } yield (media, ours, peerRtp, peerRtcp)

  private def audio(sequence: Int): Datagram =
    Datagram(
      SocketAddress(loopback, port"1"), // replaced by the caller
      RtpCodec.encode(
        pt(0),
        sequence,
        sequence * 160L,
        theirs,
        Chunk.array(Array.fill[Byte](160)(0x80.toByte))
      )
    )

  private def sendAudio(
      peer: DatagramSocket[IO],
      ours: Port,
      sequences: Int*
  ): IO[Unit] =
    sequences.toList.traverse_(n =>
      peer.write(audio(n).copy(remote = SocketAddress(loopback, ours)))
    )

  private def reportsFrom(rtcp: DatagramSocket[IO]): Stream[IO, RtcpPacket] =
    rtcp.reads.flatMap(d =>
      Stream.emits(RtcpCodec.decode(d.bytes).getOrElse(Nil))
    )

  private def blockAbout(
      packet: RtcpPacket,
      who: Ssrc
  ): Option[ReportBlock] =
    packet match {
      case RtcpPacket.ReceiverReport(_, blocks)  => blocks.find(_.ssrc == who)
      case RtcpPacket.SenderReport(_, _, blocks) => blocks.find(_.ssrc == who)
      case _                                     => None
    }

  test("a report says what arrived: the highest sequence and nothing lost") {
    call().use { (_, ours, peerRtp, peerRtcp) =>
      for {
        _     <- sendAudio(peerRtp, ours, 1, 2, 3, 4, 5)
        block <- reportsFrom(peerRtcp)
          .mapFilter(blockAbout(_, theirs))
          .find(_.highestSequence == 5)
          .compile
          .lastOrError
          .timeout(5.seconds)
      } yield assertEquals(
        (block.cumulativeLost, block.fractionLost.value),
        (0, 0)
      )
    }
  }

  test("a packet that never came is lost in the report") {
    call().use { (_, ours, peerRtp, peerRtcp) =>
      for {
        _     <- sendAudio(peerRtp, ours, 1, 2, 4, 5)
        block <- reportsFrom(peerRtcp)
          .mapFilter(blockAbout(_, theirs))
          .find(_.highestSequence == 5)
          .compile
          .lastOrError
          .timeout(5.seconds)
      } yield assertEquals(block.cumulativeLost, 1)
    }
  }

  test("a report tells who we are") {
    call().use { (_, _, _, peerRtcp) =>
      reportsFrom(peerRtcp)
        .collectFirst { case RtcpPacket.SourceDescription(names) => names.head }
        .compile
        .lastOrError
        .timeout(5.seconds)
        .map(name => assert(name.name.startsWith("softphone4s@"), name.name))
    }
  }

  test("once we have sent audio the report is a sender's, with the counts") {
    call().use { (media, _, peerRtp, peerRtcp) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(Array.fill[Byte](320 * 3)(1)))
          .through(media.sink)
          .compile
          .drain
        sent <- reportsFrom(peerRtcp)
          // Every sender report, not only the first, which may come before the
          // three frames have all gone out.
          .collect { case RtcpPacket.SenderReport(_, info, _) => info }
          .find(_.packets >= 3)
          .compile
          .lastOrError
          .timeout(5.seconds)
        first <- peerRtp.reads.head.compile.lastOrError
      } yield {
        assertEquals(sent.octets, sent.packets * 160)
        assert(RtpCodec.decode(first.bytes).isDefined)
      }
    }
  }

  test("the report the other side makes of our stream is kept") {
    call().use { (media, ours, peerRtp, peerRtcp) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(Array.fill[Byte](320 * 2)(1)))
          .through(media.sink)
          .compile
          .drain
        header <- peerRtp.reads.head.compile.lastOrError.map(d =>
          RtpCodec.decode(d.bytes).map(_._1).getOrElse(fail("not RTP"))
        )
        none <- media.peerReport
        theirReport = ReportBlock(
          header.ssrc,
          LossFraction.fromBits(25),
          3,
          1234L,
          9L,
          0L,
          0L
        )
        _ <- peerRtcp.write(
          Datagram(
            SocketAddress(loopback, next(ours)),
            RtcpCodec.encode(
              List(RtcpPacket.ReceiverReport(theirs, List(theirReport)))
            )
          )
        )
        kept <- (IO.sleep(100.millis) >> media.peerReport)
          .iterateUntil(_.isDefined)
          .timeout(5.seconds)
      } yield {
        assertEquals(none, None)
        assertEquals(kept, Some(theirReport))
      }
    }
  }

  test(
    "a report from somewhere else than the party we send to is not believed"
  ) {
    call().use { (media, ours, peerRtp, _) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(Array.fill[Byte](320)(1)))
          .through(media.sink)
          .compile
          .drain
        header <- peerRtp.reads.head.compile.lastOrError.map(d =>
          RtpCodec.decode(d.bytes).map(_._1).getOrElse(fail("not RTP"))
        )
        // From another address of the same machine.
        sent <- Network[IO]
          .bindDatagramSocket(
            address = SocketAddress(ip"127.0.0.2", port"0")
          )
          .use(socket =>
            socket
              .write(
                Datagram(
                  SocketAddress(loopback, next(ours)),
                  RtcpCodec.encode(
                    List(
                      RtcpPacket.ReceiverReport(
                        theirs,
                        List(
                          ReportBlock(
                            header.ssrc,
                            LossFraction.None,
                            0,
                            1L,
                            0L,
                            0L,
                            0L
                          )
                        )
                      )
                    )
                  )
                )
              )
          )
          .attempt
        _    <- IO.sleep(300.millis)
        kept <- media.peerReport
      } yield {
        assume(sent.isRight, "no second loopback address on this machine")
        assertEquals(kept, None)
      }
    }
  }

  test("a goodbye goes out when the media stops") {
    call().use { (media, ours, peerRtp, peerRtcp) =>
      for {
        _   <- sendAudio(peerRtp, ours, 1, 2, 3)
        _   <- IO.sleep(300.millis)
        _   <- media.stop()
        bye <- reportsFrom(peerRtcp)
          .find { case RtcpPacket.Goodbye(_, _) => true; case _ => false }
          .compile
          .lastOrError
          .timeout(5.seconds)
      } yield assert(bye.isInstanceOf[RtcpPacket.Goodbye])
    }
  }

  test("a call whose RTCP port is taken goes on without reports") {
    freePair.flatMap { held =>
      // Taking the port after our RTP one before the media starts.
      call(rtcpBlockedAt = Some(held)).use { (media, ours, peerRtp, _) =>
        for {
          _ <- List(1, 2, 3).traverse_(n =>
            peerRtp.write(audio(n).copy(remote = SocketAddress(loopback, ours)))
          )
          heard <- media.source.take(320).compile.to(Chunk).timeout(5.seconds)
        } yield assertEquals(heard.size, 320)
      }
    }
  }

  test(
    "while the call is on hold there is no one to believe, and reports are ignored"
  ) {
    call().use { (media, ours, peerRtp, peerRtcp) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(Array.fill[Byte](320)(1)))
          .through(media.sink)
          .compile
          .drain
        header <- peerRtp.reads.head.compile.lastOrError.map(d =>
          RtpCodec.decode(d.bytes).map(_._1).getOrElse(fail("not RTP"))
        )
        _ <- media.retarget(loopback, port"9", MediaDirection.Inactive)
        _ <- peerRtcp.write(
          Datagram(
            SocketAddress(loopback, next(ours)),
            RtcpCodec.encode(
              List(
                RtcpPacket.ReceiverReport(
                  theirs,
                  List(
                    ReportBlock(
                      header.ssrc,
                      LossFraction.None,
                      0,
                      1L,
                      0L,
                      0L,
                      0L
                    )
                  )
                )
              )
            )
          )
        )
        _    <- IO.sleep(300.millis)
        kept <- media.peerReport
      } yield assertEquals(kept, None)
    }
  }
}
