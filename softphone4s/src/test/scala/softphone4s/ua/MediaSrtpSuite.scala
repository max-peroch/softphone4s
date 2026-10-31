package softphone4s.ua

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{Datagram, DatagramSocket, Network}
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.SipLiterals.pt
import softphone4s.codec.{G711Codec, RtpCodec}
import softphone4s.config.RtpLatching
import softphone4s.media.{AudioCodecs, JitterConfig}
import softphone4s.model.{DtmfDigit, MediaDirection, Ssrc}
import softphone4s.sdp.{Codec, MediaPlan}
import softphone4s.srtp.*
import softphone4s.transport.RawPeer

import scala.concurrent.duration.*

/** Encrypted media over real UDP, against a peer that holds the other end of
  * the keys: what it sends to us is protected with the key we receive with, and
  * what we send it is opened with the key we send with.
  */
class MediaSrtpSuite extends CatsEffectSuite {

  private val loopback = ip"127.0.0.1"
  private val Suite    = SrtpSuite.AesCm128HmacSha1_80

  private def key(n: Int): MasterKey =
    MasterKey(
      Chunk.array(Array.fill[Byte](16)(n.toByte)),
      Chunk.array(Array.fill[Byte](14)((n + 1).toByte))
    ).fold(e => fail(e.message), identity)

  /** We send with key 1 and receive with key 2. */
  private val keys = SrtpPlan(Suite, key(1), key(2))

  private def freePort: IO[Port] =
    Network[IO]
      .bindDatagramSocket(address = SocketAddress(loopback, port"0"))
      .use(socket => RawPeer.addressOf(socket.address))
      .map(_.port)

  /** The media under test, its RTP port, and the peer's socket. */
  private def call(withEvents: Boolean = false) =
    for {
      peerPort <- Resource.eval(freePort)
      ours     <- Resource.eval(freePort)
      peer     <- Network[IO].bindDatagramSocket(
        address = SocketAddress(loopback, peerPort)
      )
      sv    <- Supervisor[IO](await = false)
      media <- Resource.make(
        MediaSession.create[IO](
          loopback,
          ours,
          AudioCodecs.Default,
          RtpLatching.Ignore,
          JitterConfig.Default
        )
      )(_.stop())
      _ <- Resource.eval(
        media.start(
          MediaPlan(
            SocketAddress(loopback, peerPort),
            Codec.Pcmu,
            pt(0),
            Option.when(withEvents)(pt(101)),
            MediaDirection.SendRecv,
            MediaDirection.SendRecv,
            None,
            Some(keys)
          ),
          sv,
          _ => IO.unit
        ) >> IO.sleep(500.millis) // the sockets are bound in the background
      )
    } yield (media, ours, peer)

  private def rtp(sequence: Int, ssrc: Long = 7L): Chunk[Byte] =
    RtpCodec.encode(
      pt(0),
      sequence,
      sequence * 160L,
      Ssrc.fromBits(ssrc.toInt),
      Chunk.array(Array.fill[Byte](160)(0x80.toByte))
    )

  /** What the peer sends: each packet protected, in order, with the key we
    * receive with.
    */
  private def theirs: IO[Ref[IO, SrtpOutbound]] =
    Ref.of(SrtpOutbound(Suite, keys.receive))

  private def send(
      peer: DatagramSocket[IO],
      ours: Port,
      protector: Ref[IO, SrtpOutbound],
      packets: Chunk[Byte]*
  ): IO[Unit] =
    packets.toList.traverse_ { packet =>
      protector
        .modify(out =>
          out
            .protect(packet)
            .fold(e => fail(e.message), (next, onWire) => (next, onWire))
        )
        .flatMap(onWire =>
          peer.write(Datagram(SocketAddress(loopback, ours), onWire))
        )
    }

  private def sendFrames(media: MediaSession[IO], frames: Int) =
    Stream
      .chunk(Chunk.array(Array.fill[Byte](320 * frames)(1)))
      .through(media.sink)
      .compile
      .drain

  test("encrypted audio from the peer is opened and played") {
    call().use { (media, ours, peer) =>
      for {
        protector <- theirs
        _         <- send(peer, ours, protector, rtp(1), rtp(2), rtp(3), rtp(4))
        heard     <- media.source.take(320).compile.to(Chunk).timeout(5.seconds)
      } yield {
        val first = ((heard(1) << 8) | (heard(0) & 0xff)).toShort.toInt
        assertEquals(first, G711Codec.decode(0x80.toByte).toInt)
      }
    }
  }

  test(
    "what we send is encrypted, and the peer opens it with the key we send with"
  ) {
    call().use { (media, _, peer) =>
      for {
        _        <- sendFrames(media, 3)
        datagram <- peer.read.timeout(5.seconds)
        opening = SrtpInbound(Suite, keys.send)
        opened  = opening
          .unprotect(datagram.bytes)
          .fold(e => fail(e.message), (_, packet) => packet)
        header = RtpCodec.decode(opened).getOrElse(fail("not RTP"))
      } yield {
        assertEquals(datagram.bytes.size, 12 + 160 + Suite.tagLength)
        assertEquals(header._1.payloadType, pt(0))
        // The payload on the wire is not the payload sent.
        assertNotEquals(datagram.bytes.drop(12).take(160), header._2)
        // And it is not readable as plain RTP with the same payload.
        assertEquals(header._2.size, 160)
      }
    }
  }

  test("packets that are not encrypted are dropped, not played") {
    call().use { (media, ours, peer) =>
      for {
        _ <- List(1, 2, 3, 4, 5).traverse_(n =>
          peer.write(Datagram(SocketAddress(loopback, ours), rtp(n)))
        )
        _     <- IO.sleep(600.millis)
        stats <- media.jitterStats
      } yield assertEquals(stats.received, 0L)
    }
  }

  test("packets protected with another key are dropped") {
    call().use { (media, ours, peer) =>
      for {
        stranger <- Ref.of[IO, SrtpOutbound](SrtpOutbound(Suite, key(9)))
        _        <- send(peer, ours, stranger, rtp(1), rtp(2), rtp(3))
        _        <- IO.sleep(600.millis)
        stats    <- media.jitterStats
      } yield assertEquals(stats.received, 0L)
    }
  }

  test("a packet sent again is dropped as a replay, and counted once") {
    call().use { (media, ours, peer) =>
      for {
        protector <- theirs
        onWire    <- protector.modify(
          _.protect(rtp(1)).fold(e => fail(e.message), (n, p) => (n, p))
        )
        wire = Datagram(SocketAddress(loopback, ours), onWire)
        _     <- peer.write(wire) >> peer.write(wire) >> peer.write(wire)
        _     <- send(peer, ours, protector, rtp(2), rtp(3))
        _     <- IO.sleep(600.millis)
        stats <- media.jitterStats
      } yield {
        assertEquals(stats.received, 3L)
        assertEquals(stats.duplicates, 0L)
      }
    }
  }

  test(
    "a new stream from the peer, another source with the same key, is taken up"
  ) {
    call().use { (media, ours, peer) =>
      for {
        protector <- theirs
        _         <- send(peer, ours, protector, rtp(1), rtp(2), rtp(3))
        // The peer starts again under another SSRC, with its own count.
        again <- theirs
        _     <- send(
          peer,
          ours,
          again,
          rtp(100, ssrc = 8L),
          rtp(101, ssrc = 8L),
          rtp(102, ssrc = 8L)
        )
        _     <- IO.sleep(600.millis)
        stats <- media.jitterStats
      } yield assertEquals(stats.received, 6L)
    }
  }

  test("a digit is sent encrypted too") {
    call(withEvents = true).use { (media, _, peer) =>
      for {
        sending <- media.sendDtmf(DtmfDigit.D5, 100.millis)
        packets <- peer.reads.take(7).compile.toList.timeout(5.seconds)
      } yield {
        assert(sending)
        val opened = packets
          .foldLeft((SrtpInbound(Suite, keys.send), List.empty[Chunk[Byte]])) {
            case ((in, done), datagram) =>
              val Right((next, packet)) =
                in.unprotect(datagram.bytes): @unchecked
              (next, done :+ packet)
          }
          ._2
        val types =
          opened.flatMap(p => RtpCodec.decode(p).map(_._1.payloadType))
        assertEquals(types.distinct, List(pt(101)))
        assertEquals(opened.size, 7)
      }
    }
  }
}
