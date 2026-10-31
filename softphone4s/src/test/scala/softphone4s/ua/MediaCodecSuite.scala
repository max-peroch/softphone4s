package softphone4s.ua

import cats.effect.std.Supervisor
import cats.effect.{IO, Resource}
import com.comcast.ip4s.*
import fs2.io.net.{Datagram, DatagramSocket, Network}
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.codec.{G711Codec, RtpCodec}
import softphone4s.config.RtpLatching
import softphone4s.media.AudioCodecs
import softphone4s.model.MediaDirection
import softphone4s.sdp.{Capabilities, Codec, MediaPlan}
import softphone4s.transport.RawPeer

import scala.concurrent.duration.*

/** Media in the codec that was negotiated, over real UDP. */
class MediaCodecSuite extends CatsEffectSuite {

  private val loopback = ip"127.0.0.1"
  private val anyPort  = SocketAddress(loopback, port"0")

  private def bound
      : Resource[IO, (DatagramSocket[IO], SocketAddress[IpAddress])] =
    Network[IO]
      .bindDatagramSocket(address = anyPort)
      .evalMap(socket => RawPeer.addressOf(socket.address).map(socket -> _))

  private def planFor(
      codec: Codec,
      payloadType: Int,
      remote: SocketAddress[IpAddress]
  ) =
    MediaPlan(
      remote,
      codec,
      payloadType,
      None,
      MediaDirection.SendRecv,
      MediaDirection.SendRecv
    )

  /** A media session on a port that was free a moment ago. */
  private def session =
    for {
      ours       <- Resource.eval(bound.use((_, address) => IO.pure(address)))
      supervisor <- Supervisor[IO](await = false)
      media      <- Resource.eval(
        MediaSession.create[IO](
          loopback,
          ours.port,
          AudioCodecs.Default,
          RtpLatching.Ignore
        )
      )
    } yield (media, ours, supervisor)

  /** Runs `media` as `plan` says against a peer socket; the peer's address is
    * in the plan.
    */
  private def withPeer[A](codec: Codec, payloadType: Int)(
      use: (
          MediaSession[IO],
          SocketAddress[IpAddress],
          DatagramSocket[IO]
      ) => IO[A]
  ): IO[A] =
    bound.use { (peerSocket, peerAddress) =>
      session.use { (media, ours, supervisor) =>
        media.start(
          planFor(codec, payloadType, peerAddress),
          supervisor,
          _ => IO.unit
        ) >>
          IO.sleep(500.millis) >>
          use(media, ours, peerSocket) <* media.stop()
      }
    }

  private def payload(n: Int): Chunk[Byte] =
    Chunk.array(Array.tabulate[Byte](160)(i => (i + n).toByte))

  test("media in A-law is sent as payload type 8 with 160 bytes every 20 ms") {
    withPeer(Codec.Pcma, 8) { (media, _, peer) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(new Array[Byte](320 * 3)))
          .through(media.sink)
          .compile
          .drain
          .start
        packets <- peer.reads.take(3).compile.toList.timeout(5.seconds)
      } yield {
        val decoded = packets.map(d => RtpCodec.decode(d.bytes).get)
        assertEquals(decoded.map(_._1.payloadType), List(8, 8, 8))
        assertEquals(decoded.map(_._2.size), List(160, 160, 160))
        assertEquals(decoded.map(_._1.timestamp), List(0L, 160L, 320L))
        // Silence in A-law, not in μ-law.
        assertEquals(
          decoded.head._2.toArray.map(_ & 0xff).distinct.toList,
          List(0xd5)
        )
      }
    }
  }

  test(
    "A-law payload received is played back as PCM, and other payload types are not"
  ) {
    withPeer(Codec.Pcma, 8) { (media, ours, peer) =>
      val sample = payload(0)
      val packet = (pt: Int, seq: Int) =>
        RtpCodec.encode(pt, seq, seq * 160L, 5L, sample, marker = false)
      for {
        _ <- peer.write(
          Datagram(ours, packet(0, 1))
        ) // μ-law: not what was agreed
        _   <- peer.write(Datagram(ours, packet(8, 2)))
        got <- media.source.take(320).compile.to(Chunk).timeout(5.seconds)
      } yield assertEquals(
        got.toArray.toList,
        G711Codec.alawToPcm(sample.toArray).toList
      )
    }
  }

  test("a session change keeps to the codec in use") {
    val both = Capabilities.Default
    withPeer(Codec.Pcma, 8) { (media, _, _) =>
      media
        .restrict(both)
        .map(caps => assertEquals(caps.codecs.toList, List(Codec.Pcma)))
    }
  }

  test("before media starts, the capabilities are whole") {
    session.use { (media, _, _) =>
      media
        .restrict(Capabilities.Default)
        .map(assertEquals(_, Capabilities.Default))
    }
  }

  test("a plan for a codec we do not have fails to start, with a typed error") {
    val g729 = Codec("G729", 8000, 1, Some(18), None)
    session.use { (media, _, supervisor) =>
      media
        .start(
          planFor(g729, 18, SocketAddress(loopback, port"9")),
          supervisor,
          _ => IO.unit
        )
        .attempt
        .map(r =>
          assertEquals(
            r.left.toOption.map(_.getMessage),
            Some("No implementation of the codec G729")
          )
        )
    }
  }
}
