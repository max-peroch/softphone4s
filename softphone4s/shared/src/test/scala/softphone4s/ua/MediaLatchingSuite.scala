package softphone4s.ua

import softphone4s.model.Ssrc
import softphone4s.SipLiterals.pt
import cats.effect.std.Supervisor
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.{Chunk, Stream}
import fs2.io.net.{Datagram, DatagramSocket, Network}
import munit.CatsEffectSuite
import softphone4s.codec.RtpCodec
import softphone4s.config.RtpLatching
import softphone4s.media.{AudioCodecs, JitterConfig}
import softphone4s.model.MediaDirection
import softphone4s.sdp.{Codec, MediaPlan}
import softphone4s.transport.{RawPeer, RtpLatch}

import scala.concurrent.duration.*

/** Media sent to where the remote party's media comes from, over real UDP: the
  * remote party's SDP names one port and its packets come from another, as when
  * a NAT has remapped it.
  */
class MediaLatchingSuite extends CatsEffectSuite {

  private val loopback = ip"127.0.0.1"
  private val anyPort  = SocketAddress(loopback, port"0")

  private def bound
      : Resource[IO, (DatagramSocket[IO], SocketAddress[IpAddress])] =
    Network[IO]
      .bindDatagramSocket(address = anyPort)
      .evalMap(socket => RawPeer.addressOf(socket.address).map(socket -> _))

  private def rtp(sequence: Int): Chunk[Byte] =
    RtpCodec.encode(
      payloadType = pt(0),
      sequenceNumber = sequence,
      timestamp = sequence * 160L,
      ssrc = Ssrc.fromBits(77),
      payload = Chunk.array(new Array[Byte](160)),
      marker = false
    )

  /** Whether `socket` is sent anything within `wait`. */
  private def receivesWithin(socket: DatagramSocket[IO], wait: FiniteDuration) =
    socket.reads.head.compile.last
      .timeout(wait)
      .attempt
      .map(_.exists(_.isDefined))

  private def scenario(
      latching: RtpLatching,
      packetsFromActual: Int
  ): IO[(Boolean, Boolean)] =
    (bound, bound, Supervisor[IO](await = false)).tupled.use {
      case ((signalledSocket, signalled), (actualSocket, _), supervisor) =>
        for {
          // The port our RTP is received on: free, and found by binding it first.
          ours  <- bound.use((_, address) => IO.pure(address.port))
          media <- MediaSession.create[IO](
            loopback,
            ours,
            AudioCodecs.Default,
            latching,
            JitterConfig.Default
          )
          _ <- media.start(
            MediaPlan(
              SocketAddress(loopback, signalled.port),
              Codec.Pcmu,
              pt(0),
              None,
              MediaDirection.SendRecv,
              MediaDirection.SendRecv
            ),
            supervisor,
            _ => IO.unit
          )
          // The media socket is bound in the background; give it time.
          _ <- IO.sleep(500.millis)
          _ <- (1 to packetsFromActual).toList.traverse_(n =>
            actualSocket.write(Datagram(SocketAddress(loopback, ours), rtp(n)))
          )
          _ <- IO.sleep(300.millis)
          _ <- supervisor.supervise(
            Stream
              .chunk(Chunk.array(new Array[Byte](320 * 5)))
              .through(media.sink)
              .compile
              .drain
          )
          atSignalled <- receivesWithin(signalledSocket, 1.second)
          atActual    <- receivesWithin(actualSocket, 1.second)
          _           <- media.stop()
        } yield (atSignalled, atActual)
    }

  test("media follows the remote party's packets to the port they come from") {
    scenario(RtpLatching.Follow, RtpLatch.Threshold + 20)
      .map(assertEquals(_, (false, true)))
  }

  test("a few stray packets do not take the media away") {
    scenario(RtpLatching.Follow, RtpLatch.Threshold - 2)
      .map(assertEquals(_, (true, false)))
  }

  test("with latching ignored, media goes where the SDP said") {
    scenario(RtpLatching.Ignore, 20).map(assertEquals(_, (true, false)))
  }
}
