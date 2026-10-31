package softphone4s.ua

import softphone4s.SipLiterals.pt
import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{Datagram, DatagramSocket, Network}
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.codec.{G711Codec, RtpCodec}
import softphone4s.config.RtpLatching
import softphone4s.media.{AudioCodecs, HalfByteCodec, JitterConfig}
import softphone4s.model.{
  DtmfDigit,
  MediaDirection,
  PayloadType,
  SoftphoneEvent
}
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
      pt(payloadType),
      None,
      MediaDirection.SendRecv,
      MediaDirection.SendRecv
    )

  /** A media session on a port that was free a moment ago. */
  private def session(codecs: AudioCodecs = AudioCodecs.Default) =
    for {
      ours       <- Resource.eval(bound.use((_, address) => IO.pure(address)))
      supervisor <- Supervisor[IO](await = false)
      media      <- Resource.eval(
        MediaSession.create[IO](
          loopback,
          ours.port,
          codecs,
          RtpLatching.Ignore,
          JitterConfig.Default
        )
      )
    } yield (media, ours, supervisor)

  /** Runs `media` as `plan` says against a peer socket; the peer's address is
    * in the plan.
    */
  private def withPeer[A](
      codec: Codec,
      payloadType: Int,
      codecs: AudioCodecs = AudioCodecs.Default
  )(
      use: (
          MediaSession[IO],
          SocketAddress[IpAddress],
          DatagramSocket[IO]
      ) => IO[A]
  ): IO[A] =
    bound.use { (peerSocket, peerAddress) =>
      session(codecs).use { (media, ours, supervisor) =>
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
        assertEquals(decoded.map(_._1.payloadType), List(pt(8), pt(8), pt(8)))
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
      val packet = (payload: Int, seq: Int) =>
        RtpCodec.encode(
          pt(payload),
          seq,
          seq * 160L,
          5L,
          sample,
          marker = false
        )
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
    session().use { (media, _, _) =>
      media
        .restrict(Capabilities.Default)
        .map(assertEquals(_, Capabilities.Default))
    }
  }

  test("a plan for a codec we do not have fails to start, with a typed error") {
    val g729 = Codec("G729", 8000, 1, Some(pt(18)), None)
    session().use { (media, _, supervisor) =>
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

  private val wideband = AudioCodecs
    .resolve(
      cats.data.NonEmptyList.one(softphone4s.config.CodecChoice.G722),
      Nil
    )
    .fold(e => fail(e.getMessage), identity)

  private def reference(name: String): Array[Byte] =
    scala.util.Using.resource(getClass.getResourceAsStream(s"/g722/$name"))(
      _.readAllBytes()
    )

  test(
    "media in G.722 is sent as payload type 9, 160 bytes every 20 ms on a clock of 8 000"
  ) {
    val pcm = reference("input.pcm")
    withPeer(Codec.G722, 9, wideband) { (media, _, peer) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(pcm.take(640 * 3)))
          .through(media.sink)
          .compile
          .drain
          .start
        packets <- peer.reads.take(3).compile.toList.timeout(5.seconds)
      } yield {
        val decoded = packets.map(d => RtpCodec.decode(d.bytes).get)
        assertEquals(decoded.map(_._1.payloadType), List(pt(9), pt(9), pt(9)))
        assertEquals(decoded.map(_._2.size), List(160, 160, 160))
        assertEquals(decoded.map(_._1.timestamp), List(0L, 160L, 320L))
        // The reference's bytes for the same PCM.
        assertEquals(
          decoded.flatMap(_._2.toArray).toList,
          reference("reference.g722").take(480).toList
        )
      }
    }
  }

  test("G.722 received is played back as the reference plays it") {
    val codes = reference("reference.g722").take(160)
    withPeer(Codec.G722, 9, wideband) { (media, ours, peer) =>
      for {
        _ <- peer.write(
          Datagram(
            ours,
            RtpCodec.encode(
              pt(9),
              1,
              0L,
              5L,
              Chunk.array(codes),
              marker = false
            )
          )
        )
        got <- media.source.take(640).compile.to(Chunk).timeout(5.seconds)
      } yield assertEquals(
        got.toArray.toList,
        reference("reference-decoded.pcm").take(640).toList
      )
    }
  }

  private val half = AudioCodecs
    .resolve(
      cats.data.NonEmptyList
        .one(softphone4s.config.CodecChoice.Named("x-half")),
      List(HalfByteCodec)
    )
    .fold(e => fail(e.getMessage), identity)

  test("a codec the program added runs on the payload type it was given") {
    withPeer(HalfByteCodec.description, 96, half) { (media, ours, peer) =>
      val pcm = Chunk.array(
        Array.tabulate[Byte](640)(i => (i % 2 * 3 + i / 2 % 50).toByte)
      )
      for {
        _       <- Stream.chunk(pcm).through(media.sink).compile.drain.start
        packets <- peer.reads.take(2).compile.toList.timeout(5.seconds)
        _       <- peer.write(
          Datagram(
            ours,
            RtpCodec.encode(
              pt(96),
              1,
              0L,
              5L,
              Chunk.array(Array.fill[Byte](160)(7)),
              marker = false
            )
          )
        )
        back <- media.source.take(320).compile.to(Chunk).timeout(5.seconds)
      } yield {
        val decoded = packets.map(d => RtpCodec.decode(d.bytes).get)
        assertEquals(decoded.map(_._1.payloadType), List(pt(96), pt(96)))
        assertEquals(decoded.map(_._2.size), List(160, 160))
        assertEquals(
          decoded.head._2.toArray.toList,
          pcm.toArray.take(320).grouped(2).map(_(1)).toList
        )
        assertEquals(
          back.toArray.toList,
          List.fill(160)(List[Byte](0, 7)).flatten
        )
      }
    }
  }

  /** A codec that fails to make its second packet, as one can. */
  private object DropsTheSecond extends softphone4s.media.AudioCodec {
    val token: String      = "x-drops"
    val description: Codec = Codec("x-drops", 8000, 1, None, None)
    val pcmRate: Int       = 8000

    def encoder[F[_]: cats.effect.Sync]: fs2.Pipe[F, Byte, Chunk[Byte]] =
      softphone4s.media.AudioCodec.encoding(
        pcmPacketBytes,
        softphone4s.media.AudioCodec.stateful {
          var seen = 0
          pcm => {
            seen += 1
            if seen == 2 then Array.emptyByteArray else pcm.take(160)
          }
        }
      )

    def decoderSession[F[_]: cats.effect.Sync]
        : cats.effect.Resource[F, softphone4s.media.AudioCodec.Session] =
      softphone4s.media.AudioCodec.stateless(payload => payload)
  }

  test(
    "a packet the codec could not make leaves a gap in time, not a shorter timeline"
  ) {
    val codecs = AudioCodecs
      .resolve(
        cats.data.NonEmptyList
          .one(softphone4s.config.CodecChoice.Named("x-drops")),
        List(DropsTheSecond)
      )
      .fold(e => fail(e.getMessage), identity)
    withPeer(DropsTheSecond.description, 96, codecs) { (media, _, peer) =>
      for {
        _ <- Stream
          .chunk(Chunk.array(new Array[Byte](320 * 3)))
          .through(media.sink)
          .compile
          .drain
          .start
        packets <- peer.reads.take(2).compile.toList.timeout(5.seconds)
      } yield {
        val decoded = packets.map(d => RtpCodec.decode(d.bytes).get._1)
        // The first and the third of three: the second is missing and its 20 ms
        // are not. Sequence numbers count packets sent, so they do not skip;
        // the marker says that there was a gap.
        assertEquals(decoded.map(_.timestamp), List(0L, 320L))
        assertEquals(decoded.map(_.sequenceNumber), List(0, 1))
        assertEquals(decoded.map(_.marker), List(true, true))
      }
    }
  }

  // ---- Digits, both ways --------------------------------------------------------------

  /** A session started with telephone events on 101, handed with its RTP
    * address, a socket that plays the other side, and the digits it has heard.
    */
  private def hearing[A](
      use: (
          MediaSession[IO],
          SocketAddress[IpAddress],
          DatagramSocket[IO],
          IO[List[DtmfDigit]]
      ) => IO[A]
  ): IO[A] =
    bound.use { (peer, peerAddress) =>
      session(AudioCodecs.Default).use { (media, ours, supervisor) =>
        Ref.of[IO, List[DtmfDigit]](Nil).flatMap { digits =>
          val plan = MediaPlan(
            peerAddress,
            Codec.Pcmu,
            pt(0),
            Some(pt(101)),
            MediaDirection.SendRecv,
            MediaDirection.SendRecv
          )
          media.start(plan, supervisor, d => digits.update(_ :+ d)) >>
            IO.sleep(500.millis) >>
            use(media, ours, peer, digits.get) <*
            media.stop()
        }
      }
    }

  private def event(
      sequence: Int,
      timestamp: Long,
      digit: DtmfDigit,
      end: Boolean,
      duration: Int = 800,
      ssrc: Long = 9L
  ): Chunk[Byte] =
    RtpCodec.encode(
      pt(101),
      sequence,
      timestamp,
      ssrc,
      SoftphoneEvent.encode(SoftphoneEvent(digit.code, end, 10, duration)),
      marker = false
    )

  test(
    "a digit we send is heard once by a session that gets all its packets, though the last comes three times"
  ) {
    hearing { (media, _, peer, _) =>
      // The far end runs the same code as we do.
      hearing { (_, otherAddress, _, heard) =>
        for {
          sent    <- media.sendDtmf(DtmfDigit.D7, 100.millis)
          packets <- peer.reads.take(7).compile.toList.timeout(5.seconds)
          _       <- packets.traverse_(d =>
            peer.write(Datagram(otherAddress, d.bytes))
          )
          _      <- IO.sleep(300.millis)
          digits <- heard
        } yield {
          assert(sent)
          assertEquals(digits, List(DtmfDigit.D7))
        }
      }
    }
  }

  test("a digit is heard if only the packets that end it get through") {
    hearing { (_, ours, peer, heard) =>
      for {
        _ <- List(3, 4, 5).traverse_(i =>
          peer.write(
            Datagram(ours, event(i, 1000L, DtmfDigit.Hash, end = true))
          )
        )
        _      <- IO.sleep(300.millis)
        digits <- heard
      } yield assertEquals(digits, List(DtmfDigit.Hash))
    }
  }

  test(
    "a digit is not heard until it ends, and not again for the packets that repeat the end"
  ) {
    hearing { (_, ours, peer, heard) =>
      for {
        _ <- List(1, 2).traverse_(i =>
          peer.write(
            Datagram(
              ours,
              event(i, 1000L, DtmfDigit.D4, end = false, duration = i * 160)
            )
          )
        )
        _      <- IO.sleep(200.millis)
        before <- heard
        _      <- List(3, 4, 5).traverse_(i =>
          peer.write(Datagram(ours, event(i, 1000L, DtmfDigit.D4, end = true)))
        )
        _     <- IO.sleep(300.millis)
        after <- heard
      } yield {
        assertEquals(before, Nil)
        assertEquals(after, List(DtmfDigit.D4))
      }
    }
  }

  test("the same digit pressed twice is two digits, and two digits are two") {
    hearing { (_, ours, peer, heard) =>
      val ends = List(
        (1, 1000L, DtmfDigit.D2),
        (2, 1000L, DtmfDigit.D2), // a repeat of the end
        (3, 3000L, DtmfDigit.D2), // pressed again, later
        (4, 5000L, DtmfDigit.A)
      )
      for {
        _ <- ends.traverse_((i, ts, d) =>
          peer.write(Datagram(ours, event(i, ts, d, end = true)))
        )
        _      <- IO.sleep(300.millis)
        digits <- heard
      } yield assertEquals(
        digits,
        List(DtmfDigit.D2, DtmfDigit.D2, DtmfDigit.A)
      )
    }
  }

  test(
    "a digit cannot be sent by a session that has not started, or whose peer did not agree"
  ) {
    session(AudioCodecs.Default).use { (media, _, _) =>
      media.sendDtmf(DtmfDigit.D1, 100.millis).map(assertEquals(_, false))
    } >> withPeer(Codec.Pcmu, 0) { (media, _, peer) =>
      // Started with no telephone events in the plan.
      media
        .sendDtmf(DtmfDigit.D1, 100.millis)
        .flatMap(sent =>
          peer.reads.interruptAfter(300.millis).compile.toList.map { packets =>
            assert(!sent)
            assertEquals(packets, Nil)
          }
        )
    }
  }

  // ---- Media that starts early ---------------------------------------------------------

  private def planTo(
      remote: SocketAddress[IpAddress],
      codec: Codec = Codec.Pcmu,
      payloadType: Int = 0,
      direction: MediaDirection = MediaDirection.SendRecv
  ) =
    MediaPlan(
      remote,
      codec,
      pt(payloadType),
      None,
      direction,
      MediaDirection.SendRecv
    )

  private val twoPackets = Chunk.array(new Array[Byte](320 * 2))

  private def packetsWithin(socket: DatagramSocket[IO], wait: FiniteDuration) =
    socket.reads.interruptAfter(wait).compile.toList

  test(
    "media that is running is not started again: it follows the new answer, and says so"
  ) {
    (bound, bound, session()).tupled.use {
      case (
            (first, firstAddress),
            (second, secondAddress),
            (media, _, supervisor)
          ) =>
        for {
          started <- media.start(planTo(firstAddress), supervisor, _ => IO.unit)
          _       <- IO.sleep(500.millis)
          _       <- Stream.chunk(twoPackets).through(media.sink).compile.drain
          atFirst <- packetsWithin(first, 300.millis)
          followed <- media.start(
            planTo(secondAddress),
            supervisor,
            _ => IO.unit
          )
          _        <- Stream.chunk(twoPackets).through(media.sink).compile.drain
          atSecond <- packetsWithin(second, 300.millis)
          moreAtFirst <- packetsWithin(first, 200.millis)
          _           <- media.stop()
        } yield {
          assertEquals(started, MediaStart.Started)
          assertEquals(followed, MediaStart.Followed)
          assertEquals(atFirst.size, 2)
          assertEquals(atSecond.size, 2)
          assertEquals(moreAtFirst, Nil)
        }
    }
  }

  test(
    "an answer that changes the codec is followed in address, and the early codec is kept"
  ) {
    (bound, session()).tupled.use {
      case ((peer, address), (media, _, supervisor)) =>
        for {
          _       <- media.start(planTo(address), supervisor, _ => IO.unit)
          _       <- IO.sleep(500.millis)
          changed <- media.start(
            planTo(address, Codec.Pcma, 8),
            supervisor,
            _ => IO.unit
          )
          _       <- Stream.chunk(twoPackets).through(media.sink).compile.drain
          packets <- packetsWithin(peer, 300.millis)
          _       <- media.stop()
        } yield {
          assertEquals(changed, MediaStart.CodecChanged)
          assertEquals(
            packets.map(d => RtpCodec.decode(d.bytes).get._1.payloadType),
            List(pt(0), pt(0))
          )
        }
    }
  }

  test(
    "media is not sent to a party that says it will not receive, until it says it will"
  ) {
    (bound, session()).tupled.use {
      case ((peer, address), (media, _, supervisor)) =>
        for {
          _ <- media.start(
            planTo(address, direction = MediaDirection.SendOnly),
            supervisor,
            _ => IO.unit
          )
          _      <- IO.sleep(500.millis)
          _      <- Stream.chunk(twoPackets).through(media.sink).compile.drain
          silent <- packetsWithin(peer, 300.millis)
          _      <- media.start(planTo(address), supervisor, _ => IO.unit)
          _      <- Stream.chunk(twoPackets).through(media.sink).compile.drain
          heard  <- packetsWithin(peer, 300.millis)
          _      <- media.stop()
        } yield {
          assertEquals(silent, Nil)
          assertEquals(heard.size, 2)
        }
    }
  }

  // ---- Comfort noise ---------------------------------------------------------------------

  /** A session started on the plan `make` gives for the peer's address. */
  private def startedOn[A](make: SocketAddress[IpAddress] => MediaPlan)(
      use: (
          MediaSession[IO],
          SocketAddress[IpAddress],
          DatagramSocket[IO]
      ) => IO[A]
  ): IO[A] =
    bound.use { (peer, peerAddress) =>
      session(AudioCodecs.Default).use { (media, ours, supervisor) =>
        media.start(make(peerAddress), supervisor, _ => IO.unit) >>
          IO.sleep(500.millis) >>
          use(media, ours, peer) <* media.stop()
      }
    }

  private def noisePlan(
      direction: MediaDirection = MediaDirection.SendRecv,
      comfort: Option[PayloadType] = Some(pt(13))
  )(address: SocketAddress[IpAddress]) =
    MediaPlan(
      address,
      Codec.Pcmu,
      pt(0),
      None,
      direction,
      MediaDirection.SendRecv,
      comfort
    )

  /** The level 40 -dBov, as a comfort noise packet. */
  private def noisePacket(ours: SocketAddress[IpAddress], sequence: Int) =
    Datagram(
      ours,
      RtpCodec.encode(
        pt(13),
        sequence,
        sequence * 160L,
        7L,
        Chunk.array(Array[Byte](40)),
        marker = false
      )
    )

  private def speech(ours: SocketAddress[IpAddress], sequence: Int) =
    Datagram(
      ours,
      RtpCodec.encode(
        pt(0),
        sequence,
        sequence * 160L,
        7L,
        Chunk.array(Array.fill[Byte](160)(0xff.toByte)),
        marker = false
      )
    )

  private def heardWithin(media: MediaSession[IO], wait: FiniteDuration) =
    media.source.interruptAfter(wait).compile.to(Chunk)

  private def loudness(pcm: Chunk[Byte]): Double = {
    val xs = pcm.toArray
      .grouped(2)
      .map(b => ((b(1) << 8) | (b(0) & 0xff)).toShort.toDouble)
      .toVector
    math.sqrt(xs.map(x => x * x).sum / xs.size)
  }

  test(
    "comfort noise from the other side is played, at its level, while it is quiet"
  ) {
    startedOn(noisePlan()) { (media, ours, peer) =>
      for {
        _     <- peer.write(noisePacket(ours, 1))
        heard <- media.source.take(320 * 5).compile.to(Chunk).timeout(5.seconds)
      } yield {
        assertEquals(heard.size, 320 * 5)
        // Level 40 -dBov: a root mean square of about 328.
        val rms = loudness(heard)
        assert(rms > 200 && rms < 500, s"rms $rms")
      }
    }
  }

  test("the noise ends when speech comes") {
    startedOn(noisePlan()) { (media, ours, peer) =>
      for {
        _     <- peer.write(noisePacket(ours, 1))
        _     <- media.source.take(320).compile.drain.timeout(5.seconds)
        _     <- peer.write(speech(ours, 2))
        _     <- IO.sleep(150.millis)
        _     <- heardWithin(media, 100.millis) // what was queued
        later <- heardWithin(media, 400.millis)
      } yield assertEquals(later.size, 0)
    }
  }

  test("the noise ends when the other side stops sending, as in a hold") {
    startedOn(noisePlan()) { (media, ours, peer) =>
      for {
        _     <- peer.write(noisePacket(ours, 1))
        _     <- media.source.take(320).compile.drain.timeout(5.seconds)
        _     <- media.retarget(ours.host, ours.port, MediaDirection.RecvOnly)
        _     <- IO.sleep(150.millis)
        _     <- heardWithin(media, 100.millis)
        later <- heardWithin(media, 400.millis)
      } yield assertEquals(later.size, 0)
    }
  }

  test("a party that sends nothing makes no noise") {
    startedOn(noisePlan(direction = MediaDirection.RecvOnly)) {
      (media, ours, peer) =>
        for {
          _     <- peer.write(noisePacket(ours, 1))
          heard <- heardWithin(media, 500.millis)
        } yield assertEquals(heard.size, 0)
    }
  }

  test("comfort noise that was not agreed to is not played") {
    startedOn(noisePlan(comfort = None)) { (media, ours, peer) =>
      for {
        _     <- peer.write(noisePacket(ours, 1))
        heard <- heardWithin(media, 500.millis)
      } yield assertEquals(heard.size, 0)
    }
  }

  test(
    "a packet that is no news of the noise, with no level in it, plays nothing"
  ) {
    startedOn(noisePlan()) { (media, ours, peer) =>
      for {
        _ <- peer.write(
          Datagram(
            ours,
            RtpCodec.encode(
              pt(13),
              1,
              160L,
              7L,
              Chunk.empty[Byte],
              marker = false
            )
          )
        )
        heard <- heardWithin(media, 500.millis)
      } yield assertEquals(heard.size, 0)
    }
  }

  test("two starts at once begin the media once: the other follows") {
    (bound, session()).tupled.use {
      case ((_, address), (media, _, supervisor)) =>
        List(1, 2)
          .parTraverse(_ =>
            media.start(planTo(address), supervisor, _ => IO.unit)
          )
          .flatTap(_ => media.stop())
          .map(results =>
            assertEquals(
              results.sortBy(_.ordinal),
              List(MediaStart.Started, MediaStart.Followed)
            )
          )
    }
  }

  test(
    "a start that fails leaves the media not started, so that a good one can follow"
  ) {
    val g729 = Codec("G729", 8000, 1, Some(pt(18)), None)
    (bound, session()).tupled.use {
      case ((_, address), (media, _, supervisor)) =>
        for {
          failed <- media
            .start(planTo(address, g729, 18), supervisor, _ => IO.unit)
            .attempt
          started <- media.start(planTo(address), supervisor, _ => IO.unit)
          _       <- media.stop()
        } yield {
          assert(failed.isLeft)
          assertEquals(started, MediaStart.Started)
        }
    }
  }

  // ---- The jitter buffer in the media -------------------------------------------------

  /** A packet of 160 samples that all have the value of μ-law code `code`. */
  private def frameOfCode(
      ours: SocketAddress[IpAddress],
      sequence: Int,
      code: Int
  ) =
    Datagram(
      ours,
      RtpCodec.encode(
        pt(0),
        sequence,
        sequence * 160L,
        7L,
        Chunk.array(Array.fill[Byte](160)(code.toByte)),
        marker = false
      )
    )

  private def decoded(code: Int): Int = G711Codec.decode(code.toByte).toInt

  /** The first sample of each 20 ms frame of `pcm`. */
  private def firstSamples(pcm: Chunk[Byte]): List[Int] =
    pcm.toArray
      .grouped(320)
      .map(f => ((f(1) << 8) | (f(0) & 0xff)).toShort.toInt)
      .toList

  private val full = noisePlan(comfort = None)

  test("packets that arrive out of order are played in order") {
    startedOn(full) { (media, ours, peer) =>
      for {
        _ <- List(1 -> 0x80, 3 -> 0x90, 2 -> 0xa0, 4 -> 0xb0).traverse_(
          (n, c) => peer.write(frameOfCode(ours, n, c))
        )
        heard <- media.source.take(320 * 4).compile.to(Chunk).timeout(5.seconds)
      } yield assertEquals(
        firstSamples(heard),
        List(0x80, 0xa0, 0x90, 0xb0).map(decoded)
      )
    }
  }

  test("a packet that is lost is concealed with the one before it, quieter") {
    startedOn(full) { (media, ours, peer) =>
      for {
        _ <- List(1 -> 0x80, 2 -> 0x90, 4 -> 0xb0, 5 -> 0xc0).traverse_(
          (n, c) => peer.write(frameOfCode(ours, n, c))
        )
        heard <- media.source.take(320 * 5).compile.to(Chunk).timeout(5.seconds)
        stats <- media.jitterStats
      } yield {
        val got = firstSamples(heard)
        assertEquals(got.take(2), List(0x80, 0x90).map(decoded))
        assertEquals(got(2), decoded(0x90) * 2 / 3)
        assertEquals(got.drop(3), List(0xb0, 0xc0).map(decoded))
        assertEquals(stats.lost, 1L)
      }
    }
  }

  test(
    "a duplicate and a packet that comes too late are counted and not played"
  ) {
    startedOn(full) { (media, ours, peer) =>
      for {
        _ <- List(1 -> 0x80, 2 -> 0x90, 2 -> 0x90, 4 -> 0xb0, 5 -> 0xc0)
          .traverse_((n, c) => peer.write(frameOfCode(ours, n, c)))
        _     <- media.source.take(320 * 5).compile.drain.timeout(5.seconds)
        _     <- peer.write(frameOfCode(ours, 3, 0xa0)) // after its turn
        _     <- IO.sleep(200.millis)
        stats <- media.jitterStats
      } yield {
        assertEquals(stats.duplicates, 1L)
        assertEquals(stats.late, 1L)
        assertEquals(stats.lost, 1L)
        assertEquals(stats.played, 4L)
      }
    }
  }

  test(
    "a digit in the middle of the speech is heard at once, and its packets are not taken for loss"
  ) {
    val plan = (address: SocketAddress[IpAddress]) =>
      MediaPlan(
        address,
        Codec.Pcmu,
        pt(0),
        Some(pt(101)),
        MediaDirection.SendRecv,
        MediaDirection.SendRecv
      )
    startedOn(plan) { (media, ours, peer) =>
      val digit = (n: Int) =>
        Datagram(
          ours,
          event(
            n,
            640L,
            DtmfDigit.D8,
            end = n >= 3,
            duration = n * 160,
            ssrc = 7L
          )
        )
      for {
        _     <- peer.write(frameOfCode(ours, 1, 0x80))
        _     <- peer.write(digit(2))
        _     <- peer.write(digit(3))
        _     <- peer.write(frameOfCode(ours, 4, 0x90))
        _     <- peer.write(frameOfCode(ours, 5, 0xa0))
        heard <- media.source.take(320 * 2).compile.to(Chunk).timeout(5.seconds)
        _     <- IO.sleep(200.millis)
        stats <- media.jitterStats
      } yield {
        assertEquals(stats.lost, 0L)
        assertEquals(stats.received, 5L)
        // The speech either side of the digit.
        assertEquals(firstSamples(heard), List(0x80, 0x90).map(decoded))
      }
    }
  }

  test(
    "speech that was queued before the comfort noise is played first, and the noise follows it"
  ) {
    startedOn(noisePlan()) { (media, ours, peer) =>
      for {
        _ <- List(1, 2, 3).traverse_(n =>
          peer.write(frameOfCode(ours, n, 0x80))
        )
        _     <- peer.write(noisePacket(ours, 4))
        heard <- media.source.take(320 * 8).compile.to(Chunk).timeout(5.seconds)
      } yield {
        val frames = heard.toArray.grouped(320).map(f => Chunk.array(f)).toList
        // The three frames of speech, then frames of noise, none of speech.
        assertEquals(
          frames.take(3).map(f => firstSamples(f).head),
          List.fill(3)(decoded(0x80))
        )
        assert(
          frames.drop(3).forall(f => loudness(f) > 100 && loudness(f) < 700),
          frames.drop(3).map(loudness).toString
        )
      }
    }
  }
}
