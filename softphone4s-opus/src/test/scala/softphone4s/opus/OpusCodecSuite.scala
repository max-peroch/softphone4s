package softphone4s.opus

import cats.data.NonEmptyList
import cats.effect.std.Supervisor
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{Datagram, DatagramSocket, Network}
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.codec.RtpCodec
import softphone4s.config.{CodecChoice, RtpLatching}
import softphone4s.media.{AudioCodec, AudioCodecs}
import softphone4s.model.{MediaDirection, PayloadType}
import softphone4s.sdp.{
  Capabilities,
  LocalMedia,
  MediaPlan,
  OfferAnswer,
  Reject,
  SessionDescription
}
import softphone4s.ua.MediaSession

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*

/** Opus through libopus. What needs the library is skipped when it cannot be
  * loaded; give its path in the system property `softphone4s.opus.library` (or
  * the environment variable `SOFTPHONE4S_OPUS_LIBRARY`) to run those.
  */
class OpusCodecSuite extends CatsEffectSuite {

  /** Where libopus is, if we have been told: a system property, an environment
    * variable, or the first line of `.opus-library` (in the module's directory,
    * which git ignores).
    */
  private def pt(n: Int): PayloadType =
    PayloadType(n).fold(e => fail(e), identity)

  private val library: Option[Path] =
    sys.props
      .get("softphone4s.opus.library")
      .orElse(sys.env.get("SOFTPHONE4S_OPUS_LIBRARY"))
      .orElse(
        List(
          Path.of(".opus-library"),
          Path.of("softphone4s-opus/.opus-library")
        )
          .find(Files.isRegularFile(_))
          .flatMap(file => Files.readAllLines(file).asScala.headOption)
      )
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Path.of(_))

  private val settings = OpusSettings(library = library)

  /** Runs `use` with the codec, or skips the test if libopus is not there. */
  private def withCodec(s: OpusSettings = settings)(
      use: AudioCodec => IO[Any]
  ): IO[Unit] =
    OpusCodec.resource[IO](s).attempt.use {
      case Left(missing: OpusError.LibraryNotFound) =>
        IO.delay(assume(false, missing.getMessage))
      case Left(error)  => IO.raiseError(error)
      case Right(codec) => use(codec).void
    }

  private def tone(samples: Int, rate: Int = 16000): Chunk[Byte] =
    Chunk.array(
      (0 until samples).flatMap { i =>
        val v = (8000 * math.sin(2 * math.Pi * 440 * i / rate)).toInt
        List((v & 0xff).toByte, ((v >> 8) & 0xff).toByte)
      }.toArray
    )

  private def samples(pcm: Chunk[Byte]): Vector[Int] =
    pcm.toArray
      .grouped(2)
      .map(b => ((b(1) << 8) | (b(0) & 0xff)).toShort.toInt)
      .toVector

  private def encode(codec: AudioCodec, pcm: Chunk[Byte]) =
    Stream.chunk(pcm).through(codec.encoder[IO]).compile.toList

  // ---- Without the library -----------------------------------------------------------

  test("a library that is not there is a typed error naming where it looked") {
    val nowhere = Path.of("/nonexistent/libopus.so")
    OpusCodec
      .resource[IO](OpusSettings(library = Some(nowhere)))
      .use_
      .attempt
      .map(r =>
        assertEquals(
          r.left.toOption,
          Some(OpusError.LibraryNotFound(List(nowhere.toString)))
        )
      )
  }

  test("settings Opus does not allow are refused before anything is loaded") {
    List(
      settings.copy(bitrate = Some(100)),
      settings.copy(complexity = Some(11)),
      settings.copy(expectedPacketLoss = 101)
    ).traverse_(s =>
      OpusCodec
        .resource[IO](s)
        .use_
        .attempt
        .map(r =>
          assert(
            r.left.exists(_.isInstanceOf[OpusError.InvalidSettings]),
            r.toString
          )
        )
    )
  }

  test("SDP calls it opus/48000/2 whatever the audio rate, and asks for mono") {
    val d = OpusCodec.description(OpusSettings(OpusSampleRate.Hz16000))
    assertEquals(
      (d.encoding, d.clockRate, d.channels, d.staticPayloadType),
      ("opus", 48000, 2, None)
    )
    val fmtp = d.fmtp.getOrElse("")
    List(
      "useinbandfec=1",
      "stereo=0",
      "sprop-stereo=0",
      "maxplaybackrate=16000"
    ).foreach(p => assert(fmtp.split(';').contains(p), fmtp))
    assert(
      OpusCodec
        .description(OpusSettings(inbandFec = InbandFec.Disabled))
        .fmtp
        .exists(_.contains("useinbandfec=0"))
    )
  }

  test(
    "a caller's Opus is answered with the number it gave, and the parameters of ours"
  ) {
    val caps = Capabilities(
      NonEmptyList.one(OpusCodec.description(OpusSettings())),
      Some(pt(101))
    )
    val offer = SessionDescription
      .parse(
        "v=0\r\no=- 1 1 IN IP4 198.51.100.1\r\ns=-\r\nc=IN IP4 198.51.100.1\r\nt=0 0\r\n" +
          "m=audio 4000 RTP/AVP 111 0\r\na=rtpmap:111 opus/48000/2\r\na=fmtp:111 minptime=10;useinbandfec=1\r\n"
      )
      .fold(e => fail(e.message), identity)
    val local  = LocalMedia(ip"192.0.2.10", port"20000", 1, 1)
    val result = OfferAnswer
      .answer(offer, local, caps)
      .fold(r => fail(r.message), identity)
    assertEquals(result.plan.payloadType, pt(111))
    val text = result.description.render
    assert(text.contains("m=audio 20000 RTP/AVP 111\r\n"), text)
    assert(text.contains("a=rtpmap:111 opus/48000/2"), text)
    assert(
      text.contains("a=fmtp:111 minptime=20;useinbandfec=1;stereo=0"),
      text
    )
    // No Opus in an offer, no answer.
    val pcmuOnly = SessionDescription
      .parse(
        "v=0\r\no=- 1 1 IN IP4 198.51.100.1\r\ns=-\r\nc=IN IP4 198.51.100.1\r\nt=0 0\r\nm=audio 4000 RTP/AVP 0\r\n"
      )
      .fold(e => fail(e.message), identity)
    assertEquals(
      OfferAnswer.answer(pcmuOnly, local, caps).left.toOption,
      Some(Reject.NoCommonCodec)
    )
  }

  // ---- With the library --------------------------------------------------------------

  test("a 20 ms frame becomes one packet, and 20 frames become 20") {
    withCodec() { codec =>
      encode(codec, tone(320 * 20)).map { packets =>
        assertEquals(packets.size, 20)
        assert(
          packets.forall(p => p.size > 0 && p.size < 200),
          packets.map(_.size).toString
        )
      }
    }
  }

  test("what is encoded is decoded: the tone comes back, after Opus's delay") {
    withCodec() { codec =>
      val input = tone(320 * 25)
      for {
        packets <- encode(codec, input)
        pcm     <- Stream
          .emits(packets)
          .through(codec.decoder[IO])
          .compile
          .to(Chunk)
      } yield {
        val out = samples(pcm)
        val in  = samples(input)
        assertEquals(out.size, in.size)
        // The best match over the delay Opus adds (a few ms, at most 40 ms here).
        val skip                          = 4000
        def correlation(lag: Int): Double = {
          val range = skip until (in.size - 700)
          val dot   = range
            .map(i =>
              in(i).toDouble * out(i + lag.min(out.size - 1 - i).max(0))
            )
            .sum
          val norm = math.sqrt(
            range.map(i => in(i).toDouble * in(i)).sum * range
              .map(i => out(i).toDouble * out(i))
              .sum
          )
          dot / norm
        }
        val best = (0 until 640).map(correlation).max
        assert(best > 0.9, s"correlation $best")
      }
    }
  }

  test("packets are not all one length: a tone takes more than silence") {
    withCodec() { codec =>
      for {
        loud  <- encode(codec, tone(320 * 10))
        quiet <- encode(codec, Chunk.array(new Array[Byte](640 * 10)))
      } yield assert(
        loud.map(_.size).sum > quiet.map(_.size).sum * 2,
        s"${loud.map(_.size).sum} against ${quiet.map(_.size).sum}"
      )
    }
  }

  test("a packet that is not Opus gives nothing, and the stream carries on") {
    withCodec() { codec =>
      for {
        packets <- encode(codec, tone(320 * 2))
        pcm     <- Stream
          .emits(
            List(
              Chunk.empty[Byte],
              Chunk.array(new Array[Byte](5000))
            ) ++ packets
          )
          .through(codec.decoder[IO])
          .compile
          .to(Chunk)
      } yield assertEquals(pcm.size, 640 * 2)
    }
  }

  test(
    "many streams come and go without trouble: the native state is freed each time"
  ) {
    withCodec() { codec =>
      (1 to 300).toList.traverse_(_ =>
        for {
          packets <- encode(codec, tone(320))
          _ <- Stream.emits(packets).through(codec.decoder[IO]).compile.drain
        } yield ()
      )
    }
  }

  test("a stream cut short frees its state too") {
    withCodec() { codec =>
      (1 to 200).toList.traverse_(_ =>
        Stream
          .chunk(tone(320 * 50))
          .through(codec.encoder[IO])
          .take(2)
          .compile
          .drain
      )
    }
  }

  test("at 8 kHz it can share a softphone with G.711") {
    withCodec(settings.copy(sampleRate = OpusSampleRate.Hz8000)) { codec =>
      val codecs = AudioCodecs
        .resolve(
          NonEmptyList.of(CodecChoice.Named("opus"), CodecChoice.Pcmu),
          List(codec)
        )
        .fold(e => fail(e.getMessage), identity)
      IO(assertEquals(codecs.pcmRate, Some(8000)))
    }
  }

  // ---- Media over RTP ----------------------------------------------------------------

  private def bound
      : Resource[IO, (DatagramSocket[IO], SocketAddress[IpAddress])] =
    Network[IO]
      .bindDatagramSocket(address = SocketAddress(ip"127.0.0.1", port"0"))
      .evalMap(socket => addressOf(socket.address).map(socket -> _))

  private def addressOf(
      address: GenSocketAddress
  ): IO[SocketAddress[IpAddress]] =
    address match {
      case a: SocketAddress[?] =>
        a.host match {
          case ip: IpAddress => IO.pure(SocketAddress(ip, a.port))
          case _ => IO.raiseError(new RuntimeException("not an IP address"))
        }
      case _ => IO.raiseError(new RuntimeException("not an IP address"))
    }

  test(
    "media in Opus is sent one packet to an RTP packet, 960 ticks apart, on the payload type given"
  ) {
    withCodec() { codec =>
      val codecs = AudioCodecs(NonEmptyList.one(codec), Some(pt(101)))
      (
        bound,
        Resource.eval(bound.use((_, a) => IO.pure(a))),
        Supervisor[IO](await = false)
      ).tupled.use { case ((peer, peerAddress), ours, supervisor) =>
        val plan = MediaPlan(
          peerAddress,
          codec.description,
          pt(111),
          None,
          MediaDirection.SendRecv,
          MediaDirection.SendRecv
        )
        for {
          media <- MediaSession
            .create[IO](ip"127.0.0.1", ours.port, codecs, RtpLatching.Ignore)
          _ <- media.start(plan, supervisor, _ => IO.unit)
          _ <- IO.sleep(500.millis)
          _ <- Stream
            .chunk(tone(320 * 4))
            .through(media.sink)
            .compile
            .drain
            .start
          packets <- peer.reads.take(4).compile.toList.timeout(5.seconds)
          decoded = packets.map(d => RtpCodec.decode(d.bytes).get)
          // And one comes back the other way.
          _ <- peer.write(
            Datagram(
              ours,
              RtpCodec.encode(
                pt(111),
                1,
                0L,
                9L,
                decoded.head._2,
                marker = false
              )
            )
          )
          pcm <- media.source.take(640).compile.to(Chunk).timeout(5.seconds)
          _   <- media.stop()
        } yield {
          assertEquals(decoded.map(_._1.payloadType), List.fill(4)(pt(111)))
          assertEquals(
            decoded.map(_._1.timestamp),
            List(0L, 960L, 1920L, 2880L)
          )
          assert(decoded.forall(_._2.size > 0))
          assertEquals(pcm.size, 640)
        }
      }
    }
  }
}
