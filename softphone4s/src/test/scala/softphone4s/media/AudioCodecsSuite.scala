package softphone4s.media

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.config.CodecChoice
import softphone4s.model.SoftphoneError
import softphone4s.sdp.Codec

class AudioCodecsSuite extends CatsEffectSuite {

  private def resolved(
      choices: CodecChoice*
  ): AudioCodecs = resolvedWith(Nil, choices*)

  private def resolvedWith(extras: List[AudioCodec], choices: CodecChoice*) =
    AudioCodecs
      .resolve(NonEmptyList.fromListUnsafe(choices.toList), extras)
      .fold(error => fail(error.getMessage), identity)

  private def pcm(samples: Int*): Chunk[Byte] =
    Chunk.array(
      samples
        .flatMap(s => List((s & 0xff).toByte, ((s >> 8) & 0xff).toByte))
        .toArray
    )

  test("the codecs chosen are used in the order given, once each") {
    val codecs = resolved(CodecChoice.Pcma, CodecChoice.Pcmu, CodecChoice.Pcma)
    assertEquals(
      codecs.capabilities.codecs.toList,
      List(Codec.Pcma, Codec.Pcmu)
    )
    assertEquals(codecs.telephoneEventType, Some(101))
  }

  test("a negotiated codec is found, and one we do not have is not") {
    assertEquals(
      AudioCodecs.Default.implementing(Codec.Pcma).map(_.description),
      Some(Codec.Pcma)
    )
    assertEquals(
      AudioCodecs.Default.implementing(Codec("G729", 8000, 1, Some(18), None)),
      None
    )
  }

  test("the codecs agree on the sample rate of the audio device") {
    assertEquals(AudioCodecs.Default.pcmRate, Some(8000))
  }

  test("a packet is 20 ms: 320 bytes of PCM and 160 of payload for G.711") {
    List(G711.Pcmu, G711.Pcma).foreach { codec =>
      assertEquals(codec.pcmPacketBytes, 320)
      assertEquals(codec.payloadPacketBytes, 160)
      assertEquals(codec.timestampStep, 160)
    }
  }

  test(
    "the encoder makes a packet from a packet of PCM, and the decoder undoes it"
  ) {
    List(G711.Pcmu, G711.Pcma).traverse_ { codec =>
      val samples = (0 until 160).map(i => ((i - 80) * 300))
      Stream
        .chunk(pcm(samples*))
        .through(codec.encoder[IO])
        .through(codec.decoder[IO])
        .compile
        .to(Chunk)
        .map { out =>
          assertEquals(out.size, 320)
          val back = out.toArray
            .grouped(2)
            .map(b => ((b(1) << 8) | (b(0) & 0xff)).toShort.toInt)
            .toList
          back.zip(samples).foreach { (b, s) =>
            assert(
              math.abs(b - s) <= math.max(64, math.abs(s) / 16 + 1),
              s"${codec.description.encoding}: $s -> $b"
            )
          }
        }
    }
  }

  test(
    "G.722 works at 16 kHz: 640 bytes of PCM, 160 of payload, a timestamp step of 160"
  ) {
    val codec = G722.Wideband
    assertEquals(codec.pcmRate, 16000)
    assertEquals(codec.pcmPacketBytes, 640)
    assertEquals(codec.payloadPacketBytes, 160)
    assertEquals(codec.timestampStep, 160)
    assertEquals(codec.description, Codec.G722)
  }

  test("G.722 cannot share an audio format with G.711") {
    val mixed =
      resolved(CodecChoice.G722, CodecChoice.Pcmu)
    assertEquals(mixed.pcmRate, None)
    assertEquals(
      resolved(CodecChoice.G722).pcmRate,
      Some(16000)
    )
  }

  test(
    "the G.722 pipes keep their state from one packet to the next, and one stream's from another's"
  ) {
    val pcm = Chunk.array(
      Array.tabulate[Byte](640 * 4)(i => ((i * 37) % 251 - 125).toByte)
    )
    def run(in: Chunk[Byte]) =
      Stream
        .chunk(in)
        .through(G722.Wideband.encoder[IO])
        .compile
        .to(Chunk)
    for {
      whole <- run(pcm)
      again <- run(pcm)
      first <- run(pcm.take(640))
    } yield {
      assertEquals(whole.size, 640)
      assertEquals(again, whole)
      assertEquals(first, whole.take(160))
    }
  }

  test("less than a packet of PCM makes nothing yet") {
    Stream
      .chunk(pcm(1, 2, 3))
      .through(G711.Pcma.encoder[IO])
      .compile
      .to(Chunk)
      .map(out => assertEquals(out.size, 0))
  }

  // ---- Codecs the program adds ------------------------------------------------------

  test(
    "a codec the program adds is found by its name, among ours, in the order asked"
  ) {
    val codecs = resolvedWith(
      List(HalfByteCodec),
      CodecChoice.Named("x-half"),
      CodecChoice.Pcmu
    )
    assertEquals(codecs.codecs.map(_.token).toList, List("x-half", "pcmu"))
    assertEquals(
      codecs.implementing(HalfByteCodec.description),
      Some(HalfByteCodec)
    )
  }

  test("its payload type is dynamic, and skips the one telephone events have") {
    val caps =
      resolvedWith(
        List(HalfByteCodec),
        CodecChoice.Named("x-half"),
        CodecChoice.Pcmu
      ).capabilities
    assertEquals(caps.offered.map(_._1), List(96, 0))
  }

  test("a name nothing has is an error") {
    assertEquals(
      AudioCodecs
        .resolve(NonEmptyList.one(CodecChoice.Named("opus")), Nil)
        .left
        .toOption,
      Some(SoftphoneError.UnknownCodec("opus"))
    )
  }

  test("a codec called what another is called is an error") {
    val twin = new AudioCodec {
      val token: String           = "pcmu"
      val description: Codec      = Codec("PCMU2", 8000, 1, None, None)
      val pcmRate: Int            = 8000
      val payloadPacketBytes: Int = 160
      def encoder[F[_]]           = G711.Pcmu.encoder
      def decoder[F[_]]           = G711.Pcmu.decoder
    }
    assertEquals(
      AudioCodecs
        .resolve(NonEmptyList.one(CodecChoice.Pcmu), List(twin))
        .left
        .toOption,
      Some(SoftphoneError.DuplicateCodec("pcmu"))
    )
  }

  test("the added codec's pipes work like any other") {
    val pcm = Chunk.array(Array.tabulate[Byte](640)(i => (i % 100).toByte))
    Stream
      .chunk(pcm)
      .through(HalfByteCodec.encoder[IO])
      .compile
      .to(Chunk)
      .map(payload => assertEquals(payload.size, 320))
  }
}
