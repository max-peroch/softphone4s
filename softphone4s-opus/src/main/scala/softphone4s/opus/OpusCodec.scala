package softphone4s.opus

import cats.effect.{Resource, Sync}
import cats.syntax.all.*
import fs2.{Chunk, Pipe}
import softphone4s.media.AudioCodec
import softphone4s.sdp.Codec

import java.lang.foreign.{Arena, MemorySegment, ValueLayout}

/** Opus (RFC 6716, RFC 7587) as a codec for `SoftphoneStreaming`: give what
  * [[OpusCodec.resource]] makes to its `extraCodecs` and list `opus` in
  * `codecs`.
  *
  * libopus is called through the Foreign Function & Memory API, so the JVM
  * wants `--enable-native-access=ALL-UNNAMED` (without it a warning is
  * printed), and libopus has to be installed (`apt install libopus0`, `brew
  * install opus`) or given in the settings.
  *
  * Opus packets are not all one length, and a packet that cannot be coded or
  * decoded, a corrupt one from the network for instance, gives nothing rather
  * than ending the call.
  */
object OpusCodec {

  /** The name `codecs` calls it by. */
  val Token: String = "opus"

  /** Frames of at most 120 ms, the longest Opus has, at 48 kHz. */
  private val MaxFrameSamples = 5760

  /** The longest packet Opus makes. */
  private val MaxPacketBytes = 4000

  /** How SDP names it: always `opus/48000/2` whatever the sample rate of the
    * audio, and the parameters say that we want mono (RFC 7587 §7).
    */
  def description(settings: OpusSettings): Codec =
    Codec(
      "opus",
      48000,
      2,
      None,
      Some(
        List(
          "minptime=20",
          s"useinbandfec=${settings.inbandFec.flag}",
          "stereo=0",
          "sprop-stereo=0",
          s"maxplaybackrate=${settings.sampleRate.hz}",
          s"sprop-maxcapturerate=${settings.sampleRate.hz}"
        ).mkString(";")
      )
    )

  /** The codec, with libopus loaded for as long as the resource is held. Fails
    * with an [[OpusError]]: the settings are not allowed, or the library cannot
    * be loaded or lacks a function.
    */
  def resource[F[_]: Sync](
      settings: OpusSettings = OpusSettings()
  ): Resource[F, AudioCodec] =
    for {
      valid <- Resource.eval(
        Sync[F].fromEither(OpusSettings.validated(settings))
      )
      arena <- Resource.make(Sync[F].delay(Arena.ofShared()))(a =>
        Sync[F].delay(a.close())
      )
      library <- Resource.eval(
        Sync[F].fromEither(Libopus.load(valid.library, arena))
      )
    } yield new Opus(valid, library)

  private final class Opus(settings: OpusSettings, library: Libopus)
      extends AudioCodec {

    val token: String      = Token
    val description: Codec = OpusCodec.description(settings)
    val pcmRate: Int       = settings.sampleRate.hz

    def encoder[F[_]: Sync]: Pipe[F, Byte, Chunk[Byte]] = {
      val pipe = AudioCodec.encoding[F](pcmPacketBytes, encoding[F])
      in => pipe(in).filter(_.nonEmpty)
    }

    def decoder[F[_]: Sync]: Pipe[F, Chunk[Byte], Byte] =
      AudioCodec.decoding(decoding[F])

    private def encoding[F[_]: Sync]: Resource[F, AudioCodec.Session] =
      Resource
        .make(Sync[F].delay(OpenEncoder.open(library, settings)))(opened =>
          Sync[F].delay(opened.foreach(_.close()))
        )
        .evalMap(opened => Sync[F].fromEither(opened.map(e => e.encode(_))))

    private def decoding[F[_]: Sync]: Resource[F, AudioCodec.Session] =
      Resource
        .make(Sync[F].delay(OpenDecoder.open(library, settings)))(opened =>
          Sync[F].delay(opened.foreach(_.close()))
        )
        .evalMap(opened => Sync[F].fromEither(opened.map(d => d.decode(_))))
  }

  private val NoBytes: Array[Byte] = Array.emptyByteArray

  /** A native encoder and the buffers it is fed through. */
  private final class OpenEncoder private (
      library: Libopus,
      arena: Arena,
      encoder: MemorySegment,
      pcm: MemorySegment,
      packet: MemorySegment
  ) {

    /** The packet for a frame of PCM, or nothing if it cannot be coded. */
    def encode(frame: Array[Byte]): Array[Byte] = {
      val samples = frame.length / 2
      if samples == 0 || samples > MaxFrameSamples then NoBytes
      else {
        MemorySegment.copy(
          frame,
          0,
          pcm,
          ValueLayout.JAVA_BYTE,
          0L,
          samples * 2
        )
        library
          .encode(encoder, pcm, samples, packet, MaxPacketBytes)
          .fold(
            _ => NoBytes,
            n => {
              val out = new Array[Byte](n)
              MemorySegment.copy(packet, ValueLayout.JAVA_BYTE, 0L, out, 0, n)
              out
            }
          )
      }
    }

    def close(): Unit = {
      library.freeEncoder(encoder)
      arena.close()
    }
  }

  private object OpenEncoder {

    def open(
        library: Libopus,
        settings: OpusSettings
    ): Either[OpusError, OpenEncoder] = {
      val arena = Arena.ofShared()
      val made  = for {
        encoder <- library.newEncoder(arena, settings.sampleRate.hz)
        _       <- configure(library, encoder, settings).leftMap { e =>
          library.freeEncoder(encoder)
          e
        }
      } yield new OpenEncoder(
        library,
        arena,
        encoder,
        arena.allocate(MaxFrameSamples * 2L),
        arena.allocate(MaxPacketBytes.toLong)
      )
      made.leftMap { e =>
        arena.close()
        e
      }
    }

    private def configure(
        library: Libopus,
        encoder: MemorySegment,
        settings: OpusSettings
    ): Either[OpusError, Unit] =
      for {
        _ <- settings.bitrate.traverse_(
          library.setEncoder(encoder, Libopus.SetBitrate, _)
        )
        _ <- settings.complexity.traverse_(
          library.setEncoder(encoder, Libopus.SetComplexity, _)
        )
        _ <- library.setEncoder(
          encoder,
          Libopus.SetInbandFec,
          settings.inbandFec.flag
        )
        _ <- library.setEncoder(
          encoder,
          Libopus.SetPacketLossPercent,
          settings.expectedPacketLoss
        )
      } yield ()
  }

  /** A native decoder and the buffers it is fed through. */
  private final class OpenDecoder private (
      library: Libopus,
      arena: Arena,
      decoder: MemorySegment,
      packet: MemorySegment,
      pcm: MemorySegment,
      capacity: Int
  ) {

    /** The PCM for a packet, or nothing if it is not a packet Opus can decode.
      */
    def decode(payload: Array[Byte]): Array[Byte] =
      if payload.isEmpty || payload.length > MaxPacketBytes then NoBytes
      else {
        MemorySegment.copy(
          payload,
          0,
          packet,
          ValueLayout.JAVA_BYTE,
          0L,
          payload.length
        )
        library
          .decode(decoder, packet, payload.length, pcm, capacity)
          .fold(
            _ => NoBytes,
            samples => {
              val out = new Array[Byte](samples * 2)
              MemorySegment.copy(
                pcm,
                ValueLayout.JAVA_BYTE,
                0L,
                out,
                0,
                samples * 2
              )
              out
            }
          )
      }

    def close(): Unit = {
      library.freeDecoder(decoder)
      arena.close()
    }
  }

  private object OpenDecoder {

    def open(
        library: Libopus,
        settings: OpusSettings
    ): Either[OpusError, OpenDecoder] = {
      val arena    = Arena.ofShared()
      val capacity = MaxFrameSamples * settings.sampleRate.hz / 48000
      val made     = library
        .newDecoder(arena, settings.sampleRate.hz)
        .map(decoder =>
          new OpenDecoder(
            library,
            arena,
            decoder,
            arena.allocate(MaxPacketBytes.toLong),
            arena.allocate(capacity * 2L),
            capacity
          )
        )
      made.leftMap { e =>
        arena.close()
        e
      }
    }
  }
}
