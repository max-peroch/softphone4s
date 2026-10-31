package softphone4s.opus

import cats.syntax.all.*

import java.lang.foreign.{
  Arena,
  FunctionDescriptor,
  Linker,
  MemorySegment,
  SymbolLookup,
  ValueLayout
}
import java.lang.invoke.MethodHandle
import java.nio.file.Path
import scala.jdk.OptionConverters.*

/** libopus, called through the Foreign Function & Memory API (final since JDK
  * 22): no JNI glue to build, no extra dependency, and the library is the one
  * installed on the system, or the one given. Only the calls a phone needs are
  * bound: a mono encoder and a mono decoder.
  *
  * The native pointers are `MemorySegment`s; the buffers they are used with are
  * made by the caller. Nothing here is thread-confined, but an encoder or a
  * decoder must not be used by two threads at once.
  */
private[opus] final class Libopus private (
    createEncoder: MethodHandle,
    encoderCtl: MethodHandle,
    encodeFrame: MethodHandle,
    destroyEncoder: MethodHandle,
    createDecoder: MethodHandle,
    decodeFrame: MethodHandle,
    destroyDecoder: MethodHandle
) {
  import Libopus.*

  private def invoke(
      function: String,
      handle: MethodHandle,
      args: Object*
  ): Either[OpusError, Any] =
    Either
      .catchNonFatal(handle.invokeWithArguments(args*))
      .leftMap(e => OpusError.LibraryUnusable(s"$function: ${e.getMessage}"))

  private def intResult(function: String, result: Any): Either[OpusError, Int] =
    result match {
      case i: java.lang.Integer => Right(i.intValue)
      case other                =>
        Left(OpusError.LibraryUnusable(s"$function returned $other"))
    }

  /** A mono encoder at `sampleRate` for speech. */
  def newEncoder(
      scratch: Arena,
      sampleRate: Int
  ): Either[OpusError, MemorySegment] = {
    val error = scratch.allocate(ValueLayout.JAVA_INT)
    for {
      result <- invoke(
        "opus_encoder_create",
        createEncoder,
        Int.box(sampleRate),
        Int.box(1),
        Int.box(ApplicationVoip),
        error
      )
      pointer <- result match {
        case p: MemorySegment => Right(p)
        case other            =>
          Left(
            OpusError.LibraryUnusable(s"opus_encoder_create returned $other")
          )
      }
      code = error.get(ValueLayout.JAVA_INT, 0)
      _ <- Either.cond(
        code == 0 && pointer.address() != 0L,
        (),
        OpusError.Native("opus_encoder_create", code)
      )
    } yield pointer
  }

  def setEncoder(
      encoder: MemorySegment,
      request: Int,
      value: Int
  ): Either[OpusError, Unit] =
    for {
      result <- invoke(
        "opus_encoder_ctl",
        encoderCtl,
        encoder,
        Int.box(request),
        Int.box(value)
      )
      code <- intResult("opus_encoder_ctl", result)
      _    <- Either.cond(
        code == 0,
        (),
        OpusError.Native("opus_encoder_ctl", code)
      )
    } yield ()

  /** Encodes `frames` samples at `pcm` into `out`, which holds `capacity`
    * bytes: the length of the packet, or a negative error code.
    */
  def encode(
      encoder: MemorySegment,
      pcm: MemorySegment,
      frames: Int,
      out: MemorySegment,
      capacity: Int
  ): Either[OpusError, Int] =
    invoke(
      "opus_encode",
      encodeFrame,
      encoder,
      pcm,
      Int.box(frames),
      out,
      Int.box(capacity)
    )
      .flatMap(intResult("opus_encode", _))
      .flatMap(n => Either.cond(n >= 0, n, OpusError.Native("opus_encode", n)))

  def freeEncoder(encoder: MemorySegment): Unit =
    invoke("opus_encoder_destroy", destroyEncoder, encoder).fold(
      _ => (),
      _ => ()
    )

  /** A mono decoder at `sampleRate`. */
  def newDecoder(
      scratch: Arena,
      sampleRate: Int
  ): Either[OpusError, MemorySegment] = {
    val error = scratch.allocate(ValueLayout.JAVA_INT)
    for {
      result <- invoke(
        "opus_decoder_create",
        createDecoder,
        Int.box(sampleRate),
        Int.box(1),
        error
      )
      pointer <- result match {
        case p: MemorySegment => Right(p)
        case other            =>
          Left(
            OpusError.LibraryUnusable(s"opus_decoder_create returned $other")
          )
      }
      code = error.get(ValueLayout.JAVA_INT, 0)
      _ <- Either.cond(
        code == 0 && pointer.address() != 0L,
        (),
        OpusError.Native("opus_decoder_create", code)
      )
    } yield pointer
  }

  /** Decodes the `length` bytes at `data` into `pcm`, which holds `capacity`
    * samples: the number of samples made.
    */
  def decode(
      decoder: MemorySegment,
      data: MemorySegment,
      length: Int,
      pcm: MemorySegment,
      capacity: Int
  ): Either[OpusError, Int] =
    invoke(
      "opus_decode",
      decodeFrame,
      decoder,
      data,
      Int.box(length),
      pcm,
      Int.box(capacity),
      Int.box(0)
    )
      .flatMap(intResult("opus_decode", _))
      .flatMap(n => Either.cond(n >= 0, n, OpusError.Native("opus_decode", n)))

  def freeDecoder(decoder: MemorySegment): Unit =
    invoke("opus_decoder_destroy", destroyDecoder, decoder).fold(
      _ => (),
      _ => ()
    )
}

private[opus] object Libopus {

  // The values libopus defines in opus_defines.h.
  val ApplicationVoip: Int      = 2048
  val SetBitrate: Int           = 4002
  val SetComplexity: Int        = 4010
  val SetInbandFec: Int         = 4012
  val SetPacketLossPercent: Int = 4014

  /** The names a library is called by, tried in turn when no path is given. */
  private val Names: List[String] = List(
    "libopus.so.0",
    "libopus.so",
    "libopus.0.dylib",
    "libopus.dylib",
    "/opt/homebrew/lib/libopus.dylib",
    "/usr/local/lib/libopus.dylib",
    "opus.dll",
    "libopus-0.dll"
  )

  /** Loads libopus from `path`, or from where it is usually found; the library
    * stays loaded as long as `arena` is open.
    */
  def load(
      path: Option[Path],
      arena: Arena
  ): Either[OpusError, Libopus] = {
    val candidates = path.fold(Names)(p => List(p.toString))
    val loaded     = candidates.collectFirstSome(name =>
      Either
        .catchNonFatal(SymbolLookup.libraryLookup(name, arena))
        .toOption
    )
    loaded
      .toRight(OpusError.LibraryNotFound(candidates))
      .flatMap(bind)
  }

  private def bind(lookup: SymbolLookup): Either[OpusError, Libopus] = {
    val linker = Linker.nativeLinker()
    import ValueLayout.{ADDRESS, JAVA_INT}

    def handle(
        name: String,
        descriptor: FunctionDescriptor,
        options: Linker.Option*
    ): Either[OpusError, MethodHandle] =
      for {
        symbol <- lookup
          .find(name)
          .toScala
          .toRight(OpusError.LibraryUnusable(s"it has no $name"))
        bound <- Either
          .catchNonFatal(linker.downcallHandle(symbol, descriptor, options*))
          .leftMap(e => OpusError.LibraryUnusable(s"$name: ${e.getMessage}"))
      } yield bound

    (
      handle(
        "opus_encoder_create",
        FunctionDescriptor.of(ADDRESS, JAVA_INT, JAVA_INT, JAVA_INT, ADDRESS)
      ),
      handle(
        "opus_encoder_ctl",
        FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_INT, JAVA_INT),
        Linker.Option.firstVariadicArg(2)
      ),
      handle(
        "opus_encode",
        FunctionDescriptor
          .of(JAVA_INT, ADDRESS, ADDRESS, JAVA_INT, ADDRESS, JAVA_INT)
      ),
      handle("opus_encoder_destroy", FunctionDescriptor.ofVoid(ADDRESS)),
      handle(
        "opus_decoder_create",
        FunctionDescriptor.of(ADDRESS, JAVA_INT, JAVA_INT, ADDRESS)
      ),
      handle(
        "opus_decode",
        FunctionDescriptor
          .of(JAVA_INT, ADDRESS, ADDRESS, JAVA_INT, ADDRESS, JAVA_INT, JAVA_INT)
      ),
      handle("opus_decoder_destroy", FunctionDescriptor.ofVoid(ADDRESS))
    ).mapN(new Libopus(_, _, _, _, _, _, _))
  }
}
