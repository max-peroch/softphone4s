package softphone4s.transport

import fs2.{Chunk, Pipe, Pull, Stream}

import java.nio.charset.StandardCharsets.ISO_8859_1
import scala.annotation.tailrec

/** Cuts the bytes of a TCP or TLS connection into SIP messages (RFC 3261
  * §18.3): a message is its headers up to a blank line and then as many body
  * bytes as `Content-Length` says. Blank lines between messages are keep-alives
  * (RFC 5626) and are skipped.
  */
object StreamFraming {

  /** What the front of a buffer holds. */
  private enum Step {
    case NeedMore
    case Skip(rest: Chunk[Byte])
    case Frame(message: Chunk[Byte], rest: Chunk[Byte])
    case Failed(error: FramingError)
  }

  private val Cr = '\r'.toByte
  private val Lf = '\n'.toByte

  /** Every complete message of the stream, or the error that ended it. A stream
    * that ends in the middle of a message yields nothing for it.
    */
  def frames[F[_]](
      limits: FramingLimits = FramingLimits()
  ): Pipe[F, Byte, Either[FramingError, Chunk[Byte]]] = {
    def loop(
        in: Stream[F, Chunk[Byte]],
        buffer: Chunk[Byte]
    ): Pull[F, Either[FramingError, Chunk[Byte]], Unit] = {
      val (messages, rest, failure) = drain(buffer, limits)
      Pull.output(Chunk.from(messages.map(Right(_)))) >> (failure match {
        case Some(error) => Pull.output1(Left(error))
        case None        =>
          in.pull.uncons1.flatMap {
            case None                => Pull.done
            case Some((chunk, tail)) => loop(tail, rest ++ chunk)
          }
      })
    }
    in => loop(in.chunks, Chunk.empty).stream
  }

  /** The complete messages at the front of `buffer`, what is left, and the
    * error that stopped it, if any.
    */
  private def drain(
      buffer: Chunk[Byte],
      limits: FramingLimits
  ): (Vector[Chunk[Byte]], Chunk[Byte], Option[FramingError]) = {
    @tailrec
    def go(
        remaining: Chunk[Byte],
        acc: Vector[Chunk[Byte]]
    ): (Vector[Chunk[Byte]], Chunk[Byte], Option[FramingError]) =
      next(remaining, limits) match {
        case Step.NeedMore             => (acc, remaining, None)
        case Step.Skip(rest)           => go(rest, acc)
        case Step.Frame(message, rest) => go(rest, acc :+ message)
        case Step.Failed(error)        => (acc, remaining, Some(error))
      }
    go(buffer, Vector.empty)
  }

  private def next(buffer: Chunk[Byte], limits: FramingLimits): Step = {
    val bytes = buffer.toArray
    if bytes.isEmpty then Step.NeedMore
    else if bytes(0) == Cr || bytes(0) == Lf then
      // A keep-alive: drop the line ending.
      Step.Skip(
        buffer.drop(if bytes.length > 1 && bytes(0) == Cr && bytes(1) == Lf then
          2
        else 1)
      )
    else
      endOfHeaders(bytes) match {
        case -1 =>
          if bytes.length > limits.maxHeaderBytes then
            Step.Failed(FramingError.HeadersTooLarge(limits.maxHeaderBytes))
          else Step.NeedMore
        case end if end > limits.maxHeaderBytes =>
          Step.Failed(FramingError.HeadersTooLarge(limits.maxHeaderBytes))
        case end =>
          contentLength(new String(bytes, 0, end, ISO_8859_1)) match {
            case Left(error)   => Step.Failed(error)
            case Right(length) =>
              if length > limits.maxBodyBytes then
                Step.Failed(
                  FramingError.BodyTooLarge(length, limits.maxBodyBytes)
                )
              else if bytes.length - end < length then Step.NeedMore
              else {
                val total = end + length.toInt
                Step.Frame(buffer.take(total), buffer.drop(total))
              }
          }
      }
  }

  /** The index just past the first blank line, or -1. */
  private def endOfHeaders(bytes: Array[Byte]): Int = {
    @tailrec
    def find(i: Int): Int =
      if i + 3 >= bytes.length then -1
      else if bytes(i) == Cr && bytes(i + 1) == Lf && bytes(
          i + 2
        ) == Cr && bytes(i + 3) == Lf
      then i + 4
      else find(i + 1)
    find(0)
  }

  /** The `Content-Length` (or its short form `l`) among the header lines. */
  private def contentLength(headers: String): Either[FramingError, Long] =
    headers.linesIterator
      .drop(1)
      .flatMap { line =>
        line.indexOf(':') match {
          case -1    => None
          case colon =>
            val name = line.substring(0, colon).trim
            Option.when(
              name.equalsIgnoreCase("Content-Length") || name.equalsIgnoreCase(
                "l"
              )
            )(line.substring(colon + 1).trim)
        }
      }
      .nextOption()
      .toRight(FramingError.MissingContentLength)
      .flatMap(text =>
        text.toLongOption
          .filter(_ >= 0)
          .toRight(FramingError.BadContentLength(text))
      )
}
