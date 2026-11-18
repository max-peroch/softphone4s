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

  /** What the front of the unread bytes holds. Positions are in the array being
    * read.
    */
  private enum Step {

    /** Not enough yet. `need` bytes in all, counted from the front, must have
      * arrived before it is worth looking again: the end of the message when
      * its length is known, else one more than there are. `scanned` of them
      * have been searched for the end of the headers, so the search goes on
      * from there.
      */
    case NeedMore(need: Int, scanned: Int)
    case Skip(to: Int)
    case Ping(to: Int)
    case Message(from: Int, to: Int)
    case Failed(error: FramingError)
  }

  /** What is left after the messages at the front of the buffer are taken, with
    * what the next look needs to know.
    */
  private final case class Drained(
      frames: Vector[Frame],
      rest: Chunk[Byte],
      failure: Option[FramingError],
      need: Int,
      scanned: Int
  )

  private val Cr = '\r'.toByte
  private val Lf = '\n'.toByte

  /** The double CRLF of a keep-alive ping. */
  private val PingBytes: Array[Byte] = Array(Cr, Lf, Cr, Lf)

  /** Whether the `count` bytes at `at` are the first of a ping. */
  private def startsPing(bytes: Array[Byte], at: Int, count: Int): Boolean = {
    @tailrec
    def same(i: Int): Boolean =
      i >= count || (bytes(at + i) == PingBytes(i) && same(i + 1))
    same(0)
  }

  /** A ping, as the event that carries it: there can be millions. */
  private val PingEvent: Either[FramingError, Frame] = Right(Frame.Ping)

  /** Every complete message of the stream, or the error that ended it. A stream
    * that ends in the middle of a message yields nothing for it. Keep-alive
    * pings and pongs are skipped; see [[events]] for those.
    */
  def frames[F[_]](
      limits: FramingLimits = FramingLimits()
  ): Pipe[F, Byte, Either[FramingError, Chunk[Byte]]] =
    _.through(events[F](limits)).collect {
      case Right(Frame.Message(bytes)) => Right(bytes)
      case Left(error)                 => Left(error)
    }

  /** As [[frames]], and the pings as well (a lone CRLF, a pong, is skipped).
    *
    * The time taken is in proportion to the bytes that come, however they are
    * cut up: the buffer is copied once for each read that completes something,
    * not for each message or ping in it, and a message that is still arriving
    * is looked at again only when all of it can be there (or, in its headers,
    * from where the search left off).
    */
  def events[F[_]](
      limits: FramingLimits = FramingLimits()
  ): Pipe[F, Byte, Either[FramingError, Frame]] = {
    def loop(
        in: Stream[F, Chunk[Byte]],
        buffer: Chunk[Byte],
        need: Int,
        scanned: Int
    ): Pull[F, Either[FramingError, Frame], Unit] =
      in.pull.uncons1.flatMap {
        case None                => Pull.done
        case Some((chunk, tail)) =>
          val combined = buffer ++ chunk
          if combined.size < need then loop(tail, combined, need, scanned)
          else {
            val drained = drain(combined, limits, scanned)
            Pull.output(Chunk.from(drained.frames.map {
              case Frame.Ping => PingEvent
              case message    => Right(message)
            })) >>
              (drained.failure match {
                case Some(error) => Pull.output1(Left(error))
                case None        =>
                  loop(tail, drained.rest, drained.need, drained.scanned)
              })
          }
      }
    in => loop(in.chunks, Chunk.empty, 1, 0).stream
  }

  /** The complete messages at the front of `buffer`, what is left, and the
    * error that stopped it, if any. `scanned` is how far the front message's
    * headers have been searched already.
    */
  private def drain(
      buffer: Chunk[Byte],
      limits: FramingLimits,
      scanned: Int
  ): Drained = {
    val bytes                      = buffer.toArray
    def rest(at: Int): Chunk[Byte] = Chunk.array(bytes, at, bytes.length - at)

    @tailrec
    def go(at: Int, acc: Vector[Frame], scanned: Int): Drained =
      next(bytes, at, scanned, limits) match {
        case Step.NeedMore(need, seen) =>
          Drained(acc, rest(at), None, need, seen)
        case Step.Skip(to)          => go(to, acc, 0)
        case Step.Ping(to)          => go(to, acc :+ Frame.Ping, 0)
        case Step.Message(from, to) =>
          go(to, acc :+ Frame.Message(Chunk.array(bytes, from, to - from)), 0)
        case Step.Failed(error) => Drained(acc, rest(at), Some(error), 0, 0)
      }
    go(0, Vector.empty, scanned)
  }

  private def next(
      bytes: Array[Byte],
      at: Int,
      scanned: Int,
      limits: FramingLimits
  ): Step = {
    val available = bytes.length - at
    if available == 0 then Step.NeedMore(1, 0)
    else if bytes(at) == Cr || bytes(at) == Lf then blankLines(bytes, at)
    else
      endOfHeaders(bytes, at, scanned) match {
        case -1 =>
          if limits.maxHeaderBytes.isExceededBy(available) then
            Step.Failed(
              FramingError.HeadersTooLarge(limits.maxHeaderBytes.value)
            )
          else Step.NeedMore(available + 1, available)
        case end if limits.maxHeaderBytes.isExceededBy(end - at) =>
          Step.Failed(
            FramingError.HeadersTooLarge(limits.maxHeaderBytes.value)
          )
        case end =>
          val headerLength = end - at
          contentLength(new String(bytes, at, headerLength, ISO_8859_1)) match {
            case Left(error)   => Step.Failed(error)
            case Right(length) =>
              if length > limits.maxBodyBytes.value then
                Step.Failed(
                  FramingError.BodyTooLarge(length, limits.maxBodyBytes.value)
                )
              else if available - headerLength < length then
                // The headers are looked at again, from the start, once the body is there.
                Step.NeedMore(headerLength + length.toInt, 0)
              else Step.Message(at, end + length.toInt)
          }
      }
  }

  /** Blank lines between messages (RFC 5626 §4.4.1): a double CRLF is a ping, a
    * single CRLF a pong or nothing at all. What looks like the start of a ping
    * but is not yet four bytes long has to wait for the rest.
    */
  private def blankLines(bytes: Array[Byte], at: Int): Step = {
    val available = bytes.length - at
    if available >= 4 && startsPing(bytes, at, 4) then Step.Ping(at + 4)
    else if available < 4 && startsPing(bytes, at, available) then
      Step.NeedMore(4, 0)
    else
      Step.Skip(
        at + (if available > 1 && bytes(at) == Cr && bytes(at + 1) == Lf then 2
              else 1)
      )
  }

  /** The position just past the first blank line of the message at `at`, or -1.
    * `scanned` bytes of it are known to hold none, and a blank line can begin
    * up to three bytes before them.
    */
  private def endOfHeaders(bytes: Array[Byte], at: Int, scanned: Int): Int = {
    @tailrec
    def find(i: Int): Int =
      if i + 3 >= bytes.length then -1
      else if bytes(i) == Cr && bytes(i + 1) == Lf && bytes(i + 2) == Cr &&
        bytes(i + 3) == Lf
      then i + 4
      else find(i + 1)
    find(at + math.max(0, scanned - 3))
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
