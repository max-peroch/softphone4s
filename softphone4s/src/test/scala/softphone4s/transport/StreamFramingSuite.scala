package softphone4s.transport

import cats.Id
import fs2.{Chunk, Stream}
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import softphone4s.codec.{SipCodec, SipGen}
import softphone4s.model.SipMessage

import java.nio.charset.StandardCharsets.ISO_8859_1

class StreamFramingSuite extends ScalaCheckSuite {

  private def bytes(s: String): Chunk[Byte] =
    Chunk.array(s.getBytes(ISO_8859_1))

  private def frame(
      pieces: List[Chunk[Byte]],
      limits: FramingLimits = FramingLimits()
  ): List[Either[FramingError, String]] =
    Stream
      .emits(pieces)
      .flatMap(Stream.chunk)
      .through(StreamFraming.frames[Id](limits))
      .compile
      .toList
      .map(_.map(c => new String(c.toArray, ISO_8859_1)))

  private def message(body: String, header: String = "Content-Length"): String =
    s"OPTIONS sip:a@b SIP/2.0\r\nVia: SIP/2.0/TCP h;branch=z9hG4bK1\r\n$header: ${body.length}\r\n\r\n$body"

  test("one message with a body is one frame") {
    val m = message("hello")
    assertEquals(frame(List(bytes(m))), List(Right(m)))
  }

  test("two messages in one chunk are two frames") {
    val a = message("one")
    val b = message("")
    assertEquals(frame(List(bytes(a + b))), List(Right(a), Right(b)))
  }

  test("a message in pieces is still one frame, whatever the cut") {
    val m = message("some body text")
    (0 to m.length).foreach { cut =>
      assertEquals(
        frame(List(bytes(m.take(cut)), bytes(m.drop(cut)))),
        List(Right(m)),
        s"cut at $cut"
      )
    }
  }

  test("one byte at a time gives the same frames") {
    val a = message("alpha")
    val b = message("be")
    assertEquals(
      frame((a + b).map(c => Chunk.singleton(c.toByte)).toList),
      List(Right(a), Right(b))
    )
  }

  test("blank lines between messages are keep-alives and skipped") {
    val a = message("x")
    assertEquals(
      frame(List(bytes("\r\n\r\n" + a + "\r\n\r\n\r\n" + a))),
      List(Right(a), Right(a))
    )
  }

  private def events(
      pieces: List[Chunk[Byte]]
  ): List[Either[FramingError, Frame]] =
    Stream
      .emits(pieces)
      .flatMap(Stream.chunk)
      .through(StreamFraming.events[Id]())
      .compile
      .toList

  test("a double CRLF is a ping, wherever it falls between messages") {
    val a = message("x")
    assertEquals(
      events(List(bytes("\r\n\r\n" + a + "\r\n\r\n"))).map(_.map {
        case Frame.Ping       => "ping"
        case Frame.Message(_) => "message"
      }),
      List(Right("ping"), Right("message"), Right("ping"))
    )
  }

  test("a ping cut in pieces is still one ping") {
    assertEquals(
      events(List(bytes("\r"), bytes("\n\r"), bytes("\n"))),
      List(Right(Frame.Ping))
    )
  }

  test("a single CRLF, a pong, is not a ping") {
    assertEquals(events(List(bytes("\r\n"))), Nil)
  }

  test("the short form of Content-Length is understood") {
    val m = message("short", header = "l")
    assertEquals(frame(List(bytes(m))), List(Right(m)))
  }

  test("a stream that ends mid-message yields nothing for it") {
    val a = message("done")
    val b = message("cut short")
    assertEquals(frame(List(bytes(a + b.dropRight(3)))), List(Right(a)))
  }

  test("a message with no Content-Length ends the stream") {
    val m = "OPTIONS sip:a@b SIP/2.0\r\nVia: SIP/2.0/TCP h\r\n\r\n"
    assertEquals(
      frame(List(bytes(m))),
      List(Left(FramingError.MissingContentLength))
    )
  }

  test("a Content-Length that is not a number ends the stream") {
    val m = "OPTIONS sip:a@b SIP/2.0\r\nContent-Length: abc\r\n\r\n"
    assertEquals(
      frame(List(bytes(m))),
      List(Left(FramingError.BadContentLength("abc")))
    )
  }

  test("headers that never end are refused once past the limit") {
    val limits = FramingLimits(maxHeaderBytes = 64)
    val m      = "OPTIONS sip:a@b SIP/2.0\r\n" + ("X: " + "a" * 100 + "\r\n")
    assertEquals(
      frame(List(bytes(m)), limits),
      List(Left(FramingError.HeadersTooLarge(64)))
    )
  }

  test("a body past the limit is refused before it is read") {
    val limits = FramingLimits(maxBodyBytes = 10)
    val m      = "OPTIONS sip:a@b SIP/2.0\r\nContent-Length: 11\r\n\r\n"
    assertEquals(
      frame(List(bytes(m)), limits),
      List(Left(FramingError.BodyTooLarge(11, 10)))
    )
  }

  test("nothing after an error is delivered") {
    val bad = "OPTIONS sip:a@b SIP/2.0\r\nContent-Length: x\r\n\r\n"
    assertEquals(frame(List(bytes(bad + message("ok")))).size, 1)
  }

  property(
    "encoded messages cut anywhere come back whole and decode to the original"
  ) {
    val gen = for {
      messages <- Gen.nonEmptyListOf(SipGen.messageGen)
      sizes    <- Gen.listOfN(40, Gen.choose(1, 300))
    } yield (messages, sizes)
    forAll(gen) { (messages: List[SipMessage], sizes: List[Int]) =>
      val encoded = messages.map(SipCodec.encode)
      val all     = encoded.foldLeft(Chunk.empty[Byte])(_ ++ _).toArray
      val pieces  = sizes.foldLeft((List.empty[Chunk[Byte]], 0)) {
        case ((acc, at), size) =>
          (acc :+ Chunk.array(all.slice(at, at + size)), at + size)
      } match {
        case (acc, at) => acc :+ Chunk.array(all.drop(at))
      }
      val framed = Stream
        .emits(pieces)
        .flatMap(Stream.chunk)
        .through(StreamFraming.frames[Id]())
        .compile
        .toList
      val decoded = framed.map(
        _.flatMap(c =>
          SipCodec
            .decode(c)
            .left
            .map(e => FramingError.BadContentLength(e.message))
        )
      )
      assertEquals(decoded, messages.map(Right(_)))
    }
  }
}
