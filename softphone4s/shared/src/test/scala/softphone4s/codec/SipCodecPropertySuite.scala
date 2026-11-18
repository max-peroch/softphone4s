package softphone4s.codec

import fs2.Chunk
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll
import softphone4s.model.*

class SipCodecPropertySuite extends ScalaCheckSuite {

  property("decode(encode(m)) == m for every valid message") {
    forAll(SipGen.messageGen) { (msg: SipMessage) =>
      assertEquals(SipCodec.decode(SipCodec.encode(msg)), Right(msg))
    }
  }

  property(
    "a stream of encoded messages is framed back into the same messages"
  ) {
    forAll(SipGen.messageGen, SipGen.messageGen) {
      (a: SipMessage, b: SipMessage) =>
        val first  = SipCodec.encode(a)
        val second = SipCodec.encode(b)
        val stream = first ++ second
        SipParser.frame(stream) match {
          case Framing.Frame(len) =>
            assertEquals(len, first.size)
            assertEquals(SipParser.parse(stream.take(len)), Right(a))
            assertEquals(
              SipParser.frame(stream.drop(len)),
              Framing.Frame(second.size)
            )
          case other => fail(s"unexpected: $other")
        }
    }
  }

  property("every strict prefix of an encoded message needs more bytes") {
    forAll(SipGen.messageGen) { (msg: SipMessage) =>
      val bytes = SipCodec.encode(msg)
      val step  = math.max(1, bytes.size / 25)
      (1 until bytes.size by step).foreach { n =>
        assertEquals(SipParser.frame(bytes.take(n)), Framing.NeedMore)
      }
    }
  }

  property("flipping any one byte of a message never throws") {
    forAll(
      SipGen.messageGen,
      org.scalacheck.Gen.choose(0, Int.MaxValue),
      org.scalacheck.Gen.choose(0, 255)
    ) { (msg: SipMessage, at: Int, value: Int) =>
      val bytes = SipCodec.encode(msg).toArray
      bytes(at % bytes.length) = value.toByte
      SipParser.parse(Chunk.array(bytes)) match {
        case Left(_)  => ()
        case Right(_) => ()
      }
    }
  }
}
