package softphone4s.transport

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.{DatagramSocket, Network}
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import softphone4s.codec.RtpCodec
import softphone4s.SipLiterals.pt
import softphone4s.model.{DtmfDigit, PayloadType, SoftphoneEvent}

import scala.concurrent.duration.*

/** The sending end of a stream, against a socket that plays the receiver. */
class RtpSenderSuite extends CatsEffectSuite {

  private val loopback = ip"127.0.0.1"

  private def bound
      : Resource[IO, (DatagramSocket[IO], SocketAddress[IpAddress])] =
    Network[IO]
      .bindDatagramSocket(address = SocketAddress(loopback, port"0"))
      .evalMap(socket => RawPeer.addressOf(socket.address).map(socket -> _))

  /** A received packet. */
  private final case class Seen(
      header: RtpCodec.RtpHeader,
      payload: Chunk[Byte]
  ) {
    def event: SoftphoneEvent =
      SoftphoneEvent.decode(payload).getOrElse(fail("not an event"))
  }

  private def seen(datagram: fs2.io.net.Datagram): Seen =
    RtpCodec.decode(datagram.bytes).fold(fail("not RTP"))(Seen.apply)

  /** The sender, the receiver's socket, and where the sender is told to send.
    */
  private def fixture(
      eventType: Option[PayloadType] = Some(pt(101)),
      sending: Boolean = true
  ) =
    for {
      (receiver, address) <- bound
      (sender, _)         <- bound
      target              <- Resource.eval(
        Ref.of[IO, Option[SocketAddress[IpAddress]]](
          Option.when(sending)(address)
        )
      )
      rtp <- Resource.eval(
        RtpSender.make[IO](sender, target.get, pt(0), eventType, 160)
      )
    } yield (rtp, receiver, target)

  /** Everything the receiver gets in the next `time`. */
  private def received(receiver: DatagramSocket[IO], time: FiniteDuration) =
    receiver.reads.interruptAfter(time).map(seen).compile.toList

  private val frame = Chunk.array(Array.fill[Byte](160)(1))

  test(
    "a digit is a packet every 20 ms with the time so far, then the last one three times"
  ) {
    fixture().use { (rtp, receiver, _) =>
      for {
        sent    <- rtp.telephoneEvent(DtmfDigit.D5, 100.millis).start
        packets <- received(receiver, 700.millis)
        _       <- sent.joinWithNever
      } yield {
        assertEquals(packets.size, 7) // 4 running, 3 ending
        assertEquals(packets.map(_.header.payloadType).distinct, List(pt(101)))
        assertEquals(packets.map(_.header.timestamp).distinct, List(0L))
        assertEquals(packets.map(_.header.ssrc).distinct.size, 1)
        assertEquals(packets.map(_.header.sequenceNumber), (0 to 6).toList)
        assertEquals(
          packets.map(_.header.marker),
          List(true) ++ List.fill(6)(false)
        )
        assertEquals(packets.map(_.event.event).distinct, List(5))
        assertEquals(
          packets.map(_.event.endBit),
          List.fill(4)(false) ++ List.fill(3)(true)
        )
        assertEquals(
          packets.map(_.event.duration),
          List(160, 320, 480, 640, 800, 800, 800)
        )
        assertEquals(packets.map(_.event.volume).distinct, List(10))
      }
    }
  }

  test(
    "a digit as short as a packet is the last packet three times, the first marked"
  ) {
    fixture().use { (rtp, receiver, _) =>
      for {
        sent    <- rtp.telephoneEvent(DtmfDigit.Star, 20.millis).start
        packets <- received(receiver, 400.millis)
        _       <- sent.joinWithNever
      } yield {
        assertEquals(packets.map(_.event.endBit), List(true, true, true))
        assertEquals(packets.map(_.event.duration), List(160, 160, 160))
        assertEquals(packets.map(_.header.marker), List(true, false, false))
        assertEquals(packets.map(_.event.event).distinct, List(10))
      }
    }
  }

  test(
    "audio is held back while a digit is sent, and carries on in step with the clock"
  ) {
    fixture().use { (rtp, receiver, _) =>
      val audio = Stream
        .awakeEvery[IO](20.millis)
        .map(_ => frame)
        .through(rtp.audio)
        .compile
        .drain
      (audio.background).use { _ =>
        for {
          _       <- IO.sleep(100.millis)
          sent    <- rtp.telephoneEvent(DtmfDigit.D1, 100.millis).start
          packets <- received(receiver, 800.millis)
          _       <- sent.joinWithNever
        } yield {
          val kinds      = packets.map(_.header.payloadType)
          val firstEvent = kinds.indexOf(101)
          val lastEvent  = kinds.lastIndexOf(101)
          assert(firstEvent > 0, "some audio came first")
          assert(lastEvent < kinds.size - 1, "and some after")
          assert(
            kinds.slice(firstEvent, lastEvent + 1).forall(_ == pt(101)),
            s"no audio between the first and last packet of the digit: $kinds"
          )
          // One run of sequence numbers for both.
          assertEquals(
            packets.map(_.header.sequenceNumber),
            packets.indices.toList.map(_ + packets.head.header.sequenceNumber)
          )
          // The digit is stamped with the time it began, the audio after it with
          // the time it is, a whole number of frames on.
          val stamp = packets(firstEvent).header.timestamp
          assertEquals(
            packets
              .slice(firstEvent, lastEvent + 1)
              .map(_.header.timestamp)
              .distinct,
            List(stamp)
          )
          val after = packets(lastEvent + 1).header
          assert(
            after.timestamp >= stamp + 160 * 5,
            s"${after.timestamp} after $stamp"
          )
          assertEquals(after.timestamp % 160, 0L)
          assert(
            after.marker,
            "audio that follows a digit begins with the marker"
          )
        }
      }
    }
  }

  test("the first audio, and the first after a gap, carry the marker") {
    fixture().use { (rtp, receiver, _) =>
      for {
        sent <- Stream
          .emits(List(frame, frame, Chunk.empty[Byte], frame))
          .through(rtp.audio)
          .compile
          .drain
          .start
        packets <- received(receiver, 300.millis)
        _       <- sent.joinWithNever
      } yield {
        assertEquals(packets.map(_.header.marker), List(true, false, true))
        // The empty frame still took its 20 ms.
        assertEquals(packets.map(_.header.timestamp), List(0L, 160L, 480L))
        assertEquals(packets.map(_.header.sequenceNumber), List(0, 1, 2))
      }
    }
  }

  test("a digit that was not agreed to is not sent") {
    fixture(eventType = None).use { (rtp, receiver, _) =>
      for {
        sent    <- rtp.telephoneEvent(DtmfDigit.D5, 100.millis)
        packets <- received(receiver, 200.millis)
      } yield {
        assert(!sent)
        assertEquals(packets, Nil)
      }
    }
  }

  test("nothing is sent while there is no one to send to, as when on hold") {
    fixture(sending = false).use { (rtp, receiver, _) =>
      for {
        sent    <- rtp.telephoneEvent(DtmfDigit.D5, 100.millis)
        packets <- received(receiver, 200.millis)
      } yield {
        assert(!sent)
        assertEquals(packets, Nil)
      }
    }
  }

  test("two digits at once are sent one after the other") {
    fixture().use { (rtp, receiver, _) =>
      for {
        both <- List(DtmfDigit.D1, DtmfDigit.D2)
          .parTraverse(rtp.telephoneEvent(_, 40.millis))
          .start
        packets <- received(receiver, 800.millis)
        results <- both.joinWithNever
      } yield {
        assertEquals(results, List(true, true))
        val digits = packets.map(_.event.event)
        // All of one digit and then all of the other: they change once.
        assertEquals(digits.distinct.toSet, Set(1, 2))
        assertEquals(digits.sliding(2).count(p => p.head != p.last), 1)
        assertEquals(
          packets.map(_.header.sequenceNumber),
          packets.indices.toList
        )
      }
    }
  }
}
