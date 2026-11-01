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

  test("nothing has been sent, so there is nothing to say as a sender") {
    fixture().use((rtp, _, _) => rtp.senderInfo.map(assertEquals(_, None)))
  }

  test("frames with nowhere to go are not counted as sent") {
    fixture(sending = false).use { (rtp, _, _) =>
      for {
        _ <- Stream.emits(List(frame, frame)).through(rtp.audio).compile.drain
        info <- rtp.senderInfo
      } yield assertEquals(info, None)
    }
  }

  test(
    "the packets and the bytes of payload sent are counted, digits included"
  ) {
    fixture().use { (rtp, receiver, _) =>
      for {
        _ <- Stream
          .emits(List(frame, frame, frame))
          .through(rtp.audio)
          .compile
          .drain
        _    <- rtp.telephoneEvent(DtmfDigit.D1, 100.millis)
        info <- rtp.senderInfo
        got  <- received(receiver, 200.millis)
      } yield {
        // Three of 160 bytes, and seven events of four.
        val sent = info.getOrElse(fail("no sender info"))
        assertEquals((sent.packets, sent.octets), (10L, 3L * 160 + 7 * 4))
        assertEquals(got.size, 10)
      }
    }
  }

  test("the stream's clock moves on with the wall clock between frames") {
    fixture().use { (rtp, _, _) =>
      for {
        _ <- Stream.emits(List(frame, frame)).through(rtp.audio).compile.drain
        first <- rtp.senderInfo
        _     <- IO.sleep(250.millis)
        later <- rtp.senderInfo
      } yield {
        val a = first.getOrElse(fail("no sender info"))
        val b = later.getOrElse(fail("no sender info"))
        // 250 ms at 8 kHz is 2 000 ticks.
        val moved = b.rtpTimestamp - a.rtpTimestamp
        assert(moved >= 2000 && moved < 2400, moved)
        assert(b.ntp > a.ntp)
        // The last frame was the second, at 160.
        assert(a.rtpTimestamp >= 160 && a.rtpTimestamp < 400, a.rtpTimestamp)
      }
    }
  }

  test("the sender report names the time of the wall clock") {
    fixture().use { (rtp, _, _) =>
      for {
        _    <- Stream.emits(List(frame)).through(rtp.audio).compile.drain
        wall <- IO.realTime
        info <- rtp.senderInfo
      } yield {
        val seconds = (info.getOrElse(fail("no sender info")).ntp >>> 32) -
          2_208_988_800L
        assert(math.abs(seconds - wall.toSeconds) <= 2, seconds)
      }
    }
  }

  test("a source is the one its packets carry") {
    fixture().use { (rtp, receiver, _) =>
      for {
        _   <- Stream.emits(List(frame)).through(rtp.audio).compile.drain
        got <- received(receiver, 200.millis)
      } yield assertEquals(got.map(_.header.ssrc), List(rtp.source))
    }
  }

  test(
    "no audio gets between the packets of a digit, when it is sent at the very moment the digit begins"
  ) {
    fixture().use { (rtp, receiver, _) =>
      // A frame and a digit set off together, again and again: whichever asks
      // first whether a digit is being sent, the other is on its heels, which
      // is the only way audio can slip in.
      val rounds          = 40
      val round: IO[Unit] =
        IO.both(
          Stream.emit(frame).through(rtp.audio).compile.drain,
          rtp.telephoneEvent(DtmfDigit.D2, 20.millis).start.void
        ).void
      val drive = Stream
        .awakeEvery[IO](60.millis)
        .take(rounds.toLong)
        .evalMap(_ => round)
        .compile
        .drain
      // Audio between the digits, as a stream would send.
      val steady = Stream
        .awakeEvery[IO](20.millis)
        .map(_ => frame)
        .through(rtp.audio)
        .compile
        .drain
      (steady.background, drive.background).tupled.use { _ =>
        received(receiver, (rounds * 60 + 600).millis).map { packets =>
          val kinds = packets.map(_.header.payloadType)
          // Three end packets for each digit.
          assertEquals(kinds.count(_ == pt(101)), rounds * 3)
          // The packets of one digit share the time it began. Audio between the
          // first and the last of them is audio in a digit.
          val digits = packets.zipWithIndex
            .filter(_._1.header.payloadType == pt(101))
            .groupBy(_._1.header.timestamp)
            .values
          val intruded = digits.count { group =>
            val places = group.map(_._2)
            packets
              .slice(places.min, places.max + 1)
              .exists(_.header.payloadType != pt(101))
          }
          assertEquals(intruded, 0, kinds.mkString(","))
        }
      }
    }
  }
}
