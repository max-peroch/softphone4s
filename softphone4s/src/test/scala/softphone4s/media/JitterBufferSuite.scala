package softphone4s.media

import fs2.Chunk
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

class JitterBufferSuite extends FunSuite with ScalaCheckSuite {

  private enum Step {
    case Push(sequence: Int, ssrc: Long = 1L)
    case Pull
  }
  import Step.*

  private def config(depth: Int = 3, capacity: Int = 12) =
    JitterConfig(depth, capacity).fold(e => fail(e), identity)

  private def packet(sequence: Int, ssrc: Long) =
    JitterPacket(
      sequence,
      sequence * 160L,
      ssrc,
      marker = false,
      Chunk.array(Array((sequence >> 8).toByte, sequence.toByte))
    )

  private def sequenceOf(payload: Chunk[Byte]): Int =
    ((payload(0) & 0xff) << 8) | (payload(1) & 0xff)

  private def show(out: Playout): String = out match {
    case Playout.Frame(payload) => s"F${sequenceOf(payload)}"
    case Playout.Lost           => "L"
    case Playout.Waiting        => "."
  }

  /** The buffer after the steps, what each pull gave and what each push became.
    */
  private def run(
      steps: List[Step],
      config: JitterConfig = config()
  ): (JitterBuffer, List[String], List[JitterBuffer.Arrival]) =
    steps.foldLeft(
      (
        JitterBuffer.empty(config),
        List.empty[String],
        List.empty[JitterBuffer.Arrival]
      )
    ) {
      case ((buffer, outs, arrivals), Push(sequence, ssrc)) =>
        val (next, arrival) = buffer.push(packet(sequence, ssrc))
        (next, outs, arrivals :+ arrival)
      case ((buffer, outs, arrivals), Pull) =>
        val (next, out) = buffer.pull
        (next, outs :+ show(out), arrivals)
    }

  private def pulls(n: Int): List[Step] = List.fill(n)(Pull)

  private def pushes(sequences: Int*): List[Step] =
    sequences.toList.map(Push(_))

  test(
    "nothing is played until a cushion of packets is held, and then they come in order"
  ) {
    val (_, outs, _) =
      run(List(Push(1), Pull, Push(2), Pull, Push(3)) ::: pulls(4))
    assertEquals(outs, List(".", ".", "F1", "F2", "F3", "."))
  }

  test(
    "packets that arrive out of order within the cushion are played in order"
  ) {
    val (buffer, outs, _) = run(pushes(1, 3, 2, 4) ::: pulls(5))
    assertEquals(outs, List("F1", "F2", "F3", "F4", "."))
    assertEquals(buffer.stats.reordered, 1L)
    assertEquals(buffer.stats.lost, 0L)
  }

  test(
    "a packet that is missing when later ones are held is a gap, played as lost"
  ) {
    val (buffer, outs, _) = run(pushes(1, 2, 4, 5) ::: pulls(5))
    assertEquals(outs, List("F1", "F2", "L", "F4", "F5"))
    assertEquals(buffer.stats.lost, 1L)
    assertEquals(buffer.stats.played, 4L)
  }

  test(
    "a packet for a gap that comes after its time is dropped, not played out of turn"
  ) {
    val (buffer, outs, arrivals) =
      run(pushes(1, 2, 4, 5) ::: pulls(3) ::: List(Push(3)) ::: pulls(2))
    assertEquals(outs, List("F1", "F2", "L", "F4", "F5"))
    assertEquals(arrivals.last, JitterBuffer.Arrival.Late)
    assertEquals(buffer.stats.late, 1L)
  }

  test(
    "a packet played already, sent again, is late, and one held, sent again, is a duplicate"
  ) {
    val (buffer, _, arrivals) =
      run(pushes(1, 2, 3) ::: List(Pull) ::: pushes(1, 2))
    assertEquals(
      arrivals.drop(3),
      List(JitterBuffer.Arrival.Late, JitterBuffer.Arrival.Duplicate)
    )
    assertEquals(buffer.stats.late, 1L)
    assertEquals(buffer.stats.duplicates, 1L)
  }

  test(
    "the sequence numbers wrap round without a gap, also when they arrive out of order across it"
  ) {
    val (_, inOrder, _) = run(pushes(65534, 65535, 0, 1) ::: pulls(4))
    assertEquals(inOrder, List("F65534", "F65535", "F0", "F1"))
    val (_, shuffled, _) = run(pushes(65534, 0, 65535, 1) ::: pulls(4))
    assertEquals(shuffled, List("F65534", "F65535", "F0", "F1"))
  }

  test("a buffer that runs dry waits to fill again, and carries on in order") {
    val (buffer, outs, _) = run(
      pushes(1, 2, 3) ::: pulls(4) ::: List(
        Push(4),
        Pull,
        Push(5),
        Pull,
        Push(6)
      ) ::: pulls(3)
    )
    assertEquals(outs, List("F1", "F2", "F3", ".", ".", ".", "F4", "F5", "F6"))
    assertEquals(buffer.stats.underruns, 1L)
  }

  test(
    "the last packets of a talkspurt are played after the same wait, though they are too few to fill the cushion"
  ) {
    val (_, outs, _) = run(pushes(1, 2) ::: pulls(5))
    assertEquals(outs, List(".", ".", "F1", "F2", "."))
  }

  test("a cushion of one plays as soon as there is a packet") {
    val (_, outs, _) = run(pushes(1) ::: pulls(2), config(1, 4))
    assertEquals(outs, List("F1", "."))
  }

  test(
    "more packets than the buffer holds push the oldest out, and playing carries on from the oldest left"
  ) {
    val (buffer, outs, _) =
      run(pushes((1 to 10)*) ::: pulls(6), config(3, 5))
    assertEquals(buffer.stats.overflows, 5L)
    assertEquals(outs.take(5), List("F6", "F7", "F8", "F9", "F10"))
  }

  test("the memory is bounded however many packets come") {
    val (buffer, _, _) = run(pushes((1 to 200)*), config(3, 8))
    assertEquals(buffer.held.size, 8)
  }

  test(
    "a packet from another source begins a new stream, keeping the count of what was done"
  ) {
    val (buffer, outs, _) = run(
      pushes(1, 2, 3) ::: List(Pull) ::: List(Push(500, ssrc = 2L)) ::: pulls(3)
    )
    assertEquals(outs.head, "F1")
    // Only the new source's packet is held, and the cushion has to form again.
    assertEquals(outs.tail, List(".", ".", "F500"))
    assertEquals(buffer.stats.played, 2L)
  }

  test(
    "a sequence number far from the newest is a new stream too, not 30 000 lost packets"
  ) {
    val (_, outs, _) = run(pushes(1, 40000) ::: pulls(4))
    assertEquals(outs, List(".", ".", "F40000", "."))
  }

  test("a depth of 0, or a capacity below the depth, is refused") {
    assert(JitterConfig(0, 4).isLeft)
    assert(JitterConfig(5, 4).isLeft)
    assert(JitterConfig(5, 5).isRight)
  }

  // A packet a tick, with neighbours swapped here and there: a cushion of three
  // absorbs that, so every packet is played, in order, and none is lost.
  property(
    "packets in a little disorder are all played in order, and none is lost"
  ) {
    val total = 60
    forAll(Gen.listOfN(total - 1, Gen.oneOf(true, false))) { swaps =>
      val arrival = swaps.zipWithIndex
        .foldLeft((Vector.range(0, total), false)) {
          case ((order, justSwapped), (swap, i)) =>
            if swap && !justSwapped then
              (order.updated(i, order(i + 1)).updated(i + 1, order(i)), true)
            else (order, false)
        }
        ._1
      val steps =
        arrival.toList.flatMap(s => List(Push(s), Pull)) ::: pulls(6)
      val played = run(steps)._2.filterNot(_ == ".")
      played == (0 until total).map(n => s"F$n").toList
    }
  }
}
