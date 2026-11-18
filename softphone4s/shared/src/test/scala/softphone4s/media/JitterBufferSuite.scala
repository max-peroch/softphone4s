package softphone4s.media

import softphone4s.model.Ssrc
import fs2.Chunk
import softphone4s.SipLiterals.pt
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

class JitterBufferSuite extends FunSuite with ScalaCheckSuite {

  private enum Step {
    case Push(sequence: Int, ssrc: Long = 1L, marker: Boolean = false)
    case Pull
  }
  import Step.*

  private def config(depth: Int = 3, capacity: Int = 12) =
    JitterConfig(depth, capacity).fold(e => fail(e), identity)

  private def packet(sequence: Int, ssrc: Long, marker: Boolean) =
    JitterPacket(
      sequence,
      sequence * 160L,
      Ssrc.fromBits(ssrc.toInt),
      marker,
      pt(0),
      Chunk.array(Array((sequence >> 8).toByte, sequence.toByte))
    )

  private def sequenceOf(payload: Chunk[Byte]): Int =
    ((payload(0) & 0xff) << 8) | (payload(1) & 0xff)

  private def show(out: Playout): String = out match {
    case Playout.Frame(packet) => s"F${sequenceOf(packet.payload)}"
    case Playout.Lost          => "L"
    case Playout.Waiting       => "."
  }

  /** The buffer after the steps, what each pull gave and what each push became.
    */
  private def run(
      steps: List[Step],
      config: JitterConfig = config()
  ): (JitterBuffer, List[String], List[JitterBuffer.Arrival]) = {
    val (buffer, outs, arrivals) = steps.foldLeft(
      (
        JitterBuffer.empty(config),
        Vector.empty[String],
        Vector.empty[JitterBuffer.Arrival]
      )
    ) {
      case ((buffer, outs, arrivals), Push(sequence, ssrc, marker)) =>
        val (next, arrival) = buffer.push(packet(sequence, ssrc, marker))
        (next, outs, arrivals :+ arrival)
      case ((buffer, outs, arrivals), Pull) =>
        val (next, out) = buffer.pull
        (next, outs :+ show(out), arrivals)
    }
    (buffer, outs.toList, arrivals.toList)
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

  test(
    "a long run across the wrap is played whole, in order, and the wrap is lost on no one"
  ) {
    val sequences = (60000 until 60000 + 12000).map(_ % 65536)
    val steps = sequences.toList.flatMap(s => List(Push(s), Pull)) ::: pulls(6)
    val (buffer, outs, arrivals) = run(steps)
    assertEquals(outs.filterNot(_ == ".").size, 12000)
    assertEquals(
      outs.filterNot(_ == ".").take(3),
      List("F60000", "F60001", "F60002")
    )
    assertEquals(
      outs.filterNot(_ == ".").slice(5535, 5538),
      List("F65535", "F0", "F1")
    )
    assert(arrivals.forall(_ == JitterBuffer.Arrival.Accepted))
    assertEquals(buffer.stats.lost, 0L)
    assertEquals(buffer.stats.late, 0L)
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

  // ---- The cushion adapts -------------------------------------------------------

  private val adaptive =
    JitterConfig.adaptive(3, 5, 12).fold(e => fail(e), identity)

  /** Plays three packets and runs dry: a pause in the stream. */
  private val pause = pushes(1, 2, 3) ::: pulls(4)

  test("a pause that no marked packet ends makes the cushion a frame larger") {
    val (buffer, _, _) = run(pause ::: List(Push(4)), adaptive)
    assertEquals(buffer.stats.underruns, 1L)
    assertEquals(buffer.stats.depth, 4)
  }

  test("a pause that a marked packet ends, the start of speech, leaves it") {
    val (buffer, _, _) = run(pause ::: List(Push(4, marker = true)), adaptive)
    assertEquals(buffer.stats.depth, 3)
  }

  test("a packet that comes after its turn makes the cushion a frame larger") {
    val (buffer, _, arrivals) =
      run(pushes(1, 2, 4, 5) ::: pulls(3) ::: List(Push(3)), adaptive)
    assertEquals(arrivals.last, JitterBuffer.Arrival.Late)
    assertEquals(buffer.stats.depth, 4)
  }

  test("the cushion grows no further than the most it may") {
    // Rounds that each run dry and are followed by a packet no marker begins:
    // contiguous sequence numbers, so that the only trouble is the pause.
    val (trouble, _) = (0 until 5).foldLeft((List.empty[Step], 1)) {
      case ((steps, n), _) =>
        val round = pushes((n to n + 5)*) ::: pulls(10) ::: List(Push(n + 6))
        (steps ::: round, n + 7)
    }
    val (buffer, _, _) = run(trouble, adaptive)
    assertEquals(buffer.stats.depth, 5)
  }

  test("a buffer whose most is its depth never changes") {
    val (buffer, _, _) = run(pause ::: List(Push(4)))
    assertEquals(buffer.stats.depth, 3)
  }

  test("the larger cushion is what the buffer fills to the next time") {
    // Three held would play at a depth of three; at four, they wait.
    val (_, outs, _) = run(pause ::: pushes(4, 5, 6) ::: List(Pull), adaptive)
    assertEquals(outs.last, ".")
    val (_, fixed, _) = run(pause ::: pushes(4, 5, 6) ::: List(Pull))
    assertEquals(fixed.last, "F4")
  }

  /** A stream with a frame a push, a frame a pull, from where `pause` left it.
    */
  private def steady(from: Int, frames: Int): List[Step] =
    (from until from + frames).toList.flatMap(n => List(Push(n), Pull))

  test(
    "after a long smooth stretch the cushion gives a frame back, down to its depth"
  ) {
    val grown         = pause ::: pushes(4, 5, 6, 7)
    val (early, _, _) = run(grown ::: steady(8, 400), adaptive)
    assertEquals(early.stats.depth, 4)
    val (later, _, _) = run(grown ::: steady(8, 700), adaptive)
    assertEquals(later.stats.depth, 3)
    // No further than the depth it started at.
    val (longer, _, _) = run(grown ::: steady(8, 2000), adaptive)
    assertEquals(longer.stats.depth, 3)
  }

  test("a lost packet starts the smooth stretch again") {
    val grown = pause ::: pushes(4, 5, 6, 7)
    // 300 smooth frames, then a gap in the sequence, then 300 more: neither
    // stretch reaches 500, though together they pass it.
    val steps = grown ::: steady(8, 300) ::: List(Push(310)) :::
      steady(311, 300)
    val (buffer, _, _) = run(steps, adaptive)
    assertEquals(buffer.stats.depth, 4)
  }

  test("a stream that begins again keeps the cushion it has grown") {
    val (buffer, _, _) =
      run(pause ::: List(Push(4)) ::: List(Push(100, ssrc = 2L)), adaptive)
    assertEquals(buffer.stats.depth, 4)
  }

  test(
    "a configuration whose most is below its depth, or above its room, is refused"
  ) {
    assert(JitterConfig.adaptive(4, 3, 12).isLeft)
    assert(JitterConfig.adaptive(3, 13, 12).isLeft)
    assert(JitterConfig.adaptive(3, 12, 12).isRight)
  }
}
