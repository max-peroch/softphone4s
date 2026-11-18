package softphone4s.media

import scala.collection.immutable.TreeMap

/** A fixed-size jitter buffer: it takes packets in whatever order and at
  * whatever times they arrive, and gives back one frame for every 20 ms, in
  * order, with the gaps made plain.
  *
  * Playing does not begin until `depth` packets are held (or, for the last
  * packets of a talkspurt, until that many frames of time have gone by), and
  * after that the buffer stays at that distance behind the newest packet. A
  * packet that is late for its place is dropped, not played out of turn; one
  * that is missing when later ones are held is a gap, reported as lost; and a
  * buffer that runs dry waits to fill again, as at the start of a talkspurt.
  *
  * The cushion adapts. A packet that comes after its turn, or a pause in the
  * packets that was no end of speech (the next packet does not carry the marker
  * bit that begins a talkspurt), shows that the cushion was too small, and it
  * grows by a frame, up to `maxDepth`; it takes effect when the buffer next
  * fills. After [[JitterBuffer.ShrinkAfter]] frames in a row with nothing of
  * the kind it shrinks by one, down to the configured depth.
  *
  * It is a pure value: [[push]] and [[pull]] give the buffer that follows. The
  * sequence numbers are widened past their 16 bits so that the wrap is no
  * special case.
  */
final case class JitterBuffer private (
    config: JitterConfig,
    ssrc: Option[softphone4s.model.Ssrc],
    held: TreeMap[Long, JitterPacket],
    newest: Option[Long],
    phase: JitterBuffer.Phase,
    lastPlayed: Option[Long],
    stats: JitterStats,
    depth: Int,
    calm: Int,
    starved: Boolean
) {
  import JitterBuffer.*

  /** Takes in a packet, and says what became of it. */
  def push(packet: JitterPacket): (JitterBuffer, Arrival) =
    if ssrc.exists(_ != packet.ssrc) then restarted(packet).push(packet)
    else {
      val widened = unwrap(packet.sequence, newest)
      if newest.exists(n => math.abs(widened - n) > MaxJump) then
        restarted(packet).push(packet)
      else arrive(widened, packet)
    }

  /** The frame for the next 20 ms. */
  def pull: (JitterBuffer, Playout) = phase match {
    case Phase.Playing(_)        => play
    case Phase.Buffering(waited) =>
      if held.isEmpty then (copy(phase = Phase.Buffering(0)), Playout.Waiting)
      else if held.size >= depth || waited + 1 >= depth then
        copy(phase = Phase.Playing(held.firstKey)).play
      else (copy(phase = Phase.Buffering(waited + 1)), Playout.Waiting)
  }

  private def arrive(
      widened: Long,
      packet: JitterPacket
  ): (JitterBuffer, Arrival) = {
    val tooLate = lastPlayed.exists(widened <= _) || (phase match {
      case Phase.Playing(expected) => widened < expected
      case _                       => false
    })
    if tooLate then
      (
        copy(stats = stats.copy(late = stats.late + 1)).grown,
        Arrival.Late
      )
    else if held.contains(widened) then
      (
        copy(stats = stats.copy(duplicates = stats.duplicates + 1)),
        Arrival.Duplicate
      )
    else {
      val outOfOrder = newest.exists(widened < _)
      val taken      = copy(
        ssrc = Some(packet.ssrc),
        held = held.updated(widened, packet),
        newest = Some(newest.fold(widened)(math.max(_, widened))),
        stats = stats.copy(
          received = stats.received + 1,
          reordered = stats.reordered + (if outOfOrder then 1 else 0)
        )
      )
      // A pause that the next packet does not call the start of speech was the
      // network's.
      val settled =
        if starved then
          (if packet.marker then taken else taken.grown).copy(starved = false)
        else taken
      (settled.withinCapacity, Arrival.Accepted)
    }
  }

  /** The cushion a frame larger, if it may be. Nothing has gone well since. */
  private def grown: JitterBuffer =
    if depth < config.maxDepth then
      copy(
        depth = depth + 1,
        calm = 0,
        stats = stats.copy(depth = depth + 1)
      )
    else copy(calm = 0)

  /** A frame went by with nothing wrong; after enough, the cushion is a frame
    * smaller, if it is larger than it need be.
    */
  private def calmer: JitterBuffer =
    if calm + 1 >= ShrinkAfter && depth > config.depth then
      copy(depth = depth - 1, calm = 0, stats = stats.copy(depth = depth - 1))
    else copy(calm = calm + 1)

  /** One packet too many: the oldest goes, and playing carries on from the
    * oldest left. Packets come in one at a time, so there is never more than
    * one too many, and the capacity is at least 1, so one is left.
    */
  private def withinCapacity: JitterBuffer =
    if held.size <= config.capacity then this
    else {
      val rest = held.tail
      copy(
        held = rest,
        phase = phase match {
          case Phase.Playing(expected) =>
            Phase.Playing(math.max(expected, rest.firstKey))
          case buffering => buffering
        },
        stats = stats.copy(overflows = stats.overflows + 1)
      )
    }

  private def play: (JitterBuffer, Playout) = phase match {
    case Phase.Playing(expected) =>
      held.get(expected) match {
        case Some(packet) =>
          (
            copy(
              held = held.removed(expected),
              phase = Phase.Playing(expected + 1),
              lastPlayed = Some(expected),
              stats = stats.copy(played = stats.played + 1)
            ).calmer,
            Playout.Frame(packet)
          )
        case None if held.nonEmpty =>
          (
            copy(
              phase = Phase.Playing(expected + 1),
              calm = 0,
              stats = stats.copy(lost = stats.lost + 1)
            ),
            Playout.Lost
          )
        case None =>
          (
            copy(
              phase = Phase.Buffering(0),
              calm = 0,
              starved = true,
              stats = stats.copy(underruns = stats.underruns + 1)
            ),
            Playout.Waiting
          )
      }
    case Phase.Buffering(_) => pull
  }

  /** A stream that began again: another source, or a jump too big to be this
    * one. What was held belongs to the old one.
    */
  private def restarted(packet: JitterPacket): JitterBuffer =
    JitterBuffer
      .empty(config)
      .copy(ssrc = Some(packet.ssrc), stats = stats, depth = depth)
}

object JitterBuffer {

  /** Where the buffer is in its cycle. */
  enum Phase {

    /** Not playing: filling, having waited `waited` frames since the first
      * packet was held.
      */
    case Buffering(waited: Int)

    /** Playing; `expected` is the widened sequence number due next. */
    case Playing(expected: Long)
  }

  /** What became of a packet that arrived. */
  enum Arrival {

    /** Held, to be played in its turn. */
    case Accepted

    /** Its turn had gone: it was a gap, or was played, already. */
    case Late

    /** Held already. */
    case Duplicate
  }

  /** The frames of smooth playing, 10 seconds' worth, after which a cushion
    * grown beyond its depth gives a frame back.
    */
  val ShrinkAfter: Int = 500

  /** A sequence number further than this from the newest is a new stream, as
    * RFC 3550 §A.1 says of a jump of more than 3 000.
    */
  val MaxJump: Long = 3000

  def empty(config: JitterConfig): JitterBuffer =
    new JitterBuffer(
      config,
      None,
      TreeMap.empty,
      None,
      Phase.Buffering(0),
      None,
      JitterStats(depth = config.depth),
      config.depth,
      0,
      starved = false
    )

  /** The 16-bit `sequence` as the number nearest `reference`, counting the
    * times it has wrapped.
    */
  private def unwrap(sequence: Int, reference: Option[Long]): Long =
    reference.fold(sequence.toLong) { r =>
      val low   = (r & 0xffffL).toInt
      val delta = sequence - low
      // Past half a cycle either way it is in the next or the last one.
      val cycles =
        if delta > 32767 then -65536L else if delta < -32768 then 65536L else 0L
      r - low + sequence + cycles
    }
}
