package softphone4s.media

import munit.FunSuite

import scala.concurrent.duration.*

class RtpReceptionSuite extends FunSuite {

  private val Rate = 8000

  /** 20 ms apart in both the sequence and the clock, from `first`. */
  private def feed(
      start: RtpReception,
      from: Int,
      count: Int,
      sequence: Int => Int = identity
  ): RtpReception =
    (from until from + count).foldLeft(start)((r, n) =>
      r.hear(sequence(n) & 0xffff, n * 160L, (n * 20).millis)
    )

  private def begin(first: Int = 0): RtpReception =
    RtpReception.start(1L, Rate, first, first * 160L, (first * 20).millis)

  test("packets in order count as expected and none lost") {
    val r = feed(begin(), 1, 49)
    assertEquals((r.received, r.expected, r.lost), (50L, 50L, 0L))
    assertEquals(r.extendedHighest, 49L)
  }

  test("a gap is loss") {
    val r = feed(begin(), 1, 9).hear(20, 20 * 160L, 400.millis)
    assertEquals((r.received, r.expected, r.lost), (11L, 21L, 10L))
  }

  test("the sequence wrapping counts a cycle") {
    val r = feed(begin(65530), 65531, 12, sequence = identity)
    assertEquals(r.extendedHighest, 65536L + 6)
    assertEquals(r.lost, 0L)
  }

  test("a duplicate is received but not expected, so loss goes below zero") {
    val r = feed(begin(), 1, 4).hear(3, 3 * 160L, 62.millis)
    assertEquals((r.received, r.expected, r.lost), (6L, 5L, -1L))
  }

  test("a packet out of order does not move the highest sequence") {
    val r = begin()
      .hear(2, 320, 40.millis)
      .hear(1, 160, 41.millis)
    assertEquals((r.extendedHighest, r.lost), (2L, 0L))
  }

  test("a jump too far to be loss is a source that began again") {
    val r = feed(begin(), 1, 9).hear(30000, 99999L, 200.millis)
    assertEquals((r.received, r.expected, r.lost), (1L, 1L, 0L))
    assertEquals(r.firstSequence, 30000)
  }

  test("packets spaced exactly as they were sent have no jitter") {
    assertEquals(feed(begin(), 1, 99).jitterUnits, 0L)
  }

  test("a packet 10 ms late adds a sixteenth of its 80 units, and it decays") {
    val late =
      feed(begin(), 1, 4).hear(5, 5 * 160L, 110.millis) // due at 100 ms
    assertEquals(late.jitterUnits, 5L)                  // 80 / 16
    val after = (6 to 40).foldLeft(late)((r, n) =>
      r.hear(n, n * 160L, (n * 20 + 10).millis)
    )
    assert(after.jitterUnits < 5L, after.jitterUnits)
  }

  test("a timestamp that wraps at 32 bits does not look like jitter") {
    val start = 0xffffffffL - 160 * 2
    val r     = (0 to 6).foldLeft(
      RtpReception.start(1L, Rate, 0, start & 0xffffffffL, Duration.Zero)
    )((r, n) =>
      if n == 0 then r
      else r.hear(n, (start + n * 160L) & 0xffffffffL, (n * 20).millis)
    )
    assertEquals(r.jitterUnits, 0L)
  }

  test("a report counts the fraction lost since the one before it") {
    val first                = feed(begin(), 1, 9) // 10 of 10
    val (afterFirst, block1) = first.report(200.millis)
    assertEquals((block1.fractionLost, block1.cumulativeLost), (0, 0))
    // Sequences 10..19 expected, the odd 5 of them come: half lost.
    val second = (11 to 19 by 2).foldLeft(afterFirst)((r, n) =>
      r.hear(n, n * 160L, (n * 20).millis)
    )
    val (_, block2) = second.report(400.millis)
    assertEquals(block2.fractionLost, 128)
    assertEquals(block2.cumulativeLost, 5)
    assertEquals(block2.highestSequence, 19L)
  }

  test("a report quotes the last sender report and how long ago it came") {
    val r          = feed(begin(), 1, 9).senderReportHeard(0xabcdL, 100.millis)
    val (_, block) = r.report(600.millis)
    assertEquals(block.lastSender, 0xabcdL)
    assertEquals(block.delaySinceLastSender, 32768L) // half a second
  }

  test("a report before any sender report quotes none") {
    val (_, block) = begin().report(1.second)
    assertEquals((block.lastSender, block.delaySinceLastSender), (0L, 0L))
  }
}
