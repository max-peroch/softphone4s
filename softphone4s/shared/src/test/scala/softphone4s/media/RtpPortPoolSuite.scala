package softphone4s.media

import com.comcast.ip4s.*
import munit.FunSuite

class RtpPortPoolSuite extends FunSuite {

  private def range(from: Port, to: Port) =
    RtpPortRange(from, to).fold(e => fail(e), identity)

  // ---- The range ------------------------------------------------------------------

  test("a range is pairs of an even port and the odd one after it") {
    val r = range(port"8000", port"8999")
    assertEquals((r.first, r.last, r.pairs), (8000, 8999, 500))
    assertEquals(r.portOf(0), Some(port"8000"))
    assertEquals(r.portOf(499), Some(port"8998"))
    assertEquals(r.portOf(500), None)
    assertEquals(r.portOf(-1), None)
  }

  test(
    "it begins at the first even port, and a last port with no partner is left out"
  ) {
    assertEquals(range(port"8001", port"8999").first, 8002)
    val r = range(port"8000", port"8004")
    assertEquals((r.pairs, r.last), (2, 8003))
  }

  test("an RTP port is told from its pair by its place in the range") {
    val r = range(port"8000", port"8999")
    assertEquals(r.indexOf(port"8000"), Some(0))
    assertEquals(r.indexOf(port"8002"), Some(1))
    assertEquals(r.indexOf(port"8001"), None) // RTCP, not RTP
    assertEquals(r.indexOf(port"7998"), None)
    assertEquals(r.indexOf(port"9000"), None)
  }

  test("a range with no pair in it is refused") {
    assert(RtpPortRange(port"8000", port"8000").isLeft)
    assert(RtpPortRange(port"8001", port"8002").isLeft)
    assert(RtpPortRange(port"9000", port"8000").isLeft)
    assert(RtpPortRange(port"8000", port"8001").isRight)
  }

  test("the top of the port space is a range too") {
    val r = range(port"65534", port"65535")
    assertEquals(r.pairs, 1)
    assertEquals(r.portOf(0), Some(port"65534"))
  }

  // ---- The pool -------------------------------------------------------------------

  private val four = range(port"20000", port"20007")

  test(
    "the first pair tried is the first of the range, then the one after the last taken"
  ) {
    val pool = RtpPortPool(four)
    assertEquals(pool.candidates(10), List(0, 1, 2, 3))
    val Some(taken) = pool.reserve(0): @unchecked
    assertEquals(taken.candidates(10), List(1, 2, 3))
  }

  test("a pair that is taken cannot be taken again") {
    val Some(taken) = RtpPortPool(four).reserve(2): @unchecked
    assertEquals(taken.reserve(2), None)
  }

  test(
    "a pair given back is not offered again before the others have been tried"
  ) {
    val Some(a)  = RtpPortPool(four).reserve(0): @unchecked
    val released = a.release(port"20000")
    // 0 is free again, but it comes last: the cursor has only moved past it.
    assertEquals(released.candidates(10), List(1, 2, 3, 0))
  }

  test("going round the range, the next pair after the last is the first") {
    val Some(a) = RtpPortPool(four).reserve(3): @unchecked
    assertEquals(a.candidates(10), List(0, 1, 2))
  }

  test("a pair given up as unusable is free but tried last") {
    val Some(a) = RtpPortPool(four).reserve(1): @unchecked
    assertEquals(a.skip(1).candidates(10), List(2, 3, 0, 1))
  }

  test("only as many pairs as asked for are offered") {
    assertEquals(RtpPortPool(four).candidates(2), List(0, 1))
  }

  test("a pool with every pair taken offers none") {
    val full = (0 until 4).foldLeft(RtpPortPool(four))((pool, i) =>
      pool.reserve(i).getOrElse(fail("taken"))
    )
    assertEquals(full.candidates(10), Nil)
    assertEquals(full.inUse.size, 4)
  }

  test("giving back a port that is not the RTP port of a pair does nothing") {
    val Some(a) = RtpPortPool(four).reserve(0): @unchecked
    assertEquals(a.release(port"20001"), a) // RTCP
    assertEquals(a.release(port"30000"), a) // out of the range
  }

  test("a pool that has given everything back is empty again") {
    val Some(a) = RtpPortPool(four).reserve(0): @unchecked
    assert(a.release(port"20000").isEmpty)
  }
}
