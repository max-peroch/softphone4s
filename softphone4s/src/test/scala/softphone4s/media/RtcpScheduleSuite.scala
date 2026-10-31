package softphone4s.media

import munit.FunSuite

import scala.concurrent.duration.*

class RtcpScheduleSuite extends FunSuite {

  private def schedule(interval: FiniteDuration) =
    RtcpSchedule(interval).fold(e => fail(e), identity)

  test("the default is five seconds") {
    assertEquals(RtcpSchedule.Default.interval, 5.seconds)
  }

  test("each wait is between half and one and a half times the interval") {
    val s = schedule(10.seconds)
    assertEquals(s.next(0.0), 5.seconds)
    assertEquals(s.next(0.5), 10.seconds)
    assertEquals(s.next(1.0), 15.seconds)
  }

  test("the first wait is half as long") {
    val s = schedule(10.seconds)
    assertEquals(s.first(0.0), 2500.millis)
    assertEquals(s.first(0.5), 5.seconds)
    assertEquals(s.first(1.0), 7500.millis)
  }

  test("a random number out of range is held to it") {
    val s = schedule(10.seconds)
    assertEquals(s.next(-3.0), s.next(0.0))
    assertEquals(s.next(7.0), s.next(1.0))
  }

  test("an interval of nothing, or less, is refused") {
    assert(RtcpSchedule(Duration.Zero).isLeft)
    assert(RtcpSchedule((-1).second).isLeft)
  }
}
