package softphone4s.model

import munit.FunSuite

import scala.concurrent.duration.*

class PositiveDurationSuite extends FunSuite {

  test("zero and a negative duration are refused, the smallest one is not") {
    assert(PositiveDuration(Duration.Zero).isLeft)
    assert(PositiveDuration((-1).second).isLeft)
    assert(PositiveDuration((-1).nanosecond).isLeft)
    assertEquals(
      PositiveDuration(1.nanosecond).map(_.value),
      Right(1.nanosecond)
    )
    assertEquals(PositiveDuration(5.seconds).map(_.value), Right(5.seconds))
  }
}
