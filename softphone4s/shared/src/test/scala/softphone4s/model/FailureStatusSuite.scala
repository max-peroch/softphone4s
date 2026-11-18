package softphone4s.model

import munit.FunSuite

class FailureStatusSuite extends FunSuite {

  test("only 300 to 699 are failure statuses") {
    List(300, 486, 603, 699).foreach(c =>
      assert(FailureStatus(c).isRight, c.toString)
    )
    List(0, 100, 200, 299, 700, -1).foreach(c =>
      assert(FailureStatus(c).isLeft, c.toString)
    )
  }

  test("known statuses have their usual phrase") {
    assertEquals(FailureStatus.BusyHere.reason, "Busy Here")
    assertEquals(FailureStatus.Decline.value, 603)
    assertEquals(FailureStatus.Decline.reason, "Decline")
    assertEquals(FailureStatus.NotAcceptableHere.reason, "Not Acceptable Here")
  }

  test("other statuses get a generic phrase for their class") {
    List(
      302 -> "Redirection",
      418 -> "Request Failure",
      599 -> "Server Failure",
      699 -> "Global Failure"
    )
      .foreach { (code, phrase) =>
        assertEquals(
          FailureStatus(code).map(_.reason),
          Right(phrase),
          code.toString
        )
      }
  }
}
