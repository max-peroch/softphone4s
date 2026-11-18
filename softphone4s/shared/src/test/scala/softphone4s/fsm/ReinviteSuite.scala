package softphone4s.fsm

import munit.FunSuite
import softphone4s.model.*

import scala.concurrent.duration.*

class ReinviteSuite extends FunSuite {

  test("the party that placed the call waits 2.1 to 4 seconds after a 491") {
    assertEquals(Reinvite.glareDelay(DialogRole.Uac, 0.0), 2100.millis)
    assertEquals(Reinvite.glareDelay(DialogRole.Uac, 1.0), 4000.millis)
    val mid = Reinvite.glareDelay(DialogRole.Uac, 0.5)
    assert(mid > 2100.millis && mid < 4000.millis, mid)
  }

  test("the other party waits 0 to 2 seconds") {
    assertEquals(Reinvite.glareDelay(DialogRole.Uas, 0.0), 0.millis)
    assertEquals(Reinvite.glareDelay(DialogRole.Uas, 1.0), 2000.millis)
  }

  test(
    "the two never wait the same time when the owner's range cannot overlap"
  ) {
    List(0.0, 0.25, 0.5, 0.75, 1.0).foreach { f =>
      assert(
        Reinvite.glareDelay(DialogRole.Uac, f) >
          Reinvite.glareDelay(DialogRole.Uas, f)
      )
    }
  }

  test("a fraction outside [0, 1] is clamped") {
    assertEquals(Reinvite.glareDelay(DialogRole.Uas, -3.0), 0.millis)
    assertEquals(Reinvite.glareDelay(DialogRole.Uas, 9.0), 2000.millis)
  }

  test("a hold offers sendonly and a resume sendrecv") {
    assertEquals(SessionChange.Hold.direction, MediaDirection.SendOnly)
    assertEquals(SessionChange.Resume.direction, MediaDirection.SendRecv)
  }
}
