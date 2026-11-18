package softphone4s.runtime

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import munit.CatsEffectSuite
import softphone4s.fsm.TimerAction

import scala.concurrent.duration.*

class TimersSuite extends CatsEffectSuite {

  private def elapsed[A](io: IO[A]): IO[(A, FiniteDuration)] =
    IO.monotonic.flatMap(t0 =>
      io.flatMap(a => IO.monotonic.map(t1 => (a, t1 - t0)))
    )

  test("a timer fires once, after its duration") {
    TestControl
      .executeEmbed(
        elapsed(
          Timers.resource[IO, String].use { timers =>
            timers.start("A", 5.seconds) >> timers.fired.take(1).compile.toList
          }
        )
      )
      .map { (fired, took) =>
        assertEquals(fired, List("A"))
        assertEquals(took, 5.seconds)
      }
  }

  test("timers fire in order of their deadlines") {
    TestControl
      .executeEmbed(
        Timers.resource[IO, String].use { timers =>
          timers.start("late", 9.seconds) >> timers.start("early", 2.seconds) >>
            timers
              .start("middle", 4.seconds) >> timers.fired.take(3).compile.toList
        }
      )
      .map(fired => assertEquals(fired, List("early", "middle", "late")))
  }

  test("starting a running timer restarts it, and it fires only once") {
    TestControl
      .executeEmbed(
        elapsed(
          Timers.resource[IO, String].use { timers =>
            timers.start("A", 5.seconds) >> IO.sleep(3.seconds) >>
              timers.start("A", 5.seconds) >>
              timers.fired.interruptAfter(30.seconds).compile.toList
          }
        )
      )
      .map { (fired, _) => assertEquals(fired, List("A")) } >>
      TestControl
        .executeEmbed(
          Timers.resource[IO, String].use { timers =>
            timers.start("A", 5.seconds) >> IO.sleep(3.seconds) >>
              timers.start("A", 5.seconds) >>
              timers.fired.take(1).compile.toList.timed.map(_._1)
          }
        )
        .map(took => assertEquals(took, 8.seconds - 3.seconds))
  }

  test("a cancelled timer never fires") {
    TestControl
      .executeEmbed(
        Timers.resource[IO, String].use { timers =>
          timers.start("A", 5.seconds) >> timers.cancel("A") >>
            timers.fired.interruptAfter(30.seconds).compile.toList
        }
      )
      .map(fired => assertEquals(fired, Nil))
  }

  test("cancelling a timer that is not running is harmless") {
    TestControl.executeEmbed(
      Timers.resource[IO, String].use(_.cancel("nope"))
    )
  }

  test("a timer can be started again after it fired") {
    TestControl
      .executeEmbed(
        Timers.resource[IO, String].use { timers =>
          timers.start("A", 1.second) >> timers.fired.take(1).compile.toList >>
            timers.start("A", 1.second) >> timers.fired.take(1).compile.toList
        }
      )
      .map(fired => assertEquals(fired, List("A")))
  }

  test("run executes the actions a state machine emitted") {
    TestControl
      .executeEmbed(
        Timers.resource[IO, String].use { timers =>
          List(
            TimerAction.Start("A", 1.second),
            TimerAction.Start("B", 2.seconds),
            TimerAction.Cancel("A")
          ).traverse_(timers.run) >>
            timers.fired.interruptAfter(10.seconds).compile.toList
        }
      )
      .map(fired => assertEquals(fired, List("B")))
  }

  test("releasing the resource cancels timers that are still pending") {
    TestControl.executeEmbed(
      Timers.resource[IO, String].use(_.start("A", 1.hour)) >> IO.sleep(2.hours)
    )
  }
}
