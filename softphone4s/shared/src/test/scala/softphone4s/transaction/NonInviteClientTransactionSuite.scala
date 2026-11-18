package softphone4s.transaction

import munit.FunSuite
import softphone4s.fsm.TimerAction
import softphone4s.transaction.TransactionExamples.*

import scala.concurrent.duration.*

class NonInviteClientTransactionSuite extends FunSuite {

  private val config = TransactionConfig()

  private def step(state: NonInviteClientState, event: ClientEvent) =
    NonInviteClientTransaction.transition(event).run(state).value

  private def start(reliability: Reliability = Reliability.Unreliable) =
    NonInviteClientTransaction.start(options, reliability, config)

  private def startTimer(t: TransactionTimer, after: FiniteDuration) =
    ClientAction.Timer(TimerAction.Start(t, after))

  private def fired(t: TransactionTimer) = ClientEvent.TimerFired(t)

  private def responded(code: Int) =
    ClientEvent.Response(response(options, code))

  test("start sends the request and starts timers E and F") {
    val (state, actions) = start()
    assert(state.isInstanceOf[NonInviteClientState.Trying])
    assertEquals(
      actions,
      List(
        ClientAction.Transmit(options),
        startTimer(TransactionTimer.E, 500.millis),
        startTimer(TransactionTimer.F, 32.seconds)
      )
    )
  }

  test("over a reliable transport there is no Timer E") {
    assertEquals(
      start(Reliability.Reliable)._2,
      List(
        ClientAction.Transmit(options),
        startTimer(TransactionTimer.F, 32.seconds)
      )
    )
  }

  test("Timer E doubles up to the T2 cap and stays there") {
    val intervals  = List(1.second, 2.seconds, 4.seconds, 4.seconds, 4.seconds)
    val (_, trace) =
      intervals.foldLeft((start()._1, List.empty[List[ClientAction]])) {
        case ((s, acc), _) =>
          val (next, actions) = step(s, fired(TransactionTimer.E))
          (next, acc :+ actions)
      }
    trace.zip(intervals).foreach { (actions, next) =>
      assertEquals(
        actions,
        List(
          ClientAction.Transmit(options),
          startTimer(TransactionTimer.E, next)
        )
      )
    }
  }

  test("in Proceeding Timer E fires at T2") {
    val (s1, _) = step(start()._1, responded(100))
    assert(s1.isInstanceOf[NonInviteClientState.Proceeding])
    val (s2, actions) = step(s1, fired(TransactionTimer.E))
    assertEquals(s2, s1)
    assertEquals(
      actions,
      List(
        ClientAction.Transmit(options),
        startTimer(TransactionTimer.E, 4.seconds)
      )
    )
  }

  test("a provisional response is passed up and keeps Timer E running") {
    val r                = response(options, 100)
    val (state, actions) = step(start()._1, ClientEvent.Response(r))
    assert(state.isInstanceOf[NonInviteClientState.Proceeding])
    assertEquals(actions, List(ClientAction.PassUp(r)))
  }

  test("a final response completes the transaction and waits on Timer K") {
    List(200, 404, 500).foreach { code =>
      val r                = response(options, code)
      val (state, actions) = step(start()._1, ClientEvent.Response(r))
      assert(state.isInstanceOf[NonInviteClientState.Completed], code.toString)
      assert(actions.contains(ClientAction.PassUp(r)))
      assert(actions.contains(startTimer(TransactionTimer.K, 5.seconds)))
      assert(
        actions.contains(
          ClientAction.Timer(TimerAction.Cancel(TransactionTimer.E))
        )
      )
      assert(
        actions.contains(
          ClientAction.Timer(TimerAction.Cancel(TransactionTimer.F))
        )
      )
    }
  }

  test("a final response after a provisional one completes it too") {
    val (s1, _)    = step(start()._1, responded(100))
    val (state, _) = step(s1, responded(200))
    assert(state.isInstanceOf[NonInviteClientState.Completed])
  }

  test("over a reliable transport a final response ends it at once") {
    val (state, actions) = step(start(Reliability.Reliable)._1, responded(200))
    assertEquals(state, NonInviteClientState.Terminated)
    assert(!actions.exists {
      case ClientAction.Timer(TimerAction.Start(_, _)) => true; case _ => false
    })
  }

  test("retransmitted responses in Completed are absorbed") {
    val (s1, _) = step(start()._1, responded(200))
    List(100, 200, 500).foreach { code =>
      val (s, actions) = step(s1, responded(code))
      assertEquals(s, s1)
      assertEquals(actions, Nil)
    }
  }

  test("Timer K ends Completed") {
    val (s1, _)          = step(start()._1, responded(200))
    val (state, actions) = step(s1, fired(TransactionTimer.K))
    assertEquals(state, NonInviteClientState.Terminated)
    assertEquals(actions, Nil)
  }

  test("Timer F times out Trying and Proceeding alike") {
    val trying     = start()._1
    val proceeding = step(trying, responded(100))._1
    List(trying, proceeding).foreach { s =>
      val (state, actions) = step(s, fired(TransactionTimer.F))
      assertEquals(state, NonInviteClientState.Terminated)
      assert(actions.contains(ClientAction.TimedOut))
    }
  }

  test("a transport error ends the transaction and tells the user") {
    List(
      start()._1,
      step(start()._1, responded(100))._1,
      step(start()._1, responded(200))._1
    )
      .foreach { s =>
        val (state, actions) = step(s, ClientEvent.TransportError)
        assertEquals(state, NonInviteClientState.Terminated)
        assert(actions.contains(ClientAction.TransportFailed))
      }
  }

  test("a terminated transaction does nothing") {
    List(
      responded(200),
      fired(TransactionTimer.F),
      fired(TransactionTimer.E),
      ClientEvent.TransportError
    )
      .foreach { e =>
        val (s, actions) = step(NonInviteClientState.Terminated, e)
        assertEquals(s, NonInviteClientState.Terminated)
        assertEquals(actions, Nil)
      }
  }
}
