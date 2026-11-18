package softphone4s.transaction

import munit.FunSuite
import softphone4s.fsm.TimerAction
import softphone4s.transaction.TransactionExamples.*

import scala.concurrent.duration.*

class NonInviteServerTransactionSuite extends FunSuite {

  private val config = TransactionConfig()

  private def step(state: NonInviteServerState, event: ServerEvent) =
    NonInviteServerTransaction.transition(event).run(state).value

  private def start(reliability: Reliability = Reliability.Unreliable) =
    NonInviteServerTransaction.start(options, reliability, config)

  private def respond(code: Int) = ServerEvent.Respond(response(options, code))

  test("start passes the request up and sends nothing") {
    val (state, actions) = start()
    assert(state.isInstanceOf[NonInviteServerState.Trying])
    assertEquals(actions, List(ServerAction.PassUp(options)))
  }

  test("a retransmitted request in Trying is discarded") {
    val (s0, _) = start()
    assertEquals(step(s0, ServerEvent.RequestRetransmitted), (s0, Nil))
  }

  test(
    "a provisional response moves to Proceeding and is repeated on retransmits"
  ) {
    val r             = response(options, 100)
    val (s1, actions) = step(start()._1, ServerEvent.Respond(r))
    assert(s1.isInstanceOf[NonInviteServerState.Proceeding])
    assertEquals(actions, List(ServerAction.Transmit(r)))
    assertEquals(
      step(s1, ServerEvent.RequestRetransmitted)._2,
      List(ServerAction.Transmit(r))
    )
  }

  test(
    "a final response completes it and starts Timer J, from Trying or Proceeding"
  ) {
    val r          = response(options, 200)
    val trying     = start()._1
    val proceeding = step(trying, respond(100))._1
    List(trying, proceeding).foreach { s =>
      val (state, actions) = step(s, ServerEvent.Respond(r))
      assert(state.isInstanceOf[NonInviteServerState.Completed])
      assertEquals(
        actions,
        List(
          ServerAction.Transmit(r),
          ServerAction.Timer(TimerAction.Start(TransactionTimer.J, 32.seconds))
        )
      )
    }
  }

  test("a retransmitted request in Completed repeats the final response") {
    val r             = response(options, 404)
    val (s1, _)       = step(start()._1, ServerEvent.Respond(r))
    val (s2, actions) = step(s1, ServerEvent.RequestRetransmitted)
    assertEquals(s2, s1)
    assertEquals(actions, List(ServerAction.Transmit(r)))
  }

  test("Timer J ends Completed") {
    val (s1, _) = step(start()._1, respond(200))
    assertEquals(
      step(s1, ServerEvent.TimerFired(TransactionTimer.J)),
      (NonInviteServerState.Terminated, Nil)
    )
  }

  test("over a reliable transport a final response ends it at once") {
    val r                = response(options, 200)
    val (state, actions) =
      step(start(Reliability.Reliable)._1, ServerEvent.Respond(r))
    assertEquals(state, NonInviteServerState.Terminated)
    assertEquals(actions, List(ServerAction.Transmit(r)))
  }

  test("a transport error ends the transaction and tells the user") {
    List(
      start()._1,
      step(start()._1, respond(100))._1,
      step(start()._1, respond(200))._1
    )
      .foreach { s =>
        val (state, actions) = step(s, ServerEvent.TransportError)
        assertEquals(state, NonInviteServerState.Terminated)
        assertEquals(actions, List(ServerAction.TransportFailed))
      }
  }

  test("a user response after the final one is ignored") {
    val (s1, _) = step(start()._1, respond(200))
    assertEquals(step(s1, respond(500)), (s1, Nil))
  }
}
