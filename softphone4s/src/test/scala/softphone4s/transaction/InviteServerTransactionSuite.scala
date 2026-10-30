package softphone4s.transaction

import munit.FunSuite
import softphone4s.fsm.TimerAction
import softphone4s.transaction.TransactionExamples.*

import scala.concurrent.duration.*

class InviteServerTransactionSuite extends FunSuite {

  private val config = TransactionConfig()

  private def step(state: InviteServerState, event: ServerEvent) =
    InviteServerTransaction.transition(event).run(state).value

  private def start(reliability: Reliability = Reliability.Unreliable) =
    InviteServerTransaction.start(invite, reliability, config)

  private def startTimer(t: TransactionTimer, after: FiniteDuration) =
    ServerAction.Timer(TimerAction.Start(t, after))

  private def cancelTimer(t: TransactionTimer) =
    ServerAction.Timer(TimerAction.Cancel(t))

  private def respond(code: Int) = ServerEvent.Respond(response(invite, code))

  private def fired(t: TransactionTimer) = ServerEvent.TimerFired(t)

  private def completed(reliability: Reliability = Reliability.Unreliable) =
    step(start(reliability)._1, respond(486))._1

  test("start passes the INVITE up and answers 100 Trying") {
    val (state, actions) = start()
    assert(state.isInstanceOf[InviteServerState.Proceeding])
    assertEquals(
      actions,
      List(
        ServerAction.PassUp(invite),
        ServerAction.Transmit(TransactionMessages.trying(invite))
      )
    )
  }

  test(
    "100 Trying copies the request's identifying headers and adds no To tag"
  ) {
    val trying = TransactionMessages.trying(invite)
    assertEquals(trying.statusCode, 100)
    assertEquals(trying.headers.vias, invite.headers.vias)
    assertEquals(trying.headers.callId, invite.headers.callId)
    assertEquals(trying.headers.from, invite.headers.from)
    assertEquals(trying.headers.to, invite.headers.to)
    assertEquals(trying.headers.cseq, invite.headers.cseq)
  }

  test("a retransmitted INVITE repeats the latest response") {
    val (s0, _) = start()
    assertEquals(
      step(s0, ServerEvent.RequestRetransmitted)._2,
      List(ServerAction.Transmit(TransactionMessages.trying(invite)))
    )
    val ringing = response(invite, 180)
    val (s1, _) = step(s0, ServerEvent.Respond(ringing))
    assertEquals(
      step(s1, ServerEvent.RequestRetransmitted)._2,
      List(ServerAction.Transmit(ringing))
    )
  }

  test("provisional responses are sent and keep Proceeding") {
    val r                = response(invite, 183)
    val (state, actions) = step(start()._1, ServerEvent.Respond(r))
    assert(state.isInstanceOf[InviteServerState.Proceeding])
    assertEquals(actions, List(ServerAction.Transmit(r)))
  }

  test(
    "a 2xx is sent and ends the transaction: the user agent retransmits it"
  ) {
    val r                = response(invite, 200)
    val (state, actions) = step(start()._1, ServerEvent.Respond(r))
    assertEquals(state, InviteServerState.Terminated)
    assertEquals(actions, List(ServerAction.Transmit(r)))
  }

  test(
    "a failure is sent, then retransmitted on Timer G and awaited on Timer H"
  ) {
    val r                = response(invite, 486)
    val (state, actions) = step(start()._1, ServerEvent.Respond(r))
    assert(state.isInstanceOf[InviteServerState.Completed])
    assertEquals(
      actions,
      List(
        ServerAction.Transmit(r),
        startTimer(TransactionTimer.G, 500.millis),
        startTimer(TransactionTimer.H, 32.seconds)
      )
    )
  }

  test("Timer G doubles up to the T2 cap") {
    val r          = response(invite, 486)
    val intervals  = List(1.second, 2.seconds, 4.seconds, 4.seconds)
    val (_, trace) =
      intervals.foldLeft((completed(), List.empty[List[ServerAction]])) {
        case ((s, acc), _) =>
          val (next, actions) = step(s, fired(TransactionTimer.G))
          (next, acc :+ actions)
      }
    trace.zip(intervals).foreach { (actions, next) =>
      assertEquals(
        actions,
        List(ServerAction.Transmit(r), startTimer(TransactionTimer.G, next))
      )
    }
  }

  test("a retransmitted INVITE in Completed repeats the failure") {
    assertEquals(
      step(completed(), ServerEvent.RequestRetransmitted)._2,
      List(ServerAction.Transmit(response(invite, 486)))
    )
  }

  test("the ACK confirms the failure, stops G and H, and starts Timer I") {
    val (state, actions) = step(completed(), ServerEvent.AckReceived)
    assert(state.isInstanceOf[InviteServerState.Confirmed])
    assertEquals(
      actions,
      List(
        cancelTimer(TransactionTimer.G),
        cancelTimer(TransactionTimer.H),
        startTimer(TransactionTimer.I, 5.seconds)
      )
    )
  }

  test("Timer I ends Confirmed, and retransmitted ACKs are absorbed") {
    val (confirmed, _) = step(completed(), ServerEvent.AckReceived)
    assertEquals(step(confirmed, ServerEvent.AckReceived), (confirmed, Nil))
    assertEquals(
      step(confirmed, ServerEvent.RequestRetransmitted),
      (confirmed, Nil)
    )
    assertEquals(
      step(confirmed, fired(TransactionTimer.I)),
      (InviteServerState.Terminated, Nil)
    )
  }

  test("Timer H tells the user no ACK came and ends the transaction") {
    val (state, actions) = step(completed(), fired(TransactionTimer.H))
    assertEquals(state, InviteServerState.Terminated)
    assert(actions.contains(ServerAction.AckTimedOut))
  }

  test("over a reliable transport there is no Timer G and the ACK ends it") {
    val (s1, actions) = step(start(Reliability.Reliable)._1, respond(486))
    assertEquals(
      actions,
      List(
        ServerAction.Transmit(response(invite, 486)),
        startTimer(TransactionTimer.H, 32.seconds)
      )
    )
    assertEquals(
      step(s1, ServerEvent.AckReceived)._1,
      InviteServerState.Terminated
    )
  }

  test("a transport error ends the transaction and tells the user") {
    List(start()._1, completed(), step(completed(), ServerEvent.AckReceived)._1)
      .foreach { s =>
        val (state, actions) = step(s, ServerEvent.TransportError)
        assertEquals(state, InviteServerState.Terminated)
        assert(actions.contains(ServerAction.TransportFailed))
      }
  }

  test("a user response in Completed or Confirmed is ignored") {
    List(completed(), step(completed(), ServerEvent.AckReceived)._1).foreach {
      s =>
        assertEquals(step(s, respond(200)), (s, Nil))
    }
  }
}
