package softphone4s.transaction

import munit.FunSuite
import softphone4s.fsm.TimerAction
import softphone4s.model.*
import softphone4s.transaction.TransactionExamples.*

import scala.concurrent.duration.*

class InviteClientTransactionSuite extends FunSuite {

  private val config = TransactionConfig()

  private def step(state: InviteClientState, event: ClientEvent) =
    InviteClientTransaction.transition(event).run(state).value

  private def start(reliability: Reliability = Reliability.Unreliable) =
    InviteClientTransaction.start(invite, reliability, config)

  private def startTimer(t: TransactionTimer, after: FiniteDuration) =
    ClientAction.Timer(TimerAction.Start(t, after))

  private def cancelTimer(t: TransactionTimer) =
    ClientAction.Timer(TimerAction.Cancel(t))

  private def fired(t: TransactionTimer) = ClientEvent.TimerFired(t)

  private def responded(code: Int) =
    ClientEvent.Response(response(invite, code))

  test("start sends the INVITE and starts timers A and B") {
    val (state, actions) = start()
    assert(state.isInstanceOf[InviteClientState.Calling])
    assertEquals(
      actions,
      List(
        ClientAction.Transmit(invite),
        startTimer(TransactionTimer.A, 500.millis),
        startTimer(TransactionTimer.B, 32.seconds)
      )
    )
  }

  test("over a reliable transport there is no Timer A") {
    val (_, actions) = start(Reliability.Reliable)
    assertEquals(
      actions,
      List(
        ClientAction.Transmit(invite),
        startTimer(TransactionTimer.B, 32.seconds)
      )
    )
  }

  test("Timer A retransmits with a doubling interval and no cap") {
    val (s0, _)  = start()
    val (s1, a1) = step(s0, fired(TransactionTimer.A))
    val (s2, a2) = step(s1, fired(TransactionTimer.A))
    val (s3, a3) = step(s2, fired(TransactionTimer.A))
    val (_, a4)  = step(s3, fired(TransactionTimer.A))
    List(a1, a2, a3, a4)
      .zip(List(1.second, 2.seconds, 4.seconds, 8.seconds))
      .foreach { (actions, next) =>
        assertEquals(
          actions,
          List(
            ClientAction.Transmit(invite),
            startTimer(TransactionTimer.A, next)
          )
        )
      }
  }

  test("Timer B ends the transaction and tells the user") {
    val (s0, _)          = start()
    val (state, actions) = step(s0, fired(TransactionTimer.B))
    assertEquals(state, InviteClientState.Terminated)
    assert(actions.contains(ClientAction.TimedOut))
    assert(!actions.exists {
      case ClientAction.PassUp(_) => true; case _ => false
    })
  }

  test("a provisional response moves to Proceeding and stops A and B") {
    val (s0, _)          = start()
    val r                = response(invite, 180)
    val (state, actions) = step(s0, ClientEvent.Response(r))
    assert(state.isInstanceOf[InviteClientState.Proceeding])
    assertEquals(
      actions,
      List(
        ClientAction.PassUp(r),
        cancelTimer(TransactionTimer.A),
        cancelTimer(TransactionTimer.B)
      )
    )
  }

  test("more provisional responses are passed up in Proceeding") {
    val (s0, _)  = start()
    val (s1, _)  = step(s0, responded(180))
    val r        = response(invite, 183)
    val (s2, a2) = step(s1, ClientEvent.Response(r))
    assert(s2.isInstanceOf[InviteClientState.Proceeding])
    assert(a2.contains(ClientAction.PassUp(r)))
  }

  test("a 2xx ends the transaction without an ACK, which is the dialog's") {
    val (s0, _)          = start()
    val r                = response(invite, 200)
    val (state, actions) = step(s0, ClientEvent.Response(r))
    assertEquals(state, InviteClientState.Terminated)
    assert(actions.contains(ClientAction.PassUp(r)))
    assert(!actions.exists {
      case ClientAction.Transmit(_) => true; case _ => false
    })
  }

  test("a 2xx after provisional responses ends it the same way") {
    val (s0, _)          = start()
    val (s1, _)          = step(s0, responded(180))
    val (state, actions) = step(s1, responded(200))
    assertEquals(state, InviteClientState.Terminated)
    assert(actions.exists {
      case ClientAction.PassUp(_) => true; case _ => false
    })
  }

  test(
    "a failure is passed up, ACKed by the transaction, and waits on Timer D"
  ) {
    val (s0, _)          = start()
    val r                = response(invite, 486, "Busy Here")
    val (state, actions) = step(s0, ClientEvent.Response(r))
    val ack              = TransactionMessages.ackForFailure(invite, r)
    state match {
      case InviteClientState.Completed(_, sent) => assertEquals(sent, ack)
      case other => fail(s"expected Completed, got $other")
    }
    assert(actions.contains(ClientAction.PassUp(r)))
    assert(actions.contains(ClientAction.Transmit(ack)))
    assert(actions.contains(startTimer(TransactionTimer.D, 32.seconds)))
  }

  test("the ACK for a failure copies the request and takes the response's To") {
    val r   = response(invite, 486)
    val ack = TransactionMessages.ackForFailure(invite, r)
    assertEquals(ack.method, SipMethod.Ack)
    assertEquals(ack.requestUri, invite.requestUri)
    assertEquals(ack.headers.callId, invite.headers.callId)
    assertEquals(ack.headers.from, invite.headers.from)
    assertEquals(ack.headers.to.tag.map(_.value), Some("b1"))
    assertEquals(ack.headers.cseq, cseqLike(invite, SipMethod.Ack))
    assertEquals(ack.headers.vias.toList, List(invite.headers.topVia))
    assertEquals(ack.headers.routes, invite.headers.routes)
    assertEquals(ack.body, "")
  }

  private def cseqLike(r: SipRequest, method: SipMethod): CSeq =
    CSeq(r.headers.cseq.seq, method)

  test("a failure after provisional responses is handled the same way") {
    val (s0, _)          = start()
    val (s1, _)          = step(s0, responded(180))
    val (state, actions) = step(s1, responded(603))
    assert(state.isInstanceOf[InviteClientState.Completed])
    assert(actions.exists {
      case ClientAction.Transmit(r) => r.method == SipMethod.Ack;
      case _                        => false
    })
  }

  test("a retransmitted failure in Completed is ACKed again, not passed up") {
    val (s0, _)     = start()
    val r           = response(invite, 486)
    val (s1, _)     = step(s0, ClientEvent.Response(r))
    val (s2, again) = step(s1, ClientEvent.Response(r))
    assertEquals(s2, s1)
    assertEquals(
      again,
      List(ClientAction.Transmit(TransactionMessages.ackForFailure(invite, r)))
    )
  }

  test("a late 2xx or provisional response in Completed is ignored") {
    val (s0, _) = start()
    val (s1, _) = step(s0, responded(486))
    List(100, 180, 200).foreach { code =>
      val (s, actions) = step(s1, responded(code))
      assertEquals(s, s1)
      assertEquals(actions, Nil)
    }
  }

  test("Timer D ends Completed") {
    val (s0, _)          = start()
    val (s1, _)          = step(s0, responded(486))
    val (state, actions) = step(s1, fired(TransactionTimer.D))
    assertEquals(state, InviteClientState.Terminated)
    assertEquals(actions, Nil)
  }

  test("over a reliable transport a failure ends the transaction at once") {
    val (s0, _)          = start(Reliability.Reliable)
    val (state, actions) = step(s0, responded(486))
    assertEquals(state, InviteClientState.Terminated)
    assert(actions.exists {
      case ClientAction.Transmit(r) => r.method == SipMethod.Ack;
      case _                        => false
    })
    assert(!actions.exists {
      case ClientAction.Timer(TimerAction.Start(_, _)) => true; case _ => false
    })
  }

  test("a transport error ends the transaction and tells the user") {
    List(
      start()._1,
      step(start()._1, responded(180))._1,
      step(start()._1, responded(486))._1
    ).foreach { s =>
      val (state, actions) = step(s, ClientEvent.TransportError)
      assertEquals(state, InviteClientState.Terminated)
      assert(actions.contains(ClientAction.TransportFailed))
    }
  }

  test("timers that no longer apply are ignored") {
    val (proceeding, _) = step(start()._1, responded(180))
    List(TransactionTimer.A, TransactionTimer.B, TransactionTimer.D).foreach {
      t =>
        val (s, actions) = step(proceeding, fired(t))
        assertEquals(s, proceeding)
        assertEquals(actions, Nil)
    }
  }

  test("a terminated transaction does nothing") {
    List(
      responded(200),
      responded(486),
      fired(TransactionTimer.B),
      ClientEvent.TransportError
    )
      .foreach { e =>
        val (s, actions) = step(InviteClientState.Terminated, e)
        assertEquals(s, InviteClientState.Terminated)
        assertEquals(actions, Nil)
      }
  }
}
