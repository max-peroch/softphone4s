package softphone4s.fsm

import munit.FunSuite
import softphone4s.model.*
import softphone4s.fsm.SipAction.*
import softphone4s.fsm.CallState.*
import softphone4s.fsm.SipEvent.*
import softphone4s.fsm.TerminationReason.*

class CallFsmSuite extends FunSuite {

  private def step(
      state: CallState,
      event: SipEvent
  ): (CallState, List[SipAction]) =
    CallFsm.transition(event).run(state).value

  private val baseCtx = InviteCtx(
    callId = "call@test",
    localTag = "ltag",
    cseq = 1,
    callee = "sip:callee@host",
    localUri = "sip:user@realm",
    offerInInvite = true
  )

  private def mkHeaders(
      to: String = "<sip:callee@host>",
      cseq: CSeq = CSeq(1, SipMethod.Invite),
      contact: String = "<sip:callee@host:5060>",
      proxyAuth: Option[String] = None,
      wwwAuth: Option[String] = None
  ) = SipHeaders(
    callId = "call@test",
    from = "<sip:user@realm>;tag=ltag",
    to = to,
    via = "SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bK1",
    cseq = cseq,
    contact = contact,
    proxyAuthenticate = proxyAuth,
    wwwAuthenticate = wwwAuth
  )

  private def resp(
      code: Int,
      reason: String,
      to: String = "<sip:callee@host>",
      body: String = "",
      proxyAuth: Option[String] = None,
      wwwAuth: Option[String] = None
  ) = SipResponse(
    code,
    reason,
    mkHeaders(to = to, proxyAuth = proxyAuth, wwwAuth = wwwAuth),
    body
  )

  private def req(method: SipMethod, body: String = "") =
    SipRequest(
      method,
      "sip:user@realm",
      mkHeaders(cseq = CSeq(1, method)),
      body
    )

  private lazy val active: Active =
    step(
      Inviting(baseCtx),
      ResponseReceived(resp(200, "OK", to = "<sip:callee@host>;tag=rtag"))
    ) match {
      case (a: Active, _) => a
      case (other, _) => throw AssertionError(s"expected Active, got $other")
    }

  // ---- Inviting ---------------------------------------------------------------

  test("Inviting + 100 Trying → state unchanged, no actions") {
    val (state, actions) =
      step(Inviting(baseCtx), ResponseReceived(resp(100, "Trying")))
    assertEquals(state, Inviting(baseCtx))
    assertEquals(actions, Nil)
  }

  test("Inviting + 180 Ringing → Ringing, no actions") {
    val (state, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(resp(180, "Ringing", to = "<sip:callee@host>;tag=rtag"))
    )
    assert(
      state match { case _: CallState.Ringing => true; case _ => false },
      s"expected Ringing, got $state"
    )
    assertEquals(actions, Nil)
  }

  test(
    "Inviting + 407 → Inviting(cseq+1), sends ACK and retries with Proxy-Authorization"
  ) {
    val challenge        = """Digest realm="sip.example.com", nonce="abc""""
    val (state, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(
        resp(407, "Proxy Auth Required", proxyAuth = Some(challenge))
      )
    )
    assertEquals(state, Inviting(baseCtx.copy(cseq = 2)))
    assert(actions.exists {
      case SendAckForNon2xx(_, _) => true; case _ => false
    })
    assert(actions.exists {
      case RetryInviteWithAuth(_, "Proxy-Authorization", Some(_)) => true;
      case _                                                      => false
    })
  }

  test("Inviting + 401 → Inviting(cseq+1), retries with Authorization") {
    val challenge        = """Digest realm="sip.example.com", nonce="abc""""
    val (state, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(resp(401, "Unauthorized", wwwAuth = Some(challenge)))
    )
    assertEquals(state, Inviting(baseCtx.copy(cseq = 2)))
    assert(actions.exists {
      case RetryInviteWithAuth(_, "Authorization", Some(_)) => true;
      case _                                                => false
    })
  }

  test(
    "Inviting + 200 OK → Active, sends ACK, starts media, notifies answered"
  ) {
    val (state, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(
        resp(200, "OK", to = "<sip:callee@host>;tag=rtag", body = "v=0")
      )
    )
    assert(state.isInstanceOf[Active], s"expected Active, got $state")
    assert(actions.exists { case SendAck(_, None) => true; case _ => false })
    assert(actions.contains(StartMedia("v=0")))
    assert(actions.contains(NotifyAnswered))
  }

  test("Inviting + 486 Busy Here → Failed, notifies rejection") {
    val (state, actions) =
      step(Inviting(baseCtx), ResponseReceived(resp(486, "Busy Here")))
    assert(state.isInstanceOf[Failed], s"expected Failed, got $state")
    assert(actions.exists {
      case NotifyHangup(Rejected(486, _)) => true; case _ => false
    })
  }

  test("Inviting + Timeout → Failed, notifies timeout") {
    val (state, actions) = step(Inviting(baseCtx), Timeout)
    assert(state.isInstanceOf[Failed], s"expected Failed, got $state")
    assert(actions.contains(NotifyHangup(TimedOut)))
  }

  test("Inviting + HangupRequested → Closed(LocalHangup), sends Cancel") {
    val (state, actions) = step(Inviting(baseCtx), HangupRequested)
    assertEquals(state, Closed(LocalHangup))
    assert(actions.exists { case SendCancel(_) => true; case _ => false })
    assert(actions.contains(NotifyHangup(LocalHangup)))
  }

  // ---- Active -----------------------------------------------------------------

  test("Active + HangupRequested → ByeSent, stops media, sends BYE") {
    val (state, actions) = step(active, HangupRequested)
    assert(state.isInstanceOf[ByeSent], s"expected ByeSent, got $state")
    assert(actions.contains(StopMedia))
    assert(actions.exists { case SendBye(_) => true; case _ => false })
  }

  test(
    "Active + remote BYE → Closed(RemoteBye), stops media, sends BYE OK, notifies hangup"
  ) {
    val (state, actions) = step(active, RequestReceived(req(SipMethod.Bye)))
    assertEquals(state, Closed(RemoteBye))
    assert(actions.contains(StopMedia))
    assert(actions.exists { case SendByeOk(_) => true; case _ => false })
    assert(actions.contains(NotifyHangup(RemoteBye)))
  }

  test("Active + remote BYE → NotifyHangup(RemoteBye) precedes StopMedia") {
    val (_, actions) = step(active, RequestReceived(req(SipMethod.Bye)))
    val notifyIdx    = actions.indexWhere {
      case NotifyHangup(RemoteBye) => true; case _ => false
    }
    val stopIdx = actions.indexOf(StopMedia)
    assert(
      notifyIdx >= 0 && stopIdx >= 0,
      s"expected both actions in: $actions"
    )
    assert(
      notifyIdx < stopIdx,
      s"NotifyHangup must precede StopMedia, got: $actions"
    )
  }

  test("Active + INFO with Signal= body → SendInfoOk and NotifyDtmf") {
    val (_, actions) =
      step(active, RequestReceived(req(SipMethod.Info, body = "Signal=1\r\n")))
    assert(actions.exists { case SendInfoOk(_) => true; case _ => false })
    assert(actions.exists { case NotifyDtmf(_) => true; case _ => false })
  }

  test("Active + INFO with bare single-char body → NotifyDtmf") {
    val (_, actions) =
      step(active, RequestReceived(req(SipMethod.Info, body = "5")))
    assert(actions.exists { case NotifyDtmf(_) => true; case _ => false })
  }

  test("Active + INFO with unparseable body → only SendInfoOk, no NotifyDtmf") {
    val (_, actions) =
      step(active, RequestReceived(req(SipMethod.Info, body = "not-dtmf")))
    assert(actions.exists { case SendInfoOk(_) => true; case _ => false })
    assert(!actions.exists { case NotifyDtmf(_) => true; case _ => false })
  }

  test("Active + unrecognized event → no state change, no actions") {
    val (state, actions) = step(active, Timeout)
    assertEquals(state, active)
    assertEquals(actions, Nil)
  }

  // ---- ByeSent ----------------------------------------------------------------

  test("ByeSent + 200 OK → Closed(LocalHangup), notifies hangup") {
    val (state, actions) =
      step(ByeSent(active.dialog), ResponseReceived(resp(200, "OK")))
    assertEquals(state, Closed(LocalHangup))
    assert(actions.contains(NotifyHangup(LocalHangup)))
  }

  test("ByeSent + Timeout → Closed(LocalHangup), notifies hangup") {
    val (state, actions) = step(ByeSent(active.dialog), Timeout)
    assertEquals(state, Closed(LocalHangup))
    assert(actions.contains(NotifyHangup(LocalHangup)))
  }

  // ---- CANCEL race ------------------------------------------------------------

  test("Closed(LocalHangup) + 200 OK → stays Closed, sends ACK and BYE") {
    val (state, actions) = step(
      Closed(LocalHangup),
      ResponseReceived(resp(200, "OK", to = "<sip:callee@host>;tag=rtag"))
    )
    assertEquals(state, Closed(LocalHangup))
    assert(actions.exists { case SendAck(_, None) => true; case _ => false })
    assert(actions.exists { case SendBye(_) => true; case _ => false })
  }
}
