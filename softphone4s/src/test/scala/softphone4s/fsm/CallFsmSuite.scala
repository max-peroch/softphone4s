package softphone4s.fsm

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.model.*
import softphone4s.fsm.SipAction.*
import softphone4s.fsm.CallState.*
import softphone4s.fsm.SipEvent.*
import softphone4s.fsm.TerminationReason.*

import scala.concurrent.duration.*

class CallFsmSuite extends FunSuite {

  private def step(
      state: CallState,
      event: SipEvent
  ): (CallState, List[SipAction]) =
    CallFsm.transition(event).run(state).value

  private val baseCtx = InviteCtx(
    callId = callId("call@test"),
    localTag = tag("ltag"),
    cseq = cseqNumber(1),
    callee = uri("sip:callee@host"),
    localUri = uri("sip:user@realm"),
    offerInInvite = true,
    inviteTimeout = 32.seconds
  )

  private def mkHeaders(
      to: String = "<sip:callee@host>",
      cseq: CSeq,
      contact: String = "<sip:callee@host:5060>",
      proxyAuth: List[String] = Nil,
      wwwAuth: List[String] = Nil
  ) = SipHeaders(
    callId = callId("call@test"),
    from = nameAddr("<sip:user@realm>;tag=ltag"),
    to = nameAddr(to),
    vias =
      NonEmptyList.one(via("SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bK1")),
    cseq = cseq,
    contacts = List(nameAddr(contact)),
    proxyAuthenticate = proxyAuth.map(challenge),
    wwwAuthenticate = wwwAuth.map(challenge)
  )

  private def resp(
      code: Int,
      reason: String,
      to: String = "<sip:callee@host>",
      body: String = "",
      proxyAuth: List[String] = Nil,
      wwwAuth: List[String] = Nil,
      method: SipMethod = SipMethod.Invite
  ) = SipResponse(
    code,
    reason,
    mkHeaders(
      to = to,
      cseq = cseq(1, method),
      proxyAuth = proxyAuth,
      wwwAuth = wwwAuth
    ),
    body
  )

  private def req(method: SipMethod, body: String = "") =
    SipRequest(
      method,
      uri("sip:user@realm"),
      mkHeaders(cseq = cseq(1, method)),
      body
    )

  private lazy val active: Active =
    step(
      Inviting(baseCtx),
      ResponseReceived(resp(200, "OK", to = "<sip:callee@host>;tag=rtag"))
    ) match {
      case (a: Active, _) => a
      case (other, _)     => fail(s"expected Active, got $other")
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
        resp(407, "Proxy Auth Required", proxyAuth = List(challenge))
      )
    )
    assertEquals(
      state,
      Inviting(
        baseCtx.copy(
          cseq = cseqNumber(2),
          authAttempts = Map(AuthKind.Proxy -> 1)
        )
      )
    )
    assert(actions.exists {
      case RetryInviteWithAuth(_, AuthKind.Proxy, _) =>
        true;
      case _ => false
    })
  }

  test("Inviting + 401 → Inviting(cseq+1), retries with Authorization") {
    val challenge        = """Digest realm="sip.example.com", nonce="abc""""
    val (state, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(resp(401, "Unauthorized", wwwAuth = List(challenge)))
    )
    assertEquals(
      state,
      Inviting(
        baseCtx.copy(
          cseq = cseqNumber(2),
          authAttempts = Map(AuthKind.Origin -> 1)
        )
      )
    )
    assert(actions.exists {
      case RetryInviteWithAuth(_, AuthKind.Origin, _) => true;
      case _                                          => false
    })
  }

  test("a 401 whose only challenge is not Digest is a final rejection") {
    val (state, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(
        resp(401, "Unauthorized", wwwAuth = List("Negotiate abc"))
      )
    )
    assertEquals(state, Failed(401, "Unauthorized"))
    assert(actions.contains(NotifyHangup(Rejected(401, "Unauthorized"))))
    assert(!actions.exists {
      case RetryInviteWithAuth(_, _, _) => true; case _ => false
    })
  }

  test("a 401 with no challenge at all is a final rejection") {
    val (state, _) =
      step(Inviting(baseCtx), ResponseReceived(resp(401, "Unauthorized")))
    assertEquals(state, Failed(401, "Unauthorized"))
  }

  test("a 401 offering SHA-256 then MD5 is answered with the MD5 challenge") {
    val (_, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(
        resp(
          401,
          "Unauthorized",
          wwwAuth = List(
            """Digest realm="r", nonce="n1", algorithm=SHA-256""",
            """Digest realm="r", nonce="n2", algorithm=MD5"""
          )
        )
      )
    )
    val nonces = actions.collect { case RetryInviteWithAuth(_, _, c) =>
      c.nonce.value
    }
    assertEquals(nonces, List("n2"))
  }

  test("a challenge offering only auth-int cannot be answered") {
    val (state, _) = step(
      Inviting(baseCtx),
      ResponseReceived(
        resp(
          401,
          "Unauthorized",
          wwwAuth = List("""Digest realm="r", nonce="n", qop="auth-int"""")
        )
      )
    )
    assertEquals(state, Failed(401, "Unauthorized"))
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

  test("Inviting + InviteTimeout → Failed, notifies timeout") {
    val (state, actions) =
      step(Inviting(baseCtx), TimerFired(CallTimer.InviteTimeout))
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

  test("Active + a stale timer expiry → no state change, no actions") {
    val (state, actions) =
      step(active, TimerFired(CallTimer.InviteTimeout))
    assertEquals(state, active)
    assertEquals(actions, Nil)
  }

  // ---- ByeSent ----------------------------------------------------------------

  test("ByeSent + the BYE's 200 OK → Closed(LocalHangup), notifies hangup") {
    val (state, actions) = step(
      ByeSent(active.dialog),
      ResponseReceived(resp(200, "OK", method = SipMethod.Bye))
    )
    assertEquals(state, Closed(LocalHangup))
    assert(actions.contains(NotifyHangup(LocalHangup)))
  }

  test("ByeSent + a failure response to the BYE also ends the call") {
    val (state, _) = step(
      ByeSent(active.dialog),
      ResponseReceived(resp(481, "Does Not Exist", method = SipMethod.Bye))
    )
    assertEquals(state, Closed(LocalHangup))
  }

  test(
    "ByeSent + a retransmitted 200 to the INVITE is ACKed, not taken for the BYE's answer"
  ) {
    val (state, actions) =
      step(ByeSent(active.dialog), ResponseReceived(resp(200, "OK")))
    assertEquals(state, ByeSent(active.dialog))
    assertEquals(actions, List(SendAck(active.dialog, None)))
  }

  test(
    "ByeSent + the BYE timing out or failing to send → Closed(LocalHangup)"
  ) {
    List(TransactionFailure.TimedOut, TransactionFailure.TransportFailed)
      .foreach { f =>
        val (state, actions) =
          step(ByeSent(active.dialog), TransactionFailed(f))
        assertEquals(state, Closed(LocalHangup))
        assert(actions.contains(NotifyHangup(LocalHangup)))
      }
  }

  // ---- Authentication retries ---------------------------------------------

  private val digest      = """Digest realm="sip.example.com", nonce="abc""""
  private val staleDigest =
    """Digest realm="sip.example.com", nonce="def", stale=true"""

  private def isRetry(a: SipAction): Boolean = a match {
    case RetryInviteWithAuth(_, _, _) => true
    case _                            => false
  }

  private def unauthorized(challenge: String) =
    ResponseReceived(resp(401, "Unauthorized", wwwAuth = List(challenge)))

  test(
    "a second 401 after we answered is final: wrong credentials do not loop"
  ) {
    val answered = baseCtx.copy(authAttempts = Map(AuthKind.Origin -> 1))
    val (state, actions) = step(Inviting(answered), unauthorized(digest))
    assertEquals(state, Failed(401, "Unauthorized"))
    assert(actions.contains(NotifyHangup(Rejected(401, "Unauthorized"))))
    assert(!actions.exists(isRetry))
    assert(actions.contains(Timer(TimerAction.Cancel(CallTimer.InviteTimeout))))
  }

  test("a stale nonce earns one more answer") {
    val answered = baseCtx.copy(authAttempts = Map(AuthKind.Origin -> 1))
    val (state, actions) = step(Inviting(answered), unauthorized(staleDigest))
    assert(actions.exists(isRetry))
    state match {
      case Inviting(ctx) =>
        assertEquals(ctx.authAttempts, Map(AuthKind.Origin -> 2))
        assertEquals(ctx.cseq, cseqNumber(2))
      case other => fail(s"expected Inviting, got $other")
    }
  }

  test("a stale nonce twice in a row is final") {
    val answered = baseCtx.copy(authAttempts = Map(AuthKind.Origin -> 2))
    val (state, actions) = step(Inviting(answered), unauthorized(staleDigest))
    assertEquals(state, Failed(401, "Unauthorized"))
    assert(!actions.exists(isRetry))
  }

  test("stale=true on the very first challenge is answered once, not twice") {
    val (state, actions) = step(Inviting(baseCtx), unauthorized(staleDigest))
    assert(actions.exists(isRetry))
    state match {
      case Inviting(ctx) =>
        assertEquals(ctx.authAttempts, Map(AuthKind.Origin -> 1))
      case other => fail(s"expected Inviting, got $other")
    }
  }

  test("a proxy challenge after we answered the server is a different party") {
    val answered     = baseCtx.copy(authAttempts = Map(AuthKind.Origin -> 1))
    val (_, actions) = step(
      Inviting(answered),
      ResponseReceived(
        resp(407, "Proxy Auth Required", proxyAuth = List(digest))
      )
    )
    assert(actions.exists {
      case RetryInviteWithAuth(_, AuthKind.Proxy, _) => true
      case _                                         => false
    })
  }

  test("a second 407 is final just like a second 401") {
    val answered   = baseCtx.copy(authAttempts = Map(AuthKind.Proxy -> 1))
    val (state, _) = step(
      Inviting(answered),
      ResponseReceived(
        resp(407, "Proxy Auth Required", proxyAuth = List(digest))
      )
    )
    assertEquals(state, Failed(407, "Proxy Auth Required"))
  }

  // ---- Timers -----------------------------------------------------------------

  private val stopInvite = Timer(TimerAction.Cancel(CallTimer.InviteTimeout))

  test("answering stops the invite timer") {
    val (_, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(resp(200, "OK", to = "<sip:callee@host>;tag=rtag"))
    )
    assert(actions.contains(stopInvite))
  }

  test("a rejection stops the invite timer") {
    val (_, actions) =
      step(Inviting(baseCtx), ResponseReceived(resp(486, "Busy Here")))
    assert(actions.contains(stopInvite))
  }

  test("cancelling before the answer stops the invite timer") {
    val (_, actions) = step(Inviting(baseCtx), HangupRequested)
    assert(actions.contains(stopInvite))
  }

  test(
    "an authenticated retry restarts the invite timer with the call's timeout"
  ) {
    val ctx          = baseCtx.copy(inviteTimeout = 7.seconds)
    val (_, actions) = step(
      Inviting(ctx),
      ResponseReceived(
        resp(
          401,
          "Unauthorized",
          wwwAuth = List("""Digest realm="r", nonce="n"""")
        )
      )
    )
    assert(
      actions.contains(
        Timer(TimerAction.Start(CallTimer.InviteTimeout, 7.seconds))
      )
    )
  }

  test("hanging up an active call sends a BYE and stops the media") {
    val (_, actions) = step(active, HangupRequested)
    assert(actions.contains(StopMedia))
    assert(actions.exists { case SendBye(_) => true; case _ => false })
    assert(!actions.exists { case Timer(_) => true; case _ => false })
  }

  // ---- Retransmitted 2xx and transaction failures --------------------------

  test("Active + a retransmitted 200 to the INVITE → ACK again, nothing else") {
    val (state, actions) =
      step(
        active,
        ResponseReceived(resp(200, "OK", to = "<sip:callee@host>;tag=rtag"))
      )
    assertEquals(state, active)
    assertEquals(actions, List(SendAck(active.dialog, None)))
  }

  test("Active + a 200 that answers something else is ignored") {
    val (state, actions) = step(
      active,
      ResponseReceived(resp(200, "OK", method = SipMethod.Cancel))
    )
    assertEquals(state, active)
    assertEquals(actions, Nil)
  }

  test(
    "Inviting + the INVITE transaction timing out → Failed(408), notifies timeout"
  ) {
    val (state, actions) = step(
      Inviting(baseCtx),
      TransactionFailed(TransactionFailure.TimedOut)
    )
    assertEquals(state, Failed(408, "Request Timeout"))
    assert(actions.contains(NotifyHangup(TimedOut)))
    assert(actions.contains(Timer(TimerAction.Cancel(CallTimer.InviteTimeout))))
  }

  test(
    "Inviting or Ringing + a transport failure → Failed, notifies the transport failure"
  ) {
    val ringing = step(
      Inviting(baseCtx),
      ResponseReceived(resp(180, "Ringing", to = "<sip:callee@host>;tag=rtag"))
    )._1
    List(Inviting(baseCtx), ringing).foreach { from =>
      val (state, actions) =
        step(from, TransactionFailed(TransactionFailure.TransportFailed))
      assertEquals(state, Failed(503, "Transport failure"))
      assert(actions.contains(NotifyHangup(TransportFailure)))
    }
  }

  test(
    "a rejection sends no ACK of its own: the transaction layer ACKs failures"
  ) {
    val (_, actions) =
      step(Inviting(baseCtx), ResponseReceived(resp(486, "Busy Here")))
    assertEquals(
      actions,
      List(stopInvite, NotifyHangup(Rejected(486, "Busy Here")))
    )
  }

  // ---- CANCEL race ------------------------------------------------------------

  test("Closed(LocalHangup) + the 200 to our CANCEL is not a late answer") {
    val (state, actions) = step(
      Closed(LocalHangup),
      ResponseReceived(resp(200, "OK", method = SipMethod.Cancel))
    )
    assertEquals(state, Closed(LocalHangup))
    assertEquals(actions, Nil)
  }

  test(
    "Closed(LocalHangup) + the 487 that ends a cancelled INVITE → nothing to do"
  ) {
    assertEquals(
      step(
        Closed(LocalHangup),
        ResponseReceived(resp(487, "Request Terminated"))
      ),
      (Closed(LocalHangup), Nil)
    )
  }

  test("Closed(LocalHangup) + 200 OK → stays Closed, sends ACK and BYE") {
    val (state, actions) = step(
      Closed(LocalHangup),
      ResponseReceived(resp(200, "OK", to = "<sip:callee@host>;tag=rtag"))
    )
    assertEquals(state, Closed(LocalHangup))
    assert(actions.exists { case SendAck(_, None) => true; case _ => false })
    assert(actions.exists { case SendBye(_) => true; case _ => false })
  }

  test("late 200 OK after CANCEL builds the dialog from the response") {
    val (_, actions) = step(
      Closed(LocalHangup),
      ResponseReceived(resp(200, "OK", to = "<sip:callee@host>;tag=rtag"))
    )
    val dialog = actions.collectFirst { case SendBye(d) => d } match {
      case Some(d) => d
      case None    => fail(s"no SendBye in $actions")
    }
    assertEquals(dialog.localUri, uri("sip:user@realm"))
    assertEquals(dialog.localTag, tag("ltag"))
    assertEquals(dialog.remoteUri, uri("sip:callee@host"))
    assertEquals(dialog.remoteTag, Some(tag("rtag")))
    assertEquals(dialog.callId, callId("call@test"))
  }
}
