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

  /** A request from the remote party within the dialog `active` stands for:
    * their tag in From, ours in To.
    */
  private def req(
      method: SipMethod,
      body: String = "",
      number: Int = 2,
      to: String = "<sip:user@realm>;tag=ltag",
      from: String = "<sip:callee@host>;tag=rtag"
  ) =
    SipRequest(
      method,
      uri("sip:user@realm"),
      mkHeaders(cseq = cseq(number, method))
        .copy(from = nameAddr(from), to = nameAddr(to)),
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

  private def challengeChosen(algorithms: String*): List[String] = {
    val (_, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(
        resp(
          401,
          "Unauthorized",
          wwwAuth = algorithms.toList.map(a =>
            s"""Digest realm="r", nonce="$a", algorithm=$a"""
          )
        )
      )
    )
    actions.collect { case RetryInviteWithAuth(_, _, c) => c.nonce.value }
  }

  test(
    "a 401 offering MD5 then SHA-256 is answered with SHA-256, the stronger"
  ) {
    assertEquals(challengeChosen("MD5", "SHA-256"), List("SHA-256"))
    assertEquals(challengeChosen("SHA-256", "MD5"), List("SHA-256"))
  }

  test("an algorithm we do not support is skipped for one we do") {
    assertEquals(challengeChosen("SHA-512-256", "MD5"), List("MD5"))
    assertEquals(challengeChosen("SHA-512-256"), Nil)
  }

  test("a challenge offering only auth-int is answered, over the body") {
    val (state, actions) = step(
      Inviting(baseCtx),
      ResponseReceived(
        resp(
          401,
          "Unauthorized",
          wwwAuth = List("""Digest realm="r", nonce="n", qop="auth-int"""")
        )
      )
    )
    assert(state.isInstanceOf[Inviting])
    assert(actions.exists(isRetry))
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
      ResponseReceived(
        resp(
          200,
          "OK",
          to = "<sip:callee@host>;tag=rtag",
          method = SipMethod.Bye
        )
      )
    )
    assertEquals(state, Closed(LocalHangup))
    assert(actions.contains(NotifyHangup(LocalHangup)))
  }

  test("ByeSent + a failure response to the BYE also ends the call") {
    val (state, _) = step(
      ByeSent(active.dialog),
      ResponseReceived(
        resp(
          481,
          "Does Not Exist",
          to = "<sip:callee@host>;tag=rtag",
          method = SipMethod.Bye
        )
      )
    )
    assertEquals(state, Closed(LocalHangup))
  }

  test(
    "ByeSent + a retransmitted 200 to the INVITE is ACKed, not taken for the BYE's answer"
  ) {
    val (state, actions) =
      step(
        ByeSent(active.dialog),
        ResponseReceived(resp(200, "OK", to = "<sip:callee@host>;tag=rtag"))
      )
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

  // ---- Requests within the dialog --------------------------------------------

  private def replyOf(actions: List[SipAction]): List[Int] =
    actions.collect { case SendReply(_, status) => status.value }

  test("a request with the wrong tags is answered 481 and changes nothing") {
    List(
      req(SipMethod.Bye, to = "<sip:user@realm>;tag=other"),
      req(SipMethod.Bye, from = "<sip:callee@host>;tag=other"),
      req(SipMethod.Bye, to = "<sip:user@realm>")
    ).foreach { bad =>
      val (state, actions) = step(active, RequestReceived(bad))
      assertEquals(state, active)
      assertEquals(replyOf(actions), List(481))
    }
  }

  test(
    "a BYE that is not newer than the dialog's last request is answered 500"
  ) {
    val advanced =
      step(active, RequestReceived(req(SipMethod.Info, "5", number = 7)))._1
    List(7, 3, 1).foreach { n =>
      val (state, actions) =
        step(advanced, RequestReceived(req(SipMethod.Bye, number = n)))
      assertEquals(state, advanced)
      assertEquals(replyOf(actions), List(500))
    }
  }

  test(
    "an INFO advances the remote CSeq, so the same number again is refused"
  ) {
    val info             = req(SipMethod.Info, "5", number = 4)
    val (state, actions) = step(active, RequestReceived(info))
    assert(actions.contains(SendInfoOk(info)))
    assertEquals(replyOf(step(state, RequestReceived(info))._2), List(500))
    assertEquals(
      replyOf(
        step(state, RequestReceived(req(SipMethod.Info, "5", number = 5)))._2
      ),
      Nil
    )
  }

  test("any CSeq is acceptable for the first request, whatever its number") {
    assertEquals(
      replyOf(
        step(active, RequestReceived(req(SipMethod.Info, "5", number = 1)))._2
      ),
      Nil
    )
    assertEquals(
      replyOf(
        step(active, RequestReceived(req(SipMethod.Info, "5", number = 900)))._2
      ),
      Nil
    )
  }

  private def sdpBody(
      direction: String = "sendrecv",
      ip: String = "192.0.2.77",
      port: Int = 4002
  ): String =
    List(
      "v=0",
      s"o=- 1 1 IN IP4 $ip",
      "s=-",
      s"c=IN IP4 $ip",
      "t=0 0",
      s"m=audio $port RTP/AVP 0 101",
      "a=rtpmap:0 PCMU/8000",
      "a=rtpmap:101 telephone-event/8000",
      s"a=$direction"
    ).mkString("", "\r\n", "\r\n")

  private def withBody(r: SipRequest, body: String): SipRequest =
    r.copy(body = body)

  test("OPTIONS in a call is answered 200 and advances the CSeq") {
    val options          = req(SipMethod.Options, number = 3)
    val (state, actions) = step(active, RequestReceived(options))
    assertEquals(actions, List(SendOptionsOk(options)))
    assertEquals(replyOf(step(state, RequestReceived(options))._2), List(500))
  }

  test(
    "a re-INVITE with an SDP we can answer is answered 200 and moves the media"
  ) {
    val reinvite = withBody(
      req(SipMethod.Invite, number = 3),
      sdpBody(ip = "192.0.2.99", port = 5000)
    )
    val (_, actions) = step(active, RequestReceived(reinvite))
    actions match {
      case List(SendSessionOk(r, Some(offer))) =>
        assertEquals(r, reinvite)
        assertEquals(offer.port.value, 5000)
        assertEquals(offer.ip.toString, "192.0.2.99")
      case other => fail(s"unexpected $other")
    }
  }

  test("an SDP that is sendonly or inactive is a hold; sendrecv is not") {
    def hold(direction: String) =
      step(
        active,
        RequestReceived(
          withBody(req(SipMethod.Update, number = 3), sdpBody(direction))
        )
      )._2 match {
        case List(SendSessionOk(_, Some(offer))) => offer.isHold
        case other                               => fail(s"unexpected $other")
      }
    assert(hold("sendonly"))
    assert(hold("inactive"))
    assert(!hold("sendrecv"))
    assert(!hold("recvonly"))
  }

  test("an UPDATE without a body is a refresh: 200, no media change") {
    val update = req(SipMethod.Update, number = 3)
    assertEquals(
      step(active, RequestReceived(update))._2,
      List(SendSessionOk(update, None))
    )
  }

  test("a re-INVITE whose SDP we cannot answer is refused 488") {
    val noPcmu = sdpBody().replace("RTP/AVP 0 101", "RTP/AVP 18")
    assertEquals(
      replyOf(
        step(
          active,
          RequestReceived(withBody(req(SipMethod.Invite, number = 3), noPcmu))
        )._2
      ),
      List(488)
    )
  }

  test(
    "a re-INVITE without SDP is answered with our offer, to be answered in the ACK"
  ) {
    val reinvite = req(SipMethod.Invite, number = 3)
    assertEquals(
      step(active, RequestReceived(reinvite))._2,
      List(SendOfferOk(reinvite))
    )
  }

  test(
    "a method we do not handle is answered 501 and still advances the CSeq"
  ) {
    val (state, actions) =
      step(active, RequestReceived(req(SipMethod.Register, number = 3)))
    assertEquals(replyOf(actions), List(501))
    assertEquals(
      replyOf(
        step(state, RequestReceived(req(SipMethod.Register, number = 3)))._2
      ),
      List(500)
    )
  }

  // ---- Our own session changes -----------------------------------------------

  private def reinviteResponse(code: Int, number: Int, body: String = "") = {
    val r = resp(code, "X", to = "<sip:callee@host>;tag=rtag", body = body)
    r.copy(headers = r.headers.copy(cseq = cseq(number, SipMethod.Invite)))
  }

  private lazy val holding: Reinviting =
    step(active, SessionChangeRequested(SessionChange.Hold)) match {
      case (r: Reinviting, _) => r
      case (other, _)         => fail(s"expected Reinviting, got $other")
    }

  test("a hold sends a re-INVITE with the next CSeq") {
    val (state, actions) =
      step(active, SessionChangeRequested(SessionChange.Hold))
    val next = active.dialog.copy(localCseq = active.dialog.localCseq.next)
    assertEquals(state, Reinviting(next, SessionChange.Hold))
    assertEquals(actions, List(SendReinvite(next, SessionChange.Hold)))
  }

  test("a 200 to the re-INVITE is ACKed and its answer applied") {
    val ok               = reinviteResponse(200, 2, sdpBody("recvonly"))
    val (state, actions) = step(holding, ResponseReceived(ok))
    assertEquals(state, Active(holding.dialog))
    assertEquals(actions, List(SendAck(holding.dialog, None), ApplyAnswer(ok)))
  }

  test("a provisional response changes nothing") {
    assertEquals(
      step(holding, ResponseReceived(reinviteResponse(100, 2))),
      (holding, Nil)
    )
  }

  test("a response to another request is not taken for the re-INVITE's") {
    assertEquals(
      step(holding, ResponseReceived(reinviteResponse(491, 1)))._1,
      holding
    )
  }

  test(
    "a 491 waits for the retry timer, then sends the re-INVITE again with a new CSeq"
  ) {
    val (waiting, actions) =
      step(holding, ResponseReceived(reinviteResponse(491, 2)))
    assertEquals(waiting, Retrying(holding.dialog, SessionChange.Hold))
    assertEquals(actions, List(ScheduleReinviteRetry(holding.dialog.role)))
    val (again, resent) = step(waiting, TimerFired(CallTimer.ReinviteRetry))
    val next = holding.dialog.copy(localCseq = holding.dialog.localCseq.next)
    assertEquals(again, Reinviting(next, SessionChange.Hold))
    assertEquals(resent, List(SendReinvite(next, SessionChange.Hold)))
  }

  test(
    "a 408 or 481 to the re-INVITE ends the call; any other refusal leaves it as it was"
  ) {
    List(408, 481).foreach { code =>
      val (state, actions) =
        step(holding, ResponseReceived(reinviteResponse(code, 2)))
      assertEquals(state, ByeSent(holding.dialog))
      assertEquals(actions, List(StopMedia, SendBye(holding.dialog)))
    }
    assertEquals(
      step(holding, ResponseReceived(reinviteResponse(488, 2))),
      (Active(holding.dialog), Nil)
    )
  }

  test("a re-INVITE that gets no answer ends the call") {
    val (state, _) =
      step(holding, TransactionFailed(TransactionFailure.TimedOut))
    assertEquals(state, ByeSent(holding.dialog))
  }

  test(
    "a re-INVITE from the remote while ours is pending gets 491, and ours stands"
  ) {
    val theirs           = req(SipMethod.Invite, sdpBody(), number = 3)
    val (state, actions) = step(holding, RequestReceived(theirs))
    assertEquals(state, holding)
    assertEquals(replyOf(actions), List(491))
  }

  test("while waiting to retry, the remote's re-INVITE is answered normally") {
    val waiting          = Retrying(holding.dialog, SessionChange.Hold)
    val theirs           = req(SipMethod.Invite, sdpBody(), number = 3)
    val (state, actions) = step(waiting, RequestReceived(theirs))
    assert(actions.exists {
      case SendSessionOk(_, Some(_)) => true; case _ => false
    })
    assertEquals(
      state,
      Retrying(holding.dialog.seen(cseqNumber(3)), SessionChange.Hold)
    )
  }

  test(
    "a BYE from the remote, or our hangup, ends the call while a re-INVITE is pending"
  ) {
    assert(
      step(holding, RequestReceived(req(SipMethod.Bye, number = 3)))._1
        .isInstanceOf[Closed]
    )
    assertEquals(step(holding, HangupRequested)._1, ByeSent(holding.dialog))
    val waiting          = Retrying(holding.dialog, SessionChange.Hold)
    val (state, actions) = step(waiting, HangupRequested)
    assertEquals(state, ByeSent(holding.dialog))
    assert(actions.contains(Timer(TimerAction.Cancel(CallTimer.ReinviteRetry))))
  }

  test("a second change request while one is pending is ignored") {
    assertEquals(
      step(holding, SessionChangeRequested(SessionChange.Resume)),
      (holding, Nil)
    )
  }

  // ---- A forked call -----------------------------------------------------------

  private def okFrom(
      tagValue: String,
      number: Int = 1,
      method: SipMethod = SipMethod.Invite
  ) = {
    val r =
      resp(200, "OK", to = s"<sip:callee@host>;tag=$tagValue", method = method)
    r.copy(headers = r.headers.copy(cseq = cseq(number, method)))
  }

  test("the first branch to answer is the call, whichever branches rang") {
    val ringing1 = step(
      Inviting(baseCtx),
      ResponseReceived(resp(180, "Ringing", to = "<sip:callee@host>;tag=a"))
    )._1
    val ringing2 = step(
      ringing1,
      ResponseReceived(resp(180, "Ringing", to = "<sip:callee@host>;tag=b"))
    )._1
    step(ringing2, ResponseReceived(okFrom("b"))) match {
      case (Active(dialog), actions) =>
        assertEquals(dialog.remoteTag, Some(tag("b")))
        assert(actions.contains(SendAck(dialog, None)))
      case (other, _) => fail(s"expected Active, got $other")
    }
  }

  test(
    "a 200 from another branch is ACKed and ended with a BYE, and the call stands"
  ) {
    List[CallState](active, ByeSent(active.dialog)).foreach { state =>
      val (next, actions) = step(state, ResponseReceived(okFrom("other")))
      assertEquals(next, state)
      actions match {
        case List(SendAck(ackDialog, None), SendBye(byeDialog)) =>
          assertEquals(ackDialog.remoteTag, Some(tag("other")))
          assertEquals(byeDialog, ackDialog)
        case other => fail(s"unexpected $other")
      }
    }
  }

  test("another branch's 200 is also ended while our re-INVITE is pending") {
    val (reinviting, _) =
      step(active, SessionChangeRequested(SessionChange.Hold))
    val (next, actions) = step(reinviting, ResponseReceived(okFrom("other")))
    assertEquals(next, reinviting)
    assert(actions.exists {
      case SendBye(d) => d.remoteTag == Some(tag("other")); case _ => false
    })
  }

  test(
    "a repeated 200 from our branch is ACKed with the CSeq of the INVITE it answers"
  ) {
    val (held, _) = step(active, SessionChangeRequested(SessionChange.Hold))
    val back      = step(held, ResponseReceived(okFrom("rtag", number = 2)))._1
    // The initial INVITE's 200 again, after the re-INVITE finished.
    val (state, actions) =
      step(back, ResponseReceived(okFrom("rtag", number = 1)))
    assertEquals(state, back)
    actions match {
      case List(SendAck(d, None)) => assertEquals(d.localCseq.value, 1)
      case other                  => fail(s"unexpected $other")
    }
  }

  test("the answer to a BYE sent to another branch does not end the call") {
    val (state, actions) = step(
      ByeSent(active.dialog),
      ResponseReceived(okFrom("other", number = 2, method = SipMethod.Bye))
    )
    assertEquals(state, ByeSent(active.dialog))
    assertEquals(actions, Nil)
  }

  // ---- A session change by UPDATE ----------------------------------------------

  private def updateResponse(code: Int, number: Int) = {
    val r = resp(
      code,
      "X",
      to = "<sip:callee@host>;tag=rtag",
      method = SipMethod.Update
    )
    r.copy(headers = r.headers.copy(cseq = cseq(number, SipMethod.Update)))
  }

  private lazy val updating: Reinviting =
    step(active, SessionChangeRequested(SessionChange.HoldByUpdate)) match {
      case (r: Reinviting, _) => r
      case (other, _)         => fail(s"expected Reinviting, got $other")
    }

  test("a hold by UPDATE sends an UPDATE with the next CSeq") {
    val next = active.dialog.copy(localCseq = active.dialog.localCseq.next)
    assertEquals(updating, Reinviting(next, SessionChange.HoldByUpdate))
    assertEquals(
      step(active, SessionChangeRequested(SessionChange.HoldByUpdate))._2,
      List(SendReinvite(next, SessionChange.HoldByUpdate))
    )
  }

  test("a 200 to the UPDATE applies its answer and is not ACKed") {
    val ok = updateResponse(200, updating.dialog.localCseq.value)
    assertEquals(
      step(updating, ResponseReceived(ok)),
      (Active(updating.dialog), List(ApplyAnswer(ok)))
    )
  }

  test("a response to an INVITE is not taken for the UPDATE's answer") {
    val other = reinviteResponse(200, updating.dialog.localCseq.value)
    assertEquals(step(updating, ResponseReceived(other))._1, updating)
  }

  test("a 491 to the UPDATE is retried with the same method") {
    val (waiting, _) = step(
      updating,
      ResponseReceived(updateResponse(491, updating.dialog.localCseq.value))
    )
    assertEquals(waiting, Retrying(updating.dialog, SessionChange.HoldByUpdate))
    val (again, resent) = step(waiting, TimerFired(CallTimer.ReinviteRetry))
    assert(resent.exists {
      case SendReinvite(_, SessionChange.HoldByUpdate) => true; case _ => false
    })
    assert(again.isInstanceOf[Reinviting])
  }
}
