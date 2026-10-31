package softphone4s.fsm

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.fsm.InboundAction.*
import softphone4s.fsm.InboundEvent.*
import softphone4s.fsm.InboundState.*
import softphone4s.model.*
import softphone4s.sdp.Capabilities
import softphone4s.transaction.TransactionConfig

import scala.concurrent.duration.*

class InboundCallFsmSuite extends FunSuite {

  private val config = TransactionConfig()

  private val sdp =
    "v=0\r\no=- 1 1 IN IP4 192.0.2.5\r\ns=-\r\nc=IN IP4 192.0.2.5\r\nt=0 0\r\n" +
      "m=audio 40000 RTP/AVP 0 101\r\na=rtpmap:0 PCMU/8000\r\n" +
      "a=rtpmap:101 telephone-event/8000\r\n"

  private def inviteWith(
      body: String = sdp,
      from: String = "<sip:alice@example.com>;tag=ft"
  ): SipRequest =
    SipRequest(
      SipMethod.Invite,
      uri("sip:test@192.0.2.10"),
      SipHeaders(
        callId = callId("in@host"),
        from = nameAddr(from),
        to = nameAddr("<sip:test@example.com>"),
        vias =
          NonEmptyList.one(via("SIP/2.0/UDP 192.0.2.5:5060;branch=z9hG4bKinv")),
        cseq = cseq(10, SipMethod.Invite),
        contacts = List(nameAddr("<sip:alice@192.0.2.5:5060>")),
        recordRoutes = List(
          nameAddr("<sip:p1.example.com;lr>"),
          nameAddr("<sip:p2.example.com;lr>")
        )
      ),
      body
    )

  private val invite = inviteWith()

  private def request(
      method: SipMethod,
      cseqNumber: Int,
      branch: String = "z9hG4bKother",
      body: String = ""
  ) =
    SipRequest(
      method,
      uri("sip:test@192.0.2.10"),
      SipHeaders(
        callId = callId("in@host"),
        from = nameAddr("<sip:alice@example.com>;tag=ft"),
        to = nameAddr("<sip:test@example.com>;tag=lt"),
        vias =
          NonEmptyList.one(via(s"SIP/2.0/UDP 192.0.2.5:5060;branch=$branch")),
        cseq = cseq(cseqNumber, method)
      ),
      body
    )

  private def start(i: SipRequest = invite) =
    InboundCallFsm.start(
      i,
      tag("lt"),
      config,
      60.seconds,
      Capabilities.Default
    )

  private def step(state: InboundState, event: InboundEvent) =
    InboundCallFsm.transition(event).run(state).value

  private val (offered, _) = start()
  private val ctx          = offered match {
    case Offered(c) => c
    case other      => fail(s"unexpected: $other")
  }
  private val ringing       = step(offered, UserRinging)._1
  private val (answered, _) = step(offered, UserAccept)
  private val active        = step(answered, AckReceived)._1

  private def timer(a: TimerAction[CallTimer]) = Timer(a)
  private def stop(t: CallTimer)               = Timer(TimerAction.Cancel(t))

  // ---- Starting --------------------------------------------------------------

  test("a call we can take is offered, with the answer timer running") {
    val (state, actions) = start()
    assert(state.isInstanceOf[Offered])
    assertEquals(
      actions,
      List(timer(TimerAction.Start(CallTimer.AnswerTimeout, 60.seconds)))
    )
  }

  test("an INVITE with no usable SDP is turned away with 488") {
    List(
      "",
      "v=0\r\n",
      sdp.replace("RTP/AVP 0 101", "RTP/AVP 18"),
      sdp.replace("c=IN IP4 192.0.2.5\r\n", "")
    )
      .foreach { body =>
        val (state, actions) = start(inviteWith(body))
        assertEquals(state, Closed)
        assertEquals(
          actions,
          List(
            SendInviteFailure(
              inviteWith(body),
              tag("lt"),
              FailureStatus.NotAcceptableHere
            ),
            NotifyHangup(TerminationReason.RejectedLocally)
          )
        )
      }
  }

  test("an INVITE without a From-tag is turned away with 400") {
    val (state, actions) = start(inviteWith(from = "<sip:alice@example.com>"))
    assertEquals(state, Closed)
    assert(actions.exists {
      case SendInviteFailure(_, _, s) => s == FailureStatus.BadRequest
      case _                          => false
    })
  }

  // ---- Ringing, accepting, rejecting ------------------------------------------

  test("ringing sends 180 once") {
    val (state, actions) = step(offered, UserRinging)
    assertEquals(state, Ringing(ctx))
    assertEquals(actions, List(SendRinging(ctx)))
    assertEquals(step(ringing, UserRinging), (ringing, Nil))
  }

  test(
    "accepting, from either state, answers 200 and starts media and the ACK timers"
  ) {
    List(offered, ringing).foreach { from =>
      val (state, actions) = step(from, UserAccept)
      assertEquals(state, Answered(ctx, 500.millis))
      assertEquals(
        actions,
        List(
          stop(CallTimer.AnswerTimeout),
          SendInviteOk(ctx),
          StartMedia(ctx.offer),
          timer(TimerAction.Start(CallTimer.RetransmitOk, 500.millis)),
          timer(TimerAction.Start(CallTimer.AckTimeout, 32.seconds))
        )
      )
    }
  }

  test(
    "the dialog formed for the answer takes the caller's tag, Contact and route"
  ) {
    val d = ctx.dialog
    assertEquals(d.role, DialogRole.Uas)
    assertEquals(d.localTag, tag("lt"))
    assertEquals(d.remoteTag, Some(tag("ft")))
    assertEquals(d.remoteTarget, uri("sip:alice@192.0.2.5:5060"))
    assertEquals(
      d.routeHeader.map(_.render),
      List("<sip:p1.example.com;lr>", "<sip:p2.example.com;lr>")
    )
    assertEquals(d.remoteCseq.map(_.value), Some(10))
  }

  test("rejecting sends the status and ends the call") {
    List(offered, ringing).foreach { from =>
      val (state, actions) = step(from, UserReject(FailureStatus.BusyHere))
      assertEquals(state, Closed)
      assertEquals(
        actions,
        List(
          stop(CallTimer.AnswerTimeout),
          SendInviteFailure(invite, tag("lt"), FailureStatus.BusyHere),
          NotifyHangup(TerminationReason.RejectedLocally)
        )
      )
    }
  }

  test("hanging up before answering declines with 603") {
    val (state, actions) = step(ringing, HangupRequested)
    assertEquals(state, Closed)
    assert(
      actions.contains(
        SendInviteFailure(invite, tag("lt"), FailureStatus.Decline)
      )
    )
  }

  test("an unanswered call is turned away with 480 when the timer runs out") {
    val (state, actions) = step(ringing, TimerFired(CallTimer.AnswerTimeout))
    assertEquals(state, Closed)
    assert(
      actions.contains(
        SendInviteFailure(
          invite,
          tag("lt"),
          FailureStatus.TemporarilyUnavailable
        )
      )
    )
    assert(actions.contains(NotifyHangup(TerminationReason.TimedOut)))
  }

  // ---- The caller cancels -------------------------------------------------------

  test("a CANCEL is answered 200 and the INVITE 487") {
    val cancel = request(SipMethod.Cancel, 10, branch = "z9hG4bKinv")
    List(offered, ringing).foreach { from =>
      val (state, actions) = step(from, RequestReceived(cancel))
      assertEquals(state, Closed)
      assertEquals(
        actions,
        List(
          stop(CallTimer.AnswerTimeout),
          Reply(cancel, 200, "OK"),
          SendInviteFailure(invite, tag("lt"), FailureStatus.RequestTerminated),
          NotifyHangup(TerminationReason.CancelledByCaller)
        )
      )
    }
  }

  test("a CANCEL for another transaction is answered 481 and changes nothing") {
    val cancel = request(SipMethod.Cancel, 10, branch = "z9hG4bKsomethingelse")
    val (state, actions) = step(ringing, RequestReceived(cancel))
    assertEquals(state, ringing)
    assertEquals(
      actions,
      List(Reply(cancel, 481, "Call/Transaction Does Not Exist"))
    )
  }

  test("a CANCEL that comes too late is answered 200 and changes nothing") {
    val cancel = request(SipMethod.Cancel, 10, branch = "z9hG4bKinv")
    List(answered, active).foreach { s =>
      val (state, actions) = step(s, RequestReceived(cancel))
      assertEquals(state, s)
      assertEquals(actions, List(Reply(cancel, 200, "OK")))
    }
  }

  // ---- Waiting for the ACK ----------------------------------------------------------

  test("the ACK establishes the call and stops the 200 timers") {
    val (state, actions) = step(answered, AckReceived)
    assertEquals(state, Active(ctx.dialog))
    assertEquals(
      actions,
      List(
        stop(CallTimer.RetransmitOk),
        stop(CallTimer.AckTimeout),
        NotifyEstablished
      )
    )
  }

  test("the 200 is repeated at 1, 2, 4 and then every 4 seconds") {
    val intervals  = List(1.second, 2.seconds, 4.seconds, 4.seconds)
    val (_, trace) =
      intervals.foldLeft((answered, List.empty[List[InboundAction]])) {
        case ((s, acc), _) =>
          val (next, actions) = step(s, TimerFired(CallTimer.RetransmitOk))
          (next, acc :+ actions)
      }
    trace.zip(intervals).foreach { (actions, next) =>
      assertEquals(
        actions,
        List(ResendOk, timer(TimerAction.Start(CallTimer.RetransmitOk, next)))
      )
    }
  }

  test("without an ACK in 64 * T1 the call is ended with a BYE") {
    val (state, actions) = step(answered, TimerFired(CallTimer.AckTimeout))
    assertEquals(state, Closed)
    assert(actions.contains(SendBye(ctx.dialog)))
    assert(actions.contains(StopMedia))
    assert(actions.contains(NotifyHangup(TerminationReason.TimedOut)))
  }

  // ---- An established call --------------------------------------------------------

  test("the caller's BYE ends the call and is answered") {
    val bye              = request(SipMethod.Bye, 11)
    val (state, actions) = step(active, RequestReceived(bye))
    assertEquals(state, Closed)
    assertEquals(
      actions,
      List(StopMedia, SendByeOk(bye), NotifyHangup(TerminationReason.RemoteBye))
    )
  }

  test("a BYE before the ACK also ends the call, and stops the 200 timers") {
    val (state, actions) =
      step(answered, RequestReceived(request(SipMethod.Bye, 11)))
    assertEquals(state, Closed)
    assert(actions.contains(stop(CallTimer.RetransmitOk)))
    assert(actions.contains(stop(CallTimer.AckTimeout)))
  }

  test("a request that is not newer than the last is answered 500") {
    List(10, 9, 1).foreach { n =>
      val (state, actions) =
        step(active, RequestReceived(request(SipMethod.Bye, n)))
      assertEquals(state, active)
      assertEquals(
        actions.map { case Reply(_, c, _) => c; case _ => -1 },
        List(500)
      )
    }
  }

  test(
    "an INFO carrying a digit is answered and delivered, and advances the CSeq"
  ) {
    val info             = request(SipMethod.Info, 11, body = "5")
    val (state, actions) = step(active, RequestReceived(info))
    assertEquals(actions, List(SendInfoOk(info), NotifyDtmf(DtmfDigit.D5)))
    assertEquals(state, Active(ctx.dialog.seen(cseqNumber(11))))
    // The same CSeq again is out of order.
    assertEquals(
      step(state, RequestReceived(info))._2.map {
        case Reply(_, c, _) => c; case _ => -1
      },
      List(500)
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

  test("before the ACK a re-INVITE or INFO is answered 501") {
    val reinvite = request(SipMethod.Invite, 11, body = sdpBody())
    assertEquals(
      step(answered, RequestReceived(reinvite))._2.collect {
        case Reply(_, c, _) => c
      },
      List(501)
    )
    val info = request(SipMethod.Info, 11, body = "5")
    assertEquals(
      step(answered, RequestReceived(info))._2,
      List(Reply(info, 501, "Not Implemented"))
    )
  }

  test(
    "once established OPTIONS is answered 200 and a re-INVITE moves or holds the media"
  ) {
    val options = request(SipMethod.Options, 11)
    assertEquals(
      step(active, RequestReceived(options))._2,
      List(SendOptionsOk(options))
    )
    val hold = request(SipMethod.Invite, 12, body = sdpBody("sendonly"))
    step(active, RequestReceived(hold))._2 match {
      case List(SendSessionOk(r, Some(offer))) =>
        assertEquals(r, hold)
        assert(offer.isHold)
      case other => fail(s"unexpected $other")
    }
    val (state, _) = step(active, RequestReceived(hold))
    assertEquals(state, Active(ctx.dialog.seen(cseqNumber(12))))
  }

  test("a re-INVITE without SDP is answered with our offer") {
    val reinvite = request(SipMethod.Invite, 11)
    assertEquals(
      step(active, RequestReceived(reinvite))._2,
      List(SendOfferOk(reinvite))
    )
  }

  test(
    "a re-INVITE we cannot answer is refused 488, an UPDATE without SDP is a refresh"
  ) {
    val bad = request(
      SipMethod.Invite,
      11,
      body = sdpBody().replace("RTP/AVP 0 101", "RTP/AVP 18")
    )
    assertEquals(
      step(active, RequestReceived(bad))._2,
      List(Reply(bad, 488, "Not Acceptable Here"))
    )
    val update = request(SipMethod.Update, 11)
    assertEquals(
      step(active, RequestReceived(update))._2,
      List(SendSessionOk(update, None))
    )
  }

  test("hanging up an established call sends a BYE and waits for its answer") {
    val (state, actions) = step(active, HangupRequested)
    assertEquals(state, ByeSent(ctx.dialog))
    assertEquals(actions, List(StopMedia, SendBye(ctx.dialog)))
    val bye = SipResponse(
      200,
      "OK",
      request(SipMethod.Bye, 2).headers
    )
    assertEquals(
      step(state, ResponseReceived(bye)),
      (Closed, List(NotifyHangup(TerminationReason.LocalHangup)))
    )
    assertEquals(
      step(state, TransactionFailed(TransactionFailure.TimedOut)),
      (Closed, List(NotifyHangup(TerminationReason.LocalHangup)))
    )
  }

  test("hanging up before the ACK also stops the 200 timers") {
    val (state, actions) = step(answered, HangupRequested)
    assertEquals(state, ByeSent(ctx.dialog))
    assert(actions.contains(stop(CallTimer.RetransmitOk)))
    assert(actions.contains(SendBye(ctx.dialog)))
  }

  test("a response to something other than our BYE does not end the BYE wait") {
    val (byeSent, _) = step(active, HangupRequested)
    val other = SipResponse(200, "OK", request(SipMethod.Cancel, 2).headers)
    assertEquals(step(byeSent, ResponseReceived(other)), (byeSent, Nil))
  }

  test("a transport failure before the answer ends the call") {
    val (state, actions) =
      step(ringing, TransactionFailed(TransactionFailure.TransportFailed))
    assertEquals(state, Closed)
    assert(actions.contains(NotifyHangup(TerminationReason.TransportFailure)))
  }

  test("once closed, nothing happens") {
    List(
      UserRinging,
      UserAccept,
      HangupRequested,
      AckReceived,
      RequestReceived(request(SipMethod.Bye, 11)),
      TimerFired(CallTimer.AnswerTimeout),
      TimerFired(CallTimer.RetransmitOk)
    ).foreach(e => assertEquals(step(Closed, e), (Closed, Nil)))
  }

  test("a request with the wrong tags is answered 481 and changes nothing") {
    def withTags(to: String, from: String) = {
      val r = request(SipMethod.Bye, 11)
      r.copy(headers = r.headers.copy(to = nameAddr(to), from = nameAddr(from)))
    }
    List(
      withTags(
        "<sip:test@example.com>;tag=other",
        "<sip:alice@example.com>;tag=ft"
      ),
      withTags(
        "<sip:test@example.com>;tag=lt",
        "<sip:alice@example.com>;tag=other"
      ),
      withTags("<sip:test@example.com>", "<sip:alice@example.com>;tag=ft")
    ).foreach { bad =>
      val (state, actions) = step(active, RequestReceived(bad))
      assertEquals(state, active)
      assertEquals(
        actions,
        List(Reply(bad, 481, "Call/Transaction Does Not Exist"))
      )
    }
  }

  // ---- Our own session changes -----------------------------------------------

  private def reinviteResponse(code: Int, number: Int) =
    SipResponse(code, "X", request(SipMethod.Invite, number).headers)

  private lazy val holding: Reinviting =
    step(active, SessionChangeRequested(SessionChange.Hold)) match {
      case (r: Reinviting, _) => r
      case (other, _)         => fail(s"expected Reinviting, got $other")
    }

  test("a hold sends a re-INVITE with the next CSeq") {
    val (state, actions) =
      step(active, SessionChangeRequested(SessionChange.Hold))
    val next = ctx.dialog.copy(localCseq = ctx.dialog.localCseq.next)
    assertEquals(state, Reinviting(next, SessionChange.Hold))
    assertEquals(actions, List(SendReinvite(next, SessionChange.Hold)))
  }

  test("a 200 to the re-INVITE is ACKed and its answer applied") {
    val ok = reinviteResponse(200, holding.dialog.localCseq.value)
    assertEquals(
      step(holding, ResponseReceived(ok)),
      (Active(holding.dialog), List(SendAck(holding.dialog), ApplyAnswer(ok)))
    )
  }

  test(
    "a 491 is retried after the timer, as the party that did not place the call"
  ) {
    val (waiting, actions) =
      step(
        holding,
        ResponseReceived(reinviteResponse(491, holding.dialog.localCseq.value))
      )
    assertEquals(waiting, Retrying(holding.dialog, SessionChange.Hold))
    assertEquals(actions, List(ScheduleReinviteRetry(DialogRole.Uas)))
    val (again, resent) = step(waiting, TimerFired(CallTimer.ReinviteRetry))
    assertEquals(again.getClass, classOf[Reinviting])
    assert(resent.exists {
      case SendReinvite(d, SessionChange.Hold) =>
        d.localCseq.value == holding.dialog.localCseq.value + 1;
      case _ => false
    })
  }

  test("a 408 or 481 ends the call; another refusal leaves it as it was") {
    List(408, 481).foreach { code =>
      val (state, _) =
        step(
          holding,
          ResponseReceived(
            reinviteResponse(code, holding.dialog.localCseq.value)
          )
        )
      assertEquals(state, ByeSent(holding.dialog))
    }
    assertEquals(
      step(
        holding,
        ResponseReceived(reinviteResponse(488, holding.dialog.localCseq.value))
      )._1,
      Active(holding.dialog)
    )
  }

  test(
    "the remote's re-INVITE while ours is pending gets 491; while waiting to retry it is answered"
  ) {
    val theirs = request(SipMethod.Invite, 11, body = sdpBody())
    assertEquals(
      step(holding, RequestReceived(theirs))._2,
      List(Reply(theirs, 491, "Request Pending"))
    )
    val waiting = Retrying(holding.dialog, SessionChange.Hold)
    assert(
      step(waiting, RequestReceived(theirs))._2.exists {
        case SendSessionOk(_, Some(_)) => true
        case _                         => false
      }
    )
  }

  test("hanging up while a re-INVITE is pending sends the BYE") {
    assertEquals(step(holding, HangupRequested)._1, ByeSent(holding.dialog))
  }

  test(
    "a change request is ignored unless the call stands with nothing pending"
  ) {
    assertEquals(
      step(holding, SessionChangeRequested(SessionChange.Resume)),
      (holding, Nil)
    )
    assertEquals(
      step(answered, SessionChangeRequested(SessionChange.Hold)),
      (answered, Nil)
    )
  }

  test("a 200 to our UPDATE applies its answer and is not ACKed") {
    val (updating, _) =
      step(active, SessionChangeRequested(SessionChange.HoldByUpdate))
    val dialog = updating match {
      case Reinviting(d, _) => d
      case other            => fail(s"unexpected $other")
    }
    val ok = SipResponse(
      200,
      "OK",
      request(SipMethod.Update, dialog.localCseq.value).headers
    )
    assertEquals(
      step(updating, ResponseReceived(ok)),
      (Active(dialog), List(ApplyAnswer(ok)))
    )
  }

  // ---- sips: requests (RFC 3261 §26.2.2) ---------------------------------------------------

  /** `invite` addressed to `target`, arrived over `transport`. */
  private def arrivedOver(target: String, transport: String): SipRequest =
    invite.copy(
      requestUri = uri(target),
      headers = invite.headers.copy(vias =
        NonEmptyList.one(
          via(s"SIP/2.0/$transport 192.0.2.5:5060;branch=z9hG4bKinv")
        )
      )
    )

  test("a sips: INVITE over UDP or TCP is turned away 403, not answered") {
    List("UDP", "TCP").foreach { transport =>
      val request          = arrivedOver("sips:test@192.0.2.10", transport)
      val (state, actions) = start(request)
      assertEquals(state, Closed)
      assertEquals(
        actions,
        List(
          SendInviteFailure(request, tag("lt"), FailureStatus.Forbidden),
          NotifyHangup(TerminationReason.RejectedLocally)
        )
      )
    }
  }

  test("a sips: INVITE over TLS is offered like any other") {
    assert(
      start(arrivedOver("sips:test@192.0.2.10", "TLS"))._1.isInstanceOf[Offered]
    )
  }

  test("a sip: INVITE is taken over any transport") {
    List("UDP", "TCP", "TLS").foreach { transport =>
      assert(
        start(arrivedOver("sip:test@192.0.2.10", transport))._1
          .isInstanceOf[Offered],
        transport
      )
    }
  }
}
