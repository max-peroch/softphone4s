package softphone4s.subscription

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.fsm.{TimerAction, TransactionFailure}
import softphone4s.model.*
import softphone4s.subscription.SubscriptionAction.*
import softphone4s.subscription.SubscriptionEvent.*
import softphone4s.subscription.SubscriptionFsmState.*

import scala.concurrent.duration.*

class SubscriptionFsmSuite extends FunSuite {

  private def expires(n: Int): Expires =
    Expires(n).fold(e => fail(e), identity)

  private val presence =
    EventHeader("presence").fold(e => fail(e), identity)

  private val context = SubscriptionContext(
    callId = callId("sub@host"),
    localTag = tag("ltag"),
    remoteTag = None,
    cseq = cseqNumber(1),
    event = presence,
    requested = expires(3600)
  )

  private def step(state: SubscriptionFsmState, event: SubscriptionEvent) =
    SubscriptionFsm.transition(event).run(state).value

  /** The state after the events, and everything they asked for, in order. */
  private def run(
      from: SubscriptionFsmState,
      events: SubscriptionEvent*
  ): (SubscriptionFsmState, List[SubscriptionAction]) =
    events.foldLeft((from, List.empty[SubscriptionAction])) {
      case ((state, done), event) =>
        val (next, actions) = step(state, event)
        (next, done ::: actions)
    }

  private def answer(
      code: Int,
      reason: String = "Reason",
      lifetime: Option[Int] = None,
      sequence: Int = 1,
      minimum: Option[Int] = None,
      www: List[String] = Nil,
      proxy: List[String] = Nil
  ): SubscriptionEvent =
    Response(
      SipResponse(
        code,
        reason,
        SipHeaders(
          callId = context.callId,
          from = nameAddr("<sip:alice@test.local>;tag=ltag"),
          to = nameAddr("<sip:bob@test.local>;tag=srv"),
          vias = NonEmptyList.one(
            via("SIP/2.0/UDP 192.0.2.10:5060;branch=z9hG4bKs")
          ),
          cseq = cseq(sequence, SipMethod.Subscribe),
          expires = lifetime.map(expires),
          minExpires = minimum.map(expires),
          wwwAuthenticate = www.map(challenge),
          proxyAuthenticate = proxy.map(challenge)
        )
      )
    )

  private def header(name: String, value: String) =
    ExtensionHeader(FieldName(name).fold(e => fail(e), identity), value)

  private val xml =
    MediaType.parse("application/pidf+xml").fold(e => fail(e), identity)

  /** A NOTIFY, as the request and as the program reads it. */
  private def notification(
      state: String,
      number: Int,
      eventValue: String = "presence",
      body: String = ""
  ): NotifyReceived = {
    val request = SipRequest(
      SipMethod.Notify,
      uri("sip:alice@192.0.2.10:5060"),
      SipHeaders(
        callId = context.callId,
        from = nameAddr("<sip:bob@test.local>;tag=srv"),
        to = nameAddr("<sip:alice@test.local>;tag=ltag"),
        vias =
          NonEmptyList.one(via("SIP/2.0/UDP 192.0.2.1:5060;branch=z9hG4bKn")),
        cseq = cseq(number, SipMethod.Notify),
        contentType = Option.when(body.nonEmpty)(xml),
        others = List(
          header("Event", eventValue),
          header("Subscription-State", state)
        )
      ),
      body
    )
    NotifyReceived(
      request,
      NotifyInfo.from(request).fold(e => fail(e.message), identity)
    )
  }

  private def okTo(n: NotifyReceived) = Respond(n.request, 200, "OK")

  private val digest      = """Digest realm="test.local", nonce="n1""""
  private val staleDigest =
    """Digest realm="test.local", nonce="n2", stale=true"""

  private def digestChallenge(text: String): DigestChallenge =
    AuthChallenge.best(List(challenge(text))).getOrElse(fail(text))

  // ---- Where each test begins ----------------------------------------------------------

  private val establishing = step(Idle, Start(context))._1

  private val withTag = context.copy(remoteTag = Some(tag("srv")))

  private val awaiting = AwaitingNotify(withTag, expires(600))

  private def subscribed(
      standing: Standing = Standing.Active,
      lifetime: Int = 600,
      ctx: SubscriptionContext = withTag.copy(remoteCseq = Some(1L))
  ) = Subscribed(ctx, standing, expires(lifetime))

  private def timersStarted(actions: List[SubscriptionAction]) =
    actions.collect { case Timer(TimerAction.Start(t, after)) => t -> after }

  private def timersCancelled(actions: List[SubscriptionAction]) =
    actions.collect { case Timer(TimerAction.Cancel(t)) => t }

  // ---- Subscribing ---------------------------------------------------------------------

  test("starting sends the SUBSCRIBE for the lifetime asked and reports it") {
    val (state, actions) = step(Idle, Start(context))
    assertEquals(state, Establishing(context, None))
    assertEquals(
      actions,
      List(
        Report(SubscriptionStatus.Subscribing),
        SendSubscribe(context, expires(3600), None)
      )
    )
  }

  test(
    "a 2xx gives the lifetime it grants, the notifier's tag, and then the first NOTIFY is awaited"
  ) {
    val (state, actions) = step(establishing, answer(200, lifetime = Some(600)))
    assertEquals(state, AwaitingNotify(withTag, expires(600)))
    assertEquals(
      timersStarted(actions),
      List(
        SubscriptionTimer.NotifyWait -> SubscriptionFsm.NotifyWait,
        SubscriptionTimer.Expiry     -> 600.seconds
      )
    )
  }

  test(
    "a 2xx that grants nothing in its Expires has the lifetime that was asked for"
  ) {
    val (state, _) = step(establishing, answer(200))
    assertEquals(state, AwaitingNotify(withTag, expires(3600)))
  }

  test("a provisional response changes nothing") {
    assertEquals(step(establishing, answer(100)), (establishing, Nil))
  }

  test("a 2xx that grants a lifetime of nothing is a refusal") {
    val (state, _) = step(establishing, answer(200, lifetime = Some(0)))
    assertEquals(state, Ended(Ending.Rejected(200, "granted no lifetime")))
  }

  test(
    "the first NOTIFY, active, makes the subscription active: answered, delivered, reported, timed"
  ) {
    val first = notification("active;expires=300", 1, body = "<presence/>")
    val (state, actions) = step(awaiting, first)
    assertEquals(
      state,
      Subscribed(
        withTag.copy(remoteCseq = Some(1L)),
        Standing.Active,
        expires(300)
      )
    )
    assertEquals(
      actions.take(3),
      List(
        okTo(first),
        Deliver(
          Notification(
            NotifyState.Active(Some(expires(300))),
            Some(xml),
            "<presence/>"
          )
        ),
        Report(SubscriptionStatus.Active(300.seconds))
      )
    )
    assertEquals(timersCancelled(actions), List(SubscriptionTimer.NotifyWait))
    assertEquals(
      timersStarted(actions),
      List(
        SubscriptionTimer.Refresh -> 150.seconds,
        SubscriptionTimer.Expiry  -> 300.seconds
      )
    )
  }

  test(
    "a NOTIFY that says pending makes it pending, and a later active makes it active"
  ) {
    val (pending, _) = step(awaiting, notification("pending;expires=300", 1))
    assertEquals(pending, subscribed(Standing.Pending, 300))
    val (active, actions) = step(pending, notification("active;expires=500", 2))
    assertEquals(
      active,
      subscribed(Standing.Active, 500, withTag.copy(remoteCseq = Some(2L)))
    )
    assert(actions.contains(Report(SubscriptionStatus.Active(500.seconds))))
  }

  test("a NOTIFY that gives no time left keeps the lifetime granted") {
    val (state, _) = step(awaiting, notification("active", 1))
    assertEquals(state, subscribed(Standing.Active, 600))
  }

  test(
    "a NOTIFY before the 2xx makes the dialog: it is answered and delivered, and the 2xx then finishes it"
  ) {
    val early = notification("active;expires=200", 1, body = "<presence/>")
    val (waiting, first) = step(establishing, early)
    assertEquals(
      waiting,
      Establishing(
        context.copy(remoteCseq = Some(1L)),
        Some(Early(Standing.Active, Some(expires(200))))
      )
    )
    assertEquals(first.take(2).head, okTo(early))
    val (state, actions) = step(waiting, answer(200, lifetime = Some(600)))
    // The NOTIFY's time left is the newer word; no wait for another NOTIFY.
    state match {
      case Subscribed(ctx, Standing.Active, left) =>
        assertEquals(left, expires(200))
        assertEquals(ctx.remoteTag, Some(tag("srv")))
      case other => fail(s"expected Subscribed: $other")
    }
    assert(actions.contains(Report(SubscriptionStatus.Active(200.seconds))))
  }

  test("a NOTIFY before the 2xx that says terminated ends it") {
    val (state, actions) =
      step(establishing, notification("terminated;reason=rejected", 1))
    assertEquals(state, Ended(Ending.Terminated(EndReason.Rejected, None)))
    assert(actions.exists { case Respond(_, 200, _) => true; case _ => false })
  }

  test("no NOTIFY after the 2xx is a failure to try again after") {
    val (state, actions) =
      step(awaiting, TimerFired(SubscriptionTimer.NotifyWait))
    assertEquals(state, Ended(Ending.Failed(SubscriptionFailure.NoNotify)))
    assert(
      actions.contains(
        Report(
          SubscriptionStatus.Ended(Ending.Failed(SubscriptionFailure.NoNotify))
        )
      )
    )
  }

  // ---- Refusals ----------------------------------------------------------------------------

  test("a refusal that is final ends it for good") {
    List(403, 404, 405, 489, 603).foreach { code =>
      assertEquals(
        step(establishing, answer(code, "No"))._1,
        Ended(Ending.Rejected(code, "No")),
        code.toString
      )
    }
  }

  test("a refusal that may pass is a failure to try again after") {
    List(408, 480, 500, 503, 504).foreach { code =>
      assertEquals(
        step(establishing, answer(code, "Later"))._1,
        Ended(Ending.Failed(SubscriptionFailure.Refused(code, "Later"))),
        code.toString
      )
    }
  }

  test(
    "no answer, and a transport that failed, are failures to try again after"
  ) {
    assertEquals(
      step(establishing, TransactionFailed(TransactionFailure.TimedOut))._1,
      Ended(Ending.Failed(SubscriptionFailure.TimedOut))
    )
    assertEquals(
      step(
        establishing,
        TransactionFailed(TransactionFailure.TransportFailed)
      )._1,
      Ended(Ending.Failed(SubscriptionFailure.TransportFailed))
    )
  }

  test(
    "a 423 asks for a longer lifetime: the SUBSCRIBE goes again with the minimum"
  ) {
    val (state, actions) = step(establishing, answer(423, minimum = Some(7200)))
    val next             = context.next.copy(requested = expires(7200))
    assertEquals(state, Establishing(next, None))
    assertEquals(actions, List(SendSubscribe(next, expires(7200), None)))
  }

  test("a 423 that asks for no more than we asked is a refusal") {
    assertEquals(
      step(establishing, answer(423, "Too Brief", minimum = Some(60)))._1,
      Ended(Ending.Rejected(423, "Too Brief"))
    )
  }

  test(
    "a challenge is answered once, with the next CSeq, and the retry is a SUBSCRIBE with the credentials"
  ) {
    val (state, actions) = step(establishing, answer(401, www = List(digest)))
    val next             = context.copy(
      cseq = context.cseq.next,
      authAttempts = Map(AuthKind.Origin -> 1),
      cached = Some((AuthKind.Origin, digestChallenge(digest)))
    )
    assertEquals(state, Establishing(next, None))
    assertEquals(
      actions,
      List(
        SendSubscribe(
          next,
          expires(3600),
          Some((AuthKind.Origin, digestChallenge(digest)))
        )
      )
    )
  }

  test("a proxy's challenge is answered as a proxy's") {
    val (_, actions) = step(establishing, answer(407, proxy = List(digest)))
    assert(actions.exists {
      case SendSubscribe(_, _, Some((AuthKind.Proxy, _))) => true
      case _                                              => false
    })
  }

  test(
    "a second challenge means the credentials were refused, unless the nonce only went stale"
  ) {
    val (again, _) = step(establishing, answer(401, www = List(digest)))
    assertEquals(
      step(again, answer(401, "Unauthorized", www = List(digest)))._1,
      Ended(Ending.Rejected(401, "Unauthorized"))
    )
    val (retry, actions) = step(again, answer(401, www = List(staleDigest)))
    assert(retry.isInstanceOf[Establishing])
    assert(actions.exists {
      case SendSubscribe(_, _, Some(_)) => true; case _ => false
    })
  }

  test("a challenge with nothing we can answer is a refusal") {
    assertEquals(
      step(establishing, answer(401, "Unauthorized"))._1,
      Ended(Ending.Rejected(401, "Unauthorized"))
    )
  }

  // ---- Refreshing --------------------------------------------------------------------------

  test(
    "at half the lifetime the subscription is refreshed, in the dialog, with the next CSeq"
  ) {
    val (state, actions) =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))
    val next = withTag.copy(remoteCseq = Some(1L)).next
    assertEquals(state, Refreshing(next, Standing.Active, expires(600)))
    assertEquals(actions, List(SendSubscribe(next, expires(3600), None)))
    assertEquals(next.remoteTag, Some(tag("srv")))
  }

  test(
    "a refresh that is accepted gives the new lifetime, and the timers start over"
  ) {
    val refreshing =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    val (state, actions) = step(refreshing, answer(200, lifetime = Some(1200)))
    state match {
      case Subscribed(_, Standing.Active, left) =>
        assertEquals(left, expires(1200))
      case other => fail(s"expected Subscribed: $other")
    }
    assertEquals(
      timersStarted(actions),
      List(
        SubscriptionTimer.Refresh -> 600.seconds,
        SubscriptionTimer.Expiry  -> 1200.seconds
      )
    )
  }

  test("a refresh answered 481 means the notifier has lost the dialog") {
    val refreshing =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    assertEquals(step(refreshing, answer(481))._1, Ended(Ending.DialogLost))
  }

  test(
    "a refresh that fails for a reason that may pass leaves it subscribed, to try again after a wait that grows"
  ) {
    val refreshing =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    val (first, a1) = step(refreshing, answer(503))
    first match {
      case Subscribed(ctx, _, _) => assertEquals(ctx.refreshFailures, 1)
      case other                 => fail(s"expected Subscribed: $other")
    }
    assertEquals(
      timersStarted(a1),
      List(SubscriptionTimer.Refresh -> 5.seconds)
    )
    val (second, a2) = run(
      first,
      TimerFired(SubscriptionTimer.Refresh),
      TransactionFailed(TransactionFailure.TimedOut)
    ) match {
      case (s, actions) => (s, actions)
    }
    second match {
      case Subscribed(ctx, _, _) => assertEquals(ctx.refreshFailures, 2)
      case other                 => fail(s"expected Subscribed: $other")
    }
    assert(timersStarted(a2).contains(SubscriptionTimer.Refresh -> 10.seconds))
  }

  test("the wait between refresh attempts doubles and stops at a minute") {
    assertEquals(
      (1 to 7).map(SubscriptionFsm.refreshBackoff).toList,
      List(
        5.seconds,
        10.seconds,
        20.seconds,
        40.seconds,
        60.seconds,
        60.seconds,
        60.seconds
      )
    )
  }

  test("a refresh that goes through after failures starts the count again") {
    val failed =
      subscribed(ctx = withTag.copy(remoteCseq = Some(1L), refreshFailures = 3))
    val refreshing = step(failed, TimerFired(SubscriptionTimer.Refresh))._1
    step(refreshing, answer(200, lifetime = Some(600)))._1 match {
      case Subscribed(ctx, _, _) => assertEquals(ctx.refreshFailures, 0)
      case other                 => fail(s"expected Subscribed: $other")
    }
  }

  test("a refresh that is refused for good ends the subscription") {
    val refreshing =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    assertEquals(
      step(refreshing, answer(403, "Forbidden"))._1,
      Ended(Ending.Rejected(403, "Forbidden"))
    )
  }

  test("a challenge to a refresh is answered in the refresh") {
    val refreshing =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    val (state, actions) = step(refreshing, answer(401, www = List(digest)))
    assert(state.isInstanceOf[Refreshing])
    assert(actions.exists {
      case SendSubscribe(_, _, Some(_)) => true; case _ => false
    })
  }

  test("a 423 to a refresh raises the lifetime asked") {
    val refreshing =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    val (_, actions) = step(refreshing, answer(423, minimum = Some(7200)))
    assert(actions.exists {
      case SendSubscribe(_, e, _) => e == expires(7200); case _ => false
    })
  }

  test(
    "the lifetime running out ends the subscription, whatever it was doing"
  ) {
    List(
      awaiting,
      subscribed(),
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    ).foreach { state =>
      assertEquals(
        step(state, TimerFired(SubscriptionTimer.Expiry))._1,
        Ended(Ending.Expired)
      )
    }
  }

  test(
    "a NOTIFY while a refresh is in flight is taken without starting the timers over"
  ) {
    val refreshing =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    val (state, actions) =
      step(refreshing, notification("active;expires=400", 2))
    assert(state.isInstanceOf[Refreshing])
    assertEquals(timersStarted(actions), Nil)
    assert(actions.contains(Report(SubscriptionStatus.Active(400.seconds))))
  }

  // ---- NOTIFYs -------------------------------------------------------------------------------

  test("a NOTIFY for another event is answered 481 and changes nothing") {
    val stray = notification("active", 2, eventValue = "dialog")
    assertEquals(
      step(subscribed(), stray),
      (
        subscribed(),
        List(Respond(stray.request, 481, "Subscription Does Not Exist"))
      )
    )
  }

  test("a NOTIFY for another id of the event is another subscription") {
    val stray = notification("active", 2, eventValue = "presence;id=2")
    assert(
      step(subscribed(), stray)._2
        .contains(Respond(stray.request, 481, "Subscription Does Not Exist"))
    )
  }

  test("a NOTIFY out of order is answered 500 and not delivered") {
    val old              = notification("active", 1)
    val (state, actions) = step(subscribed(), old)
    assertEquals(state, subscribed())
    assertEquals(
      actions,
      List(Respond(old.request, 500, "Server Internal Error"))
    )
  }

  test("NOTIFYs in order are taken, each once") {
    val (state, actions) = run(
      awaiting,
      notification("active", 1),
      notification("active", 2),
      notification("active", 3)
    )
    assertEquals(actions.count { case Deliver(_) => true; case _ => false }, 3)
    state match {
      case Subscribed(ctx, _, _) => assertEquals(ctx.remoteCseq, Some(3L))
      case other                 => fail(s"expected Subscribed: $other")
    }
  }

  test("a NOTIFY that says terminated ends the subscription, and says how") {
    val last = notification(
      "terminated;reason=probation;retry-after=30",
      2,
      body = "<presence/>"
    )
    val (state, actions) = step(subscribed(), last)
    val ending = Ending.Terminated(EndReason.Probation, Some(30.seconds))
    assertEquals(state, Ended(ending))
    assertEquals(
      actions.take(2),
      List(
        okTo(last),
        Deliver(
          Notification(
            NotifyState.Terminated(EndReason.Probation, Some(30.seconds)),
            Some(xml),
            "<presence/>"
          )
        )
      )
    )
    assertEquals(actions.last, Report(SubscriptionStatus.Ended(ending)))
    assertEquals(
      timersCancelled(actions).toSet,
      SubscriptionTimer.values.toSet
    )
  }

  test("a NOTIFY after the end, or before the start, is answered 481") {
    val stray = notification("active", 9)
    assert(
      step(Ended(Ending.Expired), stray)._2
        .contains(Respond(stray.request, 481, "Subscription Does Not Exist"))
    )
    assert(
      step(Idle, stray)._2.contains(
        Respond(stray.request, 481, "Subscription Does Not Exist")
      )
    )
  }

  // ---- Ending it -----------------------------------------------------------------------------

  test(
    "asking to end it sends a SUBSCRIBE for nothing, stops the timers and reports"
  ) {
    val (state, actions) = step(subscribed(), Stop)
    val next             = withTag.copy(remoteCseq = Some(1L)).next
    assertEquals(state, Unsubscribing(next))
    assertEquals(timersCancelled(actions).toSet, SubscriptionTimer.values.toSet)
    assert(actions.contains(Report(SubscriptionStatus.Leaving)))
    assert(actions.contains(SendSubscribe(next, Expires.Zero, None)))
  }

  test(
    "it can be ended while waiting for the first NOTIFY or while refreshing"
  ) {
    assert(step(awaiting, Stop)._1.isInstanceOf[Unsubscribing])
    val refreshing =
      step(subscribed(), TimerFired(SubscriptionTimer.Refresh))._1
    assert(step(refreshing, Stop)._1.isInstanceOf[Unsubscribing])
  }

  test("ending it before there is a dialog needs no request") {
    val (state, actions) = step(establishing, Stop)
    assertEquals(state, Ended(Ending.Unsubscribed))
    assert(!actions.exists {
      case SendSubscribe(_, _, _) => true; case _ => false
    })
  }

  test("ending it after an early NOTIFY made the dialog does send one") {
    val waiting = step(establishing, notification("active", 1))._1
    assert(step(waiting, Stop)._1.isInstanceOf[Unsubscribing])
  }

  test(
    "once the end is accepted the last NOTIFY is awaited, and it is the end"
  ) {
    val unsubscribing      = step(subscribed(), Stop)._1
    val (leaving, actions) =
      step(unsubscribing, answer(200, lifetime = Some(0), sequence = 2))
    assert(leaving.isInstanceOf[Leaving])
    assertEquals(
      timersStarted(actions),
      List(SubscriptionTimer.LeaveWait -> SubscriptionFsm.LeaveWait)
    )
    val last          = notification("terminated;reason=timeout", 2)
    val (done, reply) = step(leaving, last)
    assertEquals(done, Ended(Ending.Unsubscribed))
    assertEquals(reply.head, okTo(last))
  }

  test("if the last NOTIFY does not come, the wait for it is the end") {
    val leaving =
      step(
        step(subscribed(), Stop)._1,
        answer(200, lifetime = Some(0), sequence = 2)
      )._1
    assertEquals(
      step(leaving, TimerFired(SubscriptionTimer.LeaveWait))._1,
      Ended(Ending.Unsubscribed)
    )
  }

  test(
    "a NOTIFY while ending it is answered and not delivered, and is noted by its CSeq"
  ) {
    val unsubscribing    = step(subscribed(), Stop)._1
    val notice           = notification("active", 2)
    val (state, actions) = step(unsubscribing, notice)
    unsubscribing match {
      case Unsubscribing(ctx) =>
        assertEquals(state, Unsubscribing(ctx.copy(remoteCseq = Some(2L))))
      case other => fail(s"expected Unsubscribing: $other")
    }
    assertEquals(actions, List(okTo(notice)))
    // So that the same CSeq again is told for what it is.
    val (_, again) = step(state, notification("active", 2))
    assertEquals(
      again.map { case Respond(_, code, _) => code; case _ => -1 },
      List(500)
    )
  }

  test(
    "the last NOTIFY of a subscription we ended is answered, ends it, and is not delivered"
  ) {
    val leaving =
      step(
        step(subscribed(), Stop)._1,
        answer(200, lifetime = Some(0), sequence = 2)
      )._1
    val last =
      notification("terminated;reason=timeout", 2, body = "<presence/>")
    val (state, actions) = step(leaving, last)
    assertEquals(state, Ended(Ending.Unsubscribed))
    assert(!actions.exists { case Deliver(_) => true; case _ => false })
    assertEquals(actions.head, okTo(last))
  }

  test(
    "the answer to an earlier SUBSCRIBE, late, is not the answer to the one that ends it"
  ) {
    // Asked to end while the first SUBSCRIBE is still unanswered, but a NOTIFY
    // has come and given the dialog.
    val early        = notification("active", 1)
    val (noted, _)   = step(establishing, early)
    val (leaving, _) = step(noted, Stop)
    assert(leaving.isInstanceOf[Unsubscribing])
    // The 2xx to the first SUBSCRIBE arrives now: it is not the end accepted.
    assertEquals(step(leaving, answer(200, sequence = 1)), (leaving, Nil))
    // The 2xx to the one that ends it is.
    assert(
      step(leaving, answer(200, lifetime = Some(0), sequence = 2))._1
        .isInstanceOf[Leaving]
    )
  }

  test("a failure to end it still ends it") {
    val unsubscribing = step(subscribed(), Stop)._1
    assertEquals(
      step(unsubscribing, TransactionFailed(TransactionFailure.TimedOut))._1,
      Ended(Ending.Unsubscribed)
    )
    assertEquals(
      step(unsubscribing, answer(500, sequence = 2))._1,
      Ended(Ending.Unsubscribed)
    )
  }

  test("a challenge to the end is answered once, and then it is over") {
    val unsubscribing    = step(subscribed(), Stop)._1
    val (again, actions) =
      step(unsubscribing, answer(401, www = List(digest), sequence = 2))
    assert(again.isInstanceOf[Unsubscribing])
    assert(actions.exists {
      case SendSubscribe(_, e, Some(_)) => e.isZero; case _ => false
    })
    assertEquals(
      step(again, answer(401, www = List(digest), sequence = 3))._1,
      Ended(Ending.Unsubscribed)
    )
  }

  test(
    "a second Stop, and responses and timers that are late, change nothing"
  ) {
    val ended = Ended(Ending.Unsubscribed)
    assertEquals(step(ended, Stop), (ended, Nil))
    assertEquals(step(ended, answer(200)), (ended, Nil))
    assertEquals(
      step(ended, TimerFired(SubscriptionTimer.Refresh)),
      (ended, Nil)
    )
    assertEquals(step(Idle, answer(200)), (Idle, Nil))
    assertEquals(step(subscribed(), answer(200)), (subscribed(), Nil))
  }

  // ---- A whole life ----------------------------------------------------------------------------

  test("a subscription from the SUBSCRIBE to the end") {
    val (state, actions) = run(
      Idle,
      Start(context),
      answer(200, lifetime = Some(600)),
      notification("active;expires=600", 1, body = "<presence/>"),
      TimerFired(SubscriptionTimer.Refresh),
      answer(200, lifetime = Some(600)),
      notification("active;expires=600", 2, body = "<presence/>"),
      Stop,
      answer(200, lifetime = Some(0)),
      notification("terminated;reason=timeout", 3)
    )
    assertEquals(state, Ended(Ending.Unsubscribed))
    val sent = actions.collect { case SendSubscribe(c, e, _) =>
      (c.cseq.value, e.value)
    }
    assertEquals(sent, List((1, 3600), (2, 3600), (3, 0)))
    // The last NOTIFY, after we asked to leave, is answered and not delivered.
    assertEquals(actions.count { case Deliver(_) => true; case _ => false }, 2)
  }
}
