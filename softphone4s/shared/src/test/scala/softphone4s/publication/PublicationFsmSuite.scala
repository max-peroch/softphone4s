package softphone4s.publication

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.fsm.{TimerAction, TransactionFailure}
import softphone4s.model.*
import softphone4s.publication.PublicationAction.*
import softphone4s.publication.PublicationEvent.*
import softphone4s.publication.PublicationState.*
import softphone4s.subscription.EventHeader

import scala.concurrent.duration.*

class PublicationFsmSuite extends FunSuite {

  private def expires(n: Int): Expires =
    Expires(n).fold(e => fail(e), identity)

  private def etag(text: String): EntityTag =
    EntityTag.parse(text).fold(e => fail(e), identity)

  private val presence = EventHeader("presence").fold(e => fail(e), identity)

  private val pidf =
    MediaType.parse("application/pidf+xml").fold(e => fail(e), identity)

  private def doc(text: String) = PublicationBody(pidf, text)

  private val context = PublicationContext(
    callId = callId("pub@host"),
    localTag = tag("ltag"),
    cseq = cseqNumber(1),
    event = presence,
    requested = expires(3600),
    document = doc("one")
  )

  private def step(state: PublicationState, event: PublicationEvent) =
    PublicationFsm.transition(event).run(state).value

  private def run(
      from: PublicationState,
      events: PublicationEvent*
  ): (PublicationState, List[PublicationAction]) =
    events.foldLeft((from, List.empty[PublicationAction])) {
      case ((state, done), event) =>
        val (next, actions) = step(state, event)
        (next, done ::: actions)
    }

  private def answer(
      code: Int,
      reason: String = "Reason",
      tag: Option[String] = None,
      lifetime: Option[Int] = None,
      minimum: Option[Int] = None,
      www: List[String] = Nil
  ): PublicationEvent =
    Response(
      SipResponse(
        code,
        reason,
        SipHeaders(
          callId = context.callId,
          from = nameAddr("<sip:alice@test.local>;tag=ltag"),
          to = nameAddr("<sip:alice@test.local>;tag=srv"),
          vias = NonEmptyList.one(
            via("SIP/2.0/UDP 192.0.2.10:5060;branch=z9hG4bKp")
          ),
          cseq = cseq(1, SipMethod.Publish),
          expires = lifetime.map(expires),
          minExpires = minimum.map(expires),
          wwwAuthenticate = www.map(challenge),
          others = tag.toList.map(t => ExtensionHeader(FieldName.SipETag, t))
        )
      )
    )

  private def ok(tag: String, lifetime: Int = 3600) =
    answer(200, "OK", Some(tag), Some(lifetime))

  private val digest = """Digest realm="test.local", nonce="n1""""

  private def timersStarted(actions: List[PublicationAction]) =
    actions.collect { case Timer(TimerAction.Start(t, after)) => t -> after }

  private def timersCancelled(actions: List[PublicationAction]) =
    actions.collect { case Timer(TimerAction.Cancel(t)) => t }

  private def sent(actions: List[PublicationAction]) =
    actions.collect { case s: SendPublish => s }

  private def reports(actions: List[PublicationAction]) =
    actions.collect { case Report(s) => s }

  // ---- Where each test begins -------------------------------------------------------

  private val publishing = step(Idle, Start(context))._1

  private def published(
      ctx: PublicationContext = context,
      tag: String = "t1",
      lifetime: Int = 3600
  ) = Published(ctx, etag(tag), expires(lifetime))

  // ---- Publishing --------------------------------------------------------------------

  test("starting publishes the document with no tag, for the lifetime asked") {
    val (state, actions) = step(Idle, Start(context))
    assertEquals(state, Publishing(context))
    assertEquals(
      actions,
      List(
        Report(PublicationStatus.Publishing),
        SendPublish(context, expires(3600), None, Some(doc("one")), None)
      )
    )
  }

  test(
    "the 2xx gives the tag and the lifetime: refresh at half, lapse at the end"
  ) {
    val (state, actions) = step(publishing, ok("t1", 600))
    assertEquals(state, published(tag = "t1", lifetime = 600))
    assertEquals(
      reports(actions),
      List(PublicationStatus.Published(600.seconds))
    )
    assertEquals(
      timersStarted(actions),
      List(
        PublicationTimer.Refresh -> 300.seconds,
        PublicationTimer.Expiry  -> 600.seconds
      )
    )
  }

  test("a 2xx with no Expires means the lifetime asked for") {
    assertEquals(
      step(publishing, answer(200, "OK", Some("t1")))._1,
      published(tag = "t1", lifetime = 3600)
    )
  }

  test(
    "a 2xx the publisher cannot go on from ends it: no tag, a bad tag, no lifetime"
  ) {
    def failed(event: PublicationEvent) = step(publishing, event)._1
    assertEquals(
      failed(answer(200, "OK", None, Some(600))),
      Ended(
        PublicationEnding.Failed(
          PublicationFailure.BadResponse("no SIP-ETag in the 2xx")
        )
      )
    )
    assert(
      failed(answer(200, "OK", Some("not a token"), Some(600))) match {
        case Ended(
              PublicationEnding.Failed(PublicationFailure.BadResponse(_))
            ) =>
          true
        case _ => false
      }
    )
    assertEquals(
      failed(ok("t1", 0)),
      Ended(PublicationEnding.Rejected(200, "granted no lifetime"))
    )
  }

  test("a provisional response changes nothing") {
    assertEquals(step(publishing, answer(100)), (publishing, Nil))
  }

  // ---- Refreshing ----------------------------------------------------------------------

  test(
    "at half the lifetime the state is refreshed under its tag, with no body"
  ) {
    val (state, actions) =
      step(published(), TimerFired(PublicationTimer.Refresh))
    val next = context.next
    assertEquals(state, Refreshing(next, etag("t1"), expires(3600)))
    assertEquals(
      actions,
      List(SendPublish(next, expires(3600), Some(etag("t1")), None, None))
    )
    assertEquals(next.cseq, cseqNumber(2))
  }

  test("the tag the 2xx to a refresh gives is the one to use next") {
    val (state, _) = run(
      published(),
      TimerFired(PublicationTimer.Refresh),
      ok("t2", 1800)
    )
    state match {
      case Published(_, tag, lifetime) =>
        assertEquals((tag, lifetime), (etag("t2"), expires(1800)))
      case other => fail(s"expected Published: $other")
    }
  }

  test(
    "a refresh that gets no answer, or a 5xx, leaves it published and is tried again after a wait"
  ) {
    val refreshing = step(published(), TimerFired(PublicationTimer.Refresh))._1
    val (state, actions) =
      step(refreshing, TransactionFailed(TransactionFailure.TimedOut))
    state match {
      case Published(ctx, tag, _) =>
        assertEquals(tag, etag("t1"))
        assertEquals(ctx.refreshFailures, 1)
      case other => fail(s"expected Published: $other")
    }
    assertEquals(
      timersStarted(actions),
      List(PublicationTimer.Refresh -> 5.seconds)
    )
    // The next failure waits longer.
    val (again, later) = run(
      state,
      TimerFired(PublicationTimer.Refresh),
      answer(503, "Unavailable")
    )
    again match {
      case Published(ctx, _, _) => assertEquals(ctx.refreshFailures, 2)
      case other                => fail(s"expected Published: $other")
    }
    assertEquals(
      timersStarted(later).lastOption,
      Some(PublicationTimer.Refresh -> 10.seconds)
    )
  }

  test("a refresh that works after failures starts the count again") {
    val (state, _) = run(
      published(),
      TimerFired(PublicationTimer.Refresh),
      answer(503),
      TimerFired(PublicationTimer.Refresh),
      ok("t2")
    )
    state match {
      case Published(ctx, _, _) => assertEquals(ctx.refreshFailures, 0)
      case other                => fail(s"expected Published: $other")
    }
  }

  // ---- Changing -------------------------------------------------------------------------

  test("a new document is sent at once under the tag") {
    val (state, actions) = step(published(), Update(doc("two")))
    val next             = context.copy(document = doc("two")).next
    assertEquals(state, Changing(next, etag("t1"), expires(3600)))
    assertEquals(
      actions,
      List(
        SendPublish(
          next,
          expires(3600),
          Some(etag("t1")),
          Some(doc("two")),
          None
        )
      )
    )
  }

  test(
    "a document given while a request is in flight is sent once that is answered"
  ) {
    val (inFlight, none) = step(publishing, Update(doc("two")))
    assertEquals(none, Nil)
    inFlight match {
      case Publishing(ctx) =>
        assertEquals((ctx.document, ctx.dirty), (doc("two"), true))
      case other => fail(s"expected Publishing: $other")
    }
    val (state, actions) = step(inFlight, ok("t1"))
    state match {
      case Changing(ctx, tag, _) =>
        assertEquals(tag, etag("t1"))
        assertEquals(ctx.dirty, false)
      case other => fail(s"expected Changing: $other")
    }
    assertEquals(
      sent(actions).map(s => (s.tag, s.body)),
      List((Some(etag("t1")), Some(doc("two"))))
    )
  }

  test("the last of several documents given in flight is the one sent") {
    val (state, _) = run(
      publishing,
      Update(doc("two")),
      Update(doc("three")),
      ok("t1")
    )
    state match {
      case Changing(ctx, _, _) => assertEquals(ctx.document, doc("three"))
      case other               => fail(s"expected Changing: $other")
    }
  }

  test(
    "a change that fails is owed: the next try, after a wait, sends it and not a refresh"
  ) {
    val changing     = step(published(), Update(doc("two")))._1
    val (waiting, _) = step(changing, answer(503))
    waiting match {
      case Published(ctx, _, _) => assertEquals(ctx.dirty, true)
      case other                => fail(s"expected Published: $other")
    }
    val (state, actions) = step(waiting, TimerFired(PublicationTimer.Refresh))
    assert(state.isInstanceOf[Changing], state.toString)
    assertEquals(sent(actions).map(_.body), List(Some(doc("two"))))
  }

  // ---- The state lapses -----------------------------------------------------------------

  test(
    "a 412 says the notifier does not hold the state: all of it is published again, with no tag"
  ) {
    val refreshing = step(published(), TimerFired(PublicationTimer.Refresh))._1
    val (state, actions) =
      step(refreshing, answer(412, "Conditional Request Failed"))
    state match {
      case Publishing(ctx) => assertEquals(ctx.cseq, cseqNumber(3))
      case other           => fail(s"expected Publishing: $other")
    }
    assertEquals(
      sent(actions).map(s => (s.tag, s.body)),
      List((None, Some(doc("one"))))
    )
    assertEquals(reports(actions), List(PublicationStatus.Publishing))
  }

  test("a 412 to a PUBLISH that had no tag is a refusal") {
    assertEquals(
      step(publishing, answer(412, "Conditional Request Failed"))._1,
      Ended(PublicationEnding.Rejected(412, "Conditional Request Failed"))
    )
  }

  test(
    "when the lifetime runs out the state is published again, with the latest document"
  ) {
    val (state, actions) = run(
      published(),
      Update(doc("two")),
      ok("t2"),
      TimerFired(PublicationTimer.Expiry)
    )
    assert(state.isInstanceOf[Publishing], state.toString)
    assertEquals(
      sent(actions).lastOption.map(s => (s.tag, s.body)),
      Some((None, Some(doc("two"))))
    )
  }

  // ---- A lifetime too brief --------------------------------------------------------------

  test(
    "a 423 raises the lifetime asked for, and the first PUBLISH goes again"
  ) {
    val (state, actions) =
      step(publishing, answer(423, "Interval Too Brief", minimum = Some(7200)))
    state match {
      case Publishing(ctx) =>
        assertEquals((ctx.requested, ctx.cseq), (expires(7200), cseqNumber(2)))
      case other => fail(s"expected Publishing: $other")
    }
    assertEquals(
      sent(actions).map(s => (s.expires, s.tag)),
      List((expires(7200), None))
    )
  }

  test("a 423 to a refresh goes again under the tag") {
    val refreshing = step(published(), TimerFired(PublicationTimer.Refresh))._1
    val (state, actions) =
      step(refreshing, answer(423, "Interval Too Brief", minimum = Some(7200)))
    assert(state.isInstanceOf[Refreshing], state.toString)
    assertEquals(
      sent(actions).map(s => (s.expires, s.tag, s.body)),
      List((expires(7200), Some(etag("t1")), None))
    )
  }

  test("a 423 that asks for less than we did, or says nothing, is a refusal") {
    assertEquals(
      step(
        publishing,
        answer(423, "Interval Too Brief", minimum = Some(60))
      )._1,
      Ended(PublicationEnding.Rejected(423, "Interval Too Brief"))
    )
    assertEquals(
      step(publishing, answer(423, "Interval Too Brief"))._1,
      Ended(PublicationEnding.Rejected(423, "Interval Too Brief"))
    )
  }

  // ---- Challenges -------------------------------------------------------------------------

  test(
    "a challenge is answered once, with the same request and the next CSeq"
  ) {
    val (state, actions) = step(publishing, answer(401, www = List(digest)))
    state match {
      case Publishing(ctx) =>
        assertEquals(ctx.cseq, cseqNumber(2))
        assertEquals(ctx.authAttempts, Map(AuthKind.Origin -> 1))
      case other => fail(s"expected Publishing: $other")
    }
    assertEquals(
      sent(actions).map(s => (s.tag, s.body, s.authorization.map(_._1))),
      List((None, Some(doc("one")), Some(AuthKind.Origin)))
    )
    // A second challenge means the credentials were refused.
    assertEquals(
      step(state, answer(401, www = List(digest)))._1,
      Ended(PublicationEnding.Rejected(401, "Reason"))
    )
  }

  test("a challenge to a refresh is answered with the tag") {
    val refreshing = step(published(), TimerFired(PublicationTimer.Refresh))._1
    val (state, actions) = step(refreshing, answer(401, www = List(digest)))
    assert(state.isInstanceOf[Refreshing], state.toString)
    assertEquals(
      sent(actions).map(s => (s.tag, s.body)),
      List((Some(etag("t1")), None))
    )
  }

  test(
    "the credentials that worked are used in the next request without asking again"
  ) {
    val (state, _) = run(publishing, answer(401, www = List(digest)), ok("t1"))
    val (_, actions) = step(state, TimerFired(PublicationTimer.Refresh))
    assert(sent(actions).forall(_.authorization.isDefined), actions.toString)
  }

  // ---- Refusals and failures of the first PUBLISH ------------------------------------------

  test("the first PUBLISH refused for good is final") {
    assertEquals(
      step(publishing, answer(489, "Bad Event"))._1,
      Ended(PublicationEnding.Rejected(489, "Bad Event"))
    )
    assertEquals(
      step(publishing, answer(403, "Forbidden"))._1,
      Ended(PublicationEnding.Rejected(403, "Forbidden"))
    )
  }

  test(
    "the first PUBLISH answered with a status that may pass, or not at all, is a failure to try again"
  ) {
    assertEquals(
      step(publishing, answer(503, "Service Unavailable"))._1,
      Ended(
        PublicationEnding.Failed(
          PublicationFailure.Refused(503, "Service Unavailable")
        )
      )
    )
    assertEquals(
      step(publishing, TransactionFailed(TransactionFailure.TimedOut))._1,
      Ended(PublicationEnding.Failed(PublicationFailure.TimedOut))
    )
    assertEquals(
      step(
        publishing,
        TransactionFailed(TransactionFailure.TransportFailed)
      )._1,
      Ended(PublicationEnding.Failed(PublicationFailure.TransportFailed))
    )
  }

  // ---- Removing ----------------------------------------------------------------------------

  test(
    "removing sends Expires 0 under the tag, with no body, and stops the timers"
  ) {
    val (state, actions) = step(published(), Stop)
    val next             = context.next
    assertEquals(state, Removing(next, etag("t1")))
    assertEquals(
      sent(actions),
      List(SendPublish(next, Expires.Zero, Some(etag("t1")), None, None))
    )
    assertEquals(reports(actions), List(PublicationStatus.Removing))
    assertEquals(
      timersCancelled(actions).toSet,
      Set(PublicationTimer.Refresh, PublicationTimer.Expiry)
    )
  }

  test(
    "once removed it is over, and so it is if the removal is refused or lost: the state lapses anyway"
  ) {
    val removing = step(published(), Stop)._1
    assertEquals(
      step(removing, answer(200, "OK", Some("t1"), Some(0)))._1,
      Ended(PublicationEnding.Removed)
    )
    List(
      answer(412, "Conditional Request Failed"),
      answer(503),
      answer(403),
      TransactionFailed(TransactionFailure.TimedOut)
    ).foreach(event =>
      assertEquals(
        step(removing, event)._1,
        Ended(PublicationEnding.Removed),
        event.toString
      )
    )
  }

  test("a challenge to the removal is answered once") {
    val removing         = step(published(), Stop)._1
    val (state, actions) = step(removing, answer(401, www = List(digest)))
    assert(state.isInstanceOf[Removing], state.toString)
    assertEquals(
      sent(actions).map(s => (s.expires, s.tag)),
      List((Expires.Zero, Some(etag("t1"))))
    )
    assertEquals(
      step(state, answer(401, www = List(digest)))._1,
      Ended(PublicationEnding.Removed)
    )
  }

  test("with nothing published, stopping is the end, and says so") {
    val (state, actions) = step(Idle, Stop)
    assertEquals(state, Ended(PublicationEnding.Removed))
    assertEquals(sent(actions), Nil)
    assertEquals(
      reports(actions),
      List(PublicationStatus.Ended(PublicationEnding.Removed))
    )
  }

  test(
    "stopping while the first PUBLISH is in flight removes the state as soon as its tag is known"
  ) {
    val (waiting, none) = step(publishing, Stop)
    assertEquals(none, Nil)
    val (state, actions) = step(waiting, ok("t1"))
    assert(state.isInstanceOf[Removing], state.toString)
    assertEquals(
      sent(actions).map(s => (s.expires, s.tag)),
      List((Expires.Zero, Some(etag("t1"))))
    )
  }

  test("stopping while a request is in flight and it fails is the end") {
    val refreshing = step(published(), TimerFired(PublicationTimer.Refresh))._1
    val waiting    = step(refreshing, Stop)._1
    assertEquals(
      step(waiting, answer(503))._1,
      Ended(PublicationEnding.Removed)
    )
    assertEquals(
      step(step(publishing, Stop)._1, answer(403))._1,
      Ended(PublicationEnding.Removed)
    )
    assertEquals(
      step(
        step(published(), TimerFired(PublicationTimer.Refresh))._1,
        Stop
      )._1 match {
        case s => step(s, TimerFired(PublicationTimer.Expiry))._1
      },
      Ended(PublicationEnding.Removed)
    )
  }

  // ---- What changes nothing -----------------------------------------------------------------

  test(
    "responses, timers and documents that are late or out of place change nothing"
  ) {
    val ended    = Ended(PublicationEnding.Removed)
    val removing = step(published(), Stop)._1
    assertEquals(step(ended, Stop), (ended, Nil))
    assertEquals(step(ended, ok("t1")), (ended, Nil))
    assertEquals(step(ended, Update(doc("x"))), (ended, Nil))
    assertEquals(
      step(ended, TimerFired(PublicationTimer.Refresh)),
      (ended, Nil)
    )
    assertEquals(step(Idle, ok("t1")), (Idle, Nil))
    assertEquals(step(Idle, Update(doc("x"))), (Idle, Nil))
    assertEquals(step(published(), ok("t1")), (published(), Nil))
    assertEquals(step(removing, Update(doc("x"))), (removing, Nil))
    assertEquals(step(removing, Stop), (removing, Nil))
    assertEquals(
      step(publishing, TimerFired(PublicationTimer.Refresh)),
      (publishing, Nil)
    )
    assertEquals(
      step(publishing, TimerFired(PublicationTimer.Expiry)),
      (publishing, Nil)
    )
  }

  // ---- A whole life --------------------------------------------------------------------------

  test("a publication from the first PUBLISH to its removal") {
    val (state, actions) = run(
      Idle,
      Start(context),
      ok("t1", 600),
      TimerFired(PublicationTimer.Refresh),
      ok("t2", 600),
      Update(doc("two")),
      ok("t3", 600),
      Stop,
      answer(200, "OK", Some("t3"), Some(0))
    )
    assertEquals(state, Ended(PublicationEnding.Removed))
    assertEquals(
      sent(actions).map(s =>
        (s.tag.map(_.value), s.body.map(_.text), s.expires.value)
      ),
      List(
        (None, Some("one"), 3600),
        (Some("t1"), None, 3600),
        (Some("t2"), Some("two"), 3600),
        (Some("t3"), None, 0)
      )
    )
    // One CSeq for each, rising.
    assertEquals(
      sent(actions).map(_.context.cseq),
      List(1, 2, 3, 4).map(cseqNumber)
    )
  }
}
