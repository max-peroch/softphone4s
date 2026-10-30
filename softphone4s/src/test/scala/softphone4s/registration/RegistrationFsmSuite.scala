package softphone4s.registration

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.fsm.{TimerAction, TransactionFailure}
import softphone4s.model.*
import softphone4s.registration.RegistrationAction.*
import softphone4s.registration.RegistrationEvent.*
import softphone4s.registration.RegistrationFsmState.*

import scala.concurrent.duration.*

class RegistrationFsmSuite extends FunSuite {

  private def expires(n: Int): Expires =
    Expires(n).fold(err => fail(err), identity)

  private val contact = uri("sip:test@192.0.2.10:5060")

  private val context = RegistrationContext(
    callId = callId("reg@host"),
    localTag = tag("ltag"),
    cseq = cseqNumber(1),
    contact = contact,
    requested = expires(3600)
  )

  private def step(state: RegistrationFsmState, event: RegistrationEvent) =
    RegistrationFsm.transition(event).run(state).value

  private def resp(
      code: Int,
      reason: String = "Reason",
      contacts: List[NameAddr] = Nil,
      expiresHeader: Option[Expires] = None,
      minExpires: Option[Expires] = None,
      www: List[String] = Nil,
      proxy: List[String] = Nil
  ): RegistrationEvent =
    Response(
      SipResponse(
        code,
        reason,
        SipHeaders(
          callId = context.callId,
          from = nameAddr("<sip:test@test.local>;tag=ltag"),
          to = nameAddr("<sip:test@test.local>;tag=srv"),
          vias = NonEmptyList.one(
            via("SIP/2.0/UDP 192.0.2.10:5060;branch=z9hG4bKr")
          ),
          cseq = cseq(1, SipMethod.Register),
          contacts = contacts,
          expires = expiresHeader,
          minExpires = minExpires,
          wwwAuthenticate = www.map(challenge),
          proxyAuthenticate = proxy.map(challenge)
        )
      )
    )

  private def bound(lifetimeSeconds: Int): NameAddr =
    NameAddr(contact).withExpires(expires(lifetimeSeconds))

  private val digest = """Digest realm="test.local", nonce="n1""""

  private def digestChallenge(header: String): DigestChallenge =
    AuthChallenge.parse(header) match {
      case AuthChallenge.Digest(c) => c
      case other                   => fail(s"not a Digest challenge: $other")
    }
  private val stale = """Digest realm="test.local", nonce="n2", stale=true"""

  private val registering = step(Idle, Start(context))._1
  private def attemptOf(s: RegistrationFsmState): Int = s match {
    case Registering(_, attempt, _) => attempt
    case Backoff(_, attempt)        => attempt
    case other                      => fail(s"no attempt in $other")
  }

  private def isSend(a: RegistrationAction): Boolean = a match {
    case SendRegister(_, _, _) => true
    case _                     => false
  }

  private val registered =
    step(registering, resp(200, contacts = List(bound(600))))._1

  // ---- Registering ------------------------------------------------------------

  test("Start sends the first REGISTER and reports Registering") {
    val (state, actions) = step(Idle, Start(context))
    assertEquals(state, Registering(context, 1, RegistrationPhase.Establishing))
    assertEquals(
      actions,
      List(
        Notify(RegistrationState.Registering),
        SendRegister(context, expires(3600), None)
      )
    )
  }

  test(
    "a 200 binds for the lifetime on our Contact, and refresh is set for half of it"
  ) {
    val (state, actions) =
      step(registering, resp(200, contacts = List(bound(600))))
    assertEquals(state, Registered(context, expires(600)))
    assertEquals(
      actions,
      List(
        Notify(RegistrationState.Registered(600.seconds)),
        Timer(TimerAction.Cancel(RegistrationTimer.Retry)),
        Timer(TimerAction.Start(RegistrationTimer.Refresh, 300.seconds))
      )
    )
  }

  test(
    "the lifetime falls back to the Expires header, then to what we asked for"
  ) {
    assertEquals(
      step(registering, resp(200, expiresHeader = Some(expires(120))))._1,
      Registered(context, expires(120))
    )
    assertEquals(
      step(registering, resp(200))._1,
      Registered(context, expires(3600))
    )
  }

  test("another party's Contact in the answer is not ours") {
    val other = NameAddr(uri("sip:other@192.0.2.99")).withExpires(expires(30))
    assertEquals(
      step(registering, resp(200, contacts = List(other, bound(900))))._1,
      Registered(context, expires(900))
    )
    assertEquals(
      step(registering, resp(200, contacts = List(other)))._1,
      Registered(context, expires(3600))
    )
  }

  test("a 200 that grants zero seconds is a failure, not a registration") {
    val (state, actions) =
      step(registering, resp(200, contacts = List(bound(0))))
    assert(state.isInstanceOf[Backoff])
    assert(actions.exists {
      case Notify(
            RegistrationState.Failed(RegistrationFailure.Rejected(200, _), _)
          ) =>
        true
      case _ => false
    })
  }

  test("provisional responses are ignored") {
    assertEquals(step(registering, resp(100)), (registering, Nil))
  }

  // ---- Authentication ----------------------------------------------------------

  test("a 401 is answered with the next CSeq, once") {
    val (state, actions) = step(registering, resp(401, www = List(digest)))
    val next             = context.copy(
      cseq = cseqNumber(2),
      authAttempts = Map(AuthKind.Origin -> 1),
      cached = Some((AuthKind.Origin, digestChallenge(digest)))
    )
    assertEquals(state, Registering(next, 1, RegistrationPhase.Establishing))
    assert(actions.exists {
      case SendRegister(c, l, Some((AuthKind.Origin, ch))) =>
        c == next && l == expires(3600) && ch.nonce.value == "n1"
      case _ => false
    })
  }

  test("a 407 is answered the same way, for the proxy") {
    val (_, actions) = step(registering, resp(407, proxy = List(digest)))
    assert(actions.exists {
      case SendRegister(_, _, Some((AuthKind.Proxy, _))) => true
      case _                                             => false
    })
  }

  test("a second 401 means the credentials are refused: failure with backoff") {
    val (afterFirst, _)  = step(registering, resp(401, www = List(digest)))
    val (state, actions) = step(afterFirst, resp(401, www = List(digest)))
    assert(state.isInstanceOf[Backoff])
    assert(
      actions.contains(
        Notify(
          RegistrationState.Failed(
            RegistrationFailure.AuthenticationFailed,
            5.seconds
          )
        )
      )
    )
    assert(!actions.exists(isSend))
  }

  test("a stale nonce earns one more answer") {
    val (afterFirst, _) = step(registering, resp(401, www = List(digest)))
    val (_, actions)    = step(afterFirst, resp(401, www = List(stale)))
    assert(actions.exists(isSend))
  }

  test("a 401 with nothing we can answer is a failure") {
    val (state, _) = step(registering, resp(401, www = List("Negotiate abc")))
    assert(state.isInstanceOf[Backoff])
  }

  // ---- The credential cache ----------------------------------------------------

  private def cachedAfterSuccess = {
    val challenged = step(registering, resp(401, www = List(digest)))._1
    step(challenged, resp(200, contacts = List(bound(600))))._1
  }

  private def authorizationOf(actions: List[RegistrationAction]) =
    actions.collectFirst { case SendRegister(_, _, auth) => auth }.flatten

  test(
    "after an authenticated success the next refresh carries the credentials at once"
  ) {
    val (_, actions) =
      step(cachedAfterSuccess, TimerFired(RegistrationTimer.Refresh))
    assertEquals(
      authorizationOf(actions).map((kind, c) => (kind, c.nonce.value)),
      Some((AuthKind.Origin, "n1"))
    )
  }

  test(
    "a retry after a failure and the final unregistration also use the cache"
  ) {
    val ok = cachedAfterSuccess
    assert(authorizationOf(step(ok, Stop)._2).isDefined)
    val failed = step(
      step(ok, TimerFired(RegistrationTimer.Refresh))._1,
      resp(500)
    )._1
    assert(
      authorizationOf(
        step(failed, TimerFired(RegistrationTimer.Retry))._2
      ).isDefined
    )
  }

  test("a registration that never needed credentials sends none") {
    val ok = step(registering, resp(200, contacts = List(bound(600))))._1
    assertEquals(
      authorizationOf(step(ok, TimerFired(RegistrationTimer.Refresh))._2),
      None
    )
  }

  test(
    "a stale answer to the cached credentials is answered with the fresh nonce, which replaces the cache"
  ) {
    val (refreshing, _) =
      step(cachedAfterSuccess, TimerFired(RegistrationTimer.Refresh))
    val (state, actions) = step(refreshing, resp(401, www = List(stale)))
    assertEquals(
      authorizationOf(actions).map((_, c) => c.nonce.value),
      Some("n2")
    )
    state match {
      case Registering(c, _, _) =>
        assertEquals(c.cached.map((_, ch) => ch.nonce.value), Some("n2"))
      case other => fail(s"unexpected: $other")
    }
  }

  test("credentials that are refused are forgotten, not sent again") {
    val (refreshing, _) =
      step(cachedAfterSuccess, TimerFired(RegistrationTimer.Refresh))
    val (answered, _) =
      step(refreshing, resp(401, www = List(digest))) // a new challenge
    val (failed, _) =
      step(answered, resp(401, www = List(digest))) // refused again
    failed match {
      case Backoff(c, _) => assertEquals(c.cached, None)
      case other         => fail(s"unexpected: $other")
    }
    assertEquals(
      authorizationOf(step(failed, TimerFired(RegistrationTimer.Retry))._2),
      None
    )
  }

  test("SHA-256 is chosen over MD5 when both are offered") {
    val both = List(
      """Digest realm="test.local", nonce="m", algorithm=MD5""",
      """Digest realm="test.local", nonce="s", algorithm=SHA-256"""
    )
    val (_, actions) = step(registering, resp(401, www = both))
    assertEquals(
      authorizationOf(actions).map((_, c) => c.nonce.value),
      Some("s")
    )
  }

  // ---- 423 Interval Too Brief --------------------------------------------------

  test("a 423 raises the lifetime to Min-Expires and tries again") {
    val (state, actions) =
      step(registering, resp(423, minExpires = Some(expires(7200))))
    val next = context.copy(cseq = cseqNumber(2), requested = expires(7200))
    assertEquals(state, Registering(next, 1, RegistrationPhase.Establishing))
    assertEquals(actions, List(SendRegister(next, expires(7200), None)))
  }

  test("a 423 that does not ask for more than we offered is a failure") {
    List(None, Some(expires(60))).foreach { minimum =>
      val (state, _) = step(registering, resp(423, minExpires = minimum))
      assert(state.isInstanceOf[Backoff], minimum.toString)
    }
  }

  // ---- Failure and backoff -------------------------------------------------------

  test("any other final error fails, and a retry is scheduled") {
    val (state, actions) = step(registering, resp(500, "Server Error"))
    assertEquals(state, Backoff(context, 1))
    assertEquals(
      actions,
      List(
        Timer(TimerAction.Cancel(RegistrationTimer.Refresh)),
        Notify(
          RegistrationState.Failed(
            RegistrationFailure.Rejected(500, "Server Error"),
            5.seconds
          )
        ),
        Timer(TimerAction.Start(RegistrationTimer.Retry, 5.seconds))
      )
    )
  }

  test("a timeout or transport failure fails the attempt with its own reason") {
    List(
      TransactionFailure.TimedOut        -> RegistrationFailure.TimedOut,
      TransactionFailure.TransportFailed -> RegistrationFailure.TransportFailed
    ).foreach { (failure, reason) =>
      val (state, actions) = step(registering, TransactionFailed(failure))
      assert(state.isInstanceOf[Backoff])
      assert(
        actions.contains(Notify(RegistrationState.Failed(reason, 5.seconds)))
      )
    }
  }

  test("the backoff doubles from 5 s and stops at 5 minutes") {
    assertEquals(
      (1 to 8).map(RegistrationFsm.backoff).toList,
      List(
        5.seconds,
        10.seconds,
        20.seconds,
        40.seconds,
        80.seconds,
        160.seconds,
        300.seconds,
        300.seconds
      )
    )
    assertEquals(RegistrationFsm.backoff(1000), 300.seconds)
  }

  test("the retry registers again with a fresh CSeq and counts the attempt") {
    val failed           = step(registering, resp(500))._1
    val (state, actions) = step(failed, TimerFired(RegistrationTimer.Retry))
    val next             = context.copy(cseq = cseqNumber(2))
    assertEquals(state, Registering(next, 2, RegistrationPhase.Establishing))
    assertEquals(
      actions,
      List(
        Notify(RegistrationState.Registering),
        SendRegister(next, expires(3600), None)
      )
    )
    // The second failure waits twice as long.
    val (_, again) = step(state, resp(500))
    assert(
      again.contains(
        Timer(TimerAction.Start(RegistrationTimer.Retry, 10.seconds))
      )
    )
  }

  test("a success resets the backoff") {
    val failed    = step(registering, resp(500))._1
    val retrying  = step(failed, TimerFired(RegistrationTimer.Retry))._1
    val good      = step(retrying, resp(200, contacts = List(bound(600))))._1
    val refreshed = step(good, TimerFired(RegistrationTimer.Refresh))._1
    assertEquals(attemptOf(refreshed), 1)
  }

  // ---- Refreshing --------------------------------------------------------------

  test("the refresh sends a new REGISTER quietly, still reporting Registered") {
    val (state, actions) =
      step(registered, TimerFired(RegistrationTimer.Refresh))
    val next = context.copy(cseq = cseqNumber(2))
    assertEquals(state, Registering(next, 1, RegistrationPhase.Refreshing))
    assertEquals(actions, List(SendRegister(next, expires(3600), None)))
  }

  test("a refresh that succeeds binds again for the new lifetime") {
    val (refreshing, _) =
      step(registered, TimerFired(RegistrationTimer.Refresh))
    val (state, actions) =
      step(refreshing, resp(200, contacts = List(bound(1200))))
    assertEquals(
      state,
      Registered(
        refreshing match {
          case Registering(c, _, _) => c
          case other                => fail(s"unexpected $other")
        },
        expires(1200)
      )
    )
    assert(
      actions.contains(
        Timer(TimerAction.Start(RegistrationTimer.Refresh, 600.seconds))
      )
    )
  }

  test("a refresh that fails reports the failure and retries") {
    val (refreshing, _) =
      step(registered, TimerFired(RegistrationTimer.Refresh))
    val (state, actions) =
      step(refreshing, TransactionFailed(TransactionFailure.TimedOut))
    assert(state.isInstanceOf[Backoff])
    assert(actions.exists {
      case Notify(RegistrationState.Failed(RegistrationFailure.TimedOut, _)) =>
        true
      case _ => false
    })
  }

  // ---- Stopping ----------------------------------------------------------------

  test("Stop removes the binding with Expires: 0 and cancels the timers") {
    val (state, actions) = step(registered, Stop)
    val next             = context.copy(cseq = cseqNumber(1).next)
    assertEquals(state, Unregistering(next))
    assertEquals(
      actions,
      List(
        Timer(TimerAction.Cancel(RegistrationTimer.Refresh)),
        Timer(TimerAction.Cancel(RegistrationTimer.Retry)),
        Notify(RegistrationState.Unregistering),
        SendRegister(next, Expires.Zero, None)
      )
    )
  }

  test("the answer to the unregistration ends it, whatever it says") {
    List(200, 404, 500).foreach { code =>
      val (state, actions) = step(step(registered, Stop)._1, resp(code))
      assertEquals(state, Stopped, code.toString)
      assertEquals(actions, List(Notify(RegistrationState.Unregistered)))
    }
  }

  test("a challenge to the unregistration is answered once, then given up on") {
    val unregistering             = step(registered, Stop)._1
    val (afterChallenge, actions) =
      step(unregistering, resp(401, www = List(digest)))
    assert(actions.exists {
      case SendRegister(_, l, Some((AuthKind.Origin, _))) => l.isZero
      case _                                              => false
    })
    assertEquals(
      step(afterChallenge, resp(401, www = List(digest)))._1,
      Stopped
    )
  }

  test("an unregistration that times out or cannot be sent still ends") {
    List(TransactionFailure.TimedOut, TransactionFailure.TransportFailed)
      .foreach { f =>
        assertEquals(
          step(step(registered, Stop)._1, TransactionFailed(f)),
          (Stopped, List(Notify(RegistrationState.Unregistered)))
        )
      }
  }

  test(
    "Stop while registering also unregisters, in case the first REGISTER got through"
  ) {
    val (state, actions) = step(registering, Stop)
    assert(state.isInstanceOf[Unregistering])
    assert(actions.exists(isSend))
  }

  test("Stop while waiting to retry, or before starting, sends nothing") {
    val backoff = step(registering, resp(500))._1
    List(backoff, Idle).foreach { s =>
      val (state, actions) = step(s, Stop)
      assertEquals(state, Stopped)
      assert(!actions.exists(isSend))
      assert(actions.contains(Notify(RegistrationState.Unregistered)))
    }
  }

  test("once stopped, nothing happens") {
    List(
      Start(context),
      resp(200),
      Stop,
      TimerFired(RegistrationTimer.Refresh),
      TimerFired(RegistrationTimer.Retry),
      TransactionFailed(TransactionFailure.TimedOut)
    ).foreach(e => assertEquals(step(Stopped, e), (Stopped, Nil)))
  }
}
