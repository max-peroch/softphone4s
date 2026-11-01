package softphone4s.subscription

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.model.*

import scala.concurrent.duration.*

class NotifyHeadersSuite extends FunSuite {

  private def expires(n: Int): Expires =
    Expires(n).fold(e => fail(e), identity)

  private def event(name: String, id: Option[String] = None) =
    EventHeader(name, id).fold(e => fail(e), identity)

  // ---- Event ------------------------------------------------------------------

  test("an Event is its package, and its id if it has one") {
    assertEquals(EventHeader.parse("presence"), Right(event("presence")))
    assertEquals(
      EventHeader.parse("message-summary;id=7"),
      Right(event("message-summary", Some("7")))
    )
  }

  test(
    "the package is compared without regard to case, and other parameters are not part of it"
  ) {
    assertEquals(EventHeader.parse("Presence"), Right(event("presence")))
    assertEquals(
      EventHeader.parse("presence;foo=bar"),
      Right(event("presence"))
    )
    assert(event("presence", Some("1")).matches(event("PRESENCE", Some("1"))))
    assert(!event("presence", Some("1")).matches(event("presence", Some("2"))))
    assert(!event("presence").matches(event("presence", Some("1"))))
    assert(!event("presence").matches(event("dialog")))
  }

  test("a package with dots, as presence.winfo, is a token") {
    assertEquals(event("presence.winfo").render, "presence.winfo")
  }

  test("an Event that is not a token, or is empty, is refused") {
    assert(EventHeader.parse("").isLeft)
    assert(EventHeader.parse("pre sence").isLeft)
    assert(EventHeader("a/b").isLeft)
    assert(EventHeader("presence", Some("a b")).isLeft)
  }

  test("an Event renders as it parses") {
    assertEquals(
      event("message-summary", Some("7")).render,
      "message-summary;id=7"
    )
    assertEquals(
      EventHeader.parse(event("dialog", Some("x")).render),
      Right(event("dialog", Some("x")))
    )
  }

  // ---- Subscription-State -------------------------------------------------------

  test("active and pending, with and without the time left") {
    assertEquals(NotifyState.parse("active"), Right(NotifyState.Active(None)))
    assertEquals(
      NotifyState.parse("active;expires=3600"),
      Right(NotifyState.Active(Some(expires(3600))))
    )
    assertEquals(
      NotifyState.parse("pending;expires=60"),
      Right(NotifyState.Pending(Some(expires(60))))
    )
    assertEquals(
      NotifyState.parse("Active ; Expires=5"),
      Right(NotifyState.Active(Some(expires(5))))
    )
  }

  test("terminated says why, and when to try again") {
    assertEquals(
      NotifyState.parse("terminated;reason=timeout"),
      Right(NotifyState.Terminated(EndReason.Timeout, None))
    )
    assertEquals(
      NotifyState.parse("terminated;reason=probation;retry-after=30"),
      Right(NotifyState.Terminated(EndReason.Probation, Some(30.seconds)))
    )
  }

  test("every reason is known, and one we do not know is kept as it was said") {
    List(
      "deactivated" -> EndReason.Deactivated,
      "probation"   -> EndReason.Probation,
      "rejected"    -> EndReason.Rejected,
      "timeout"     -> EndReason.Timeout,
      "giveup"      -> EndReason.GiveUp,
      "noresource"  -> EndReason.NoResource,
      "invariant"   -> EndReason.Invariant
    ).foreach((text, reason) =>
      assertEquals(
        NotifyState.parse(s"terminated;reason=$text"),
        Right(NotifyState.Terminated(reason, None))
      )
    )
    assertEquals(
      NotifyState.parse("terminated;reason=Whatever"),
      Right(NotifyState.Terminated(EndReason.Other("whatever"), None))
    )
  }

  test("a terminated state with no reason says so, and is not followed") {
    assertEquals(
      NotifyState.parse("terminated"),
      Right(NotifyState.Terminated(EndReason.Unspecified, None))
    )
    assertEquals(
      Ending
        .Terminated(EndReason.Unspecified, None)
        .resubscribeAfter(20.seconds),
      None
    )
  }

  test(
    "a state we do not know, or an expiry that is not a number, is refused"
  ) {
    assert(NotifyState.parse("sleeping").isLeft)
    assert(NotifyState.parse("active;expires=soon").isLeft)
    assert(NotifyState.parse("").isLeft)
  }

  test("only terminated is terminated") {
    assert(NotifyState.parse("terminated").exists(_.isTerminated))
    assert(!NotifyState.Active(None).isTerminated)
    assert(!NotifyState.Pending(None).isTerminated)
  }

  // ---- NOTIFY ---------------------------------------------------------------------

  private def header(name: String, value: String) =
    ExtensionHeader(FieldName(name).fold(e => fail(e), identity), value)

  private def notify(
      method: SipMethod = SipMethod.Notify,
      others: List[ExtensionHeader] = List(
        header("Event", "presence"),
        header("Subscription-State", "active;expires=600")
      ),
      contentType: Option[MediaType] = None,
      body: String = "",
      number: Int = 3
  ) =
    SipRequest(
      method,
      uri("sip:alice@192.0.2.10:5060"),
      SipHeaders(
        callId = callId("sub@host"),
        from = nameAddr("<sip:bob@test.local>;tag=srv"),
        to = nameAddr("<sip:alice@test.local>;tag=ltag"),
        vias =
          NonEmptyList.one(via("SIP/2.0/UDP 192.0.2.1:5060;branch=z9hG4bKn")),
        cseq = cseq(number, method),
        contentType = contentType,
        others = others
      ),
      body
    )

  test("a NOTIFY is read: its event, its state, what it carries and its CSeq") {
    val xml =
      MediaType.parse("application/pidf+xml").fold(e => fail(e), identity)
    val read = NotifyInfo.from(
      notify(contentType = Some(xml), body = "<presence/>", number = 9)
    )
    assertEquals(
      read,
      Right(
        NotifyInfo(
          event("presence"),
          NotifyState.Active(Some(expires(600))),
          Some(xml),
          "<presence/>",
          9L
        )
      )
    )
  }

  test("the compact form of Event is read too") {
    val request = notify(others =
      List(header("o", "dialog"), header("Subscription-State", "pending"))
    )
    assertEquals(NotifyInfo.from(request).map(_.event), Right(event("dialog")))
  }

  test(
    "a request that is not a NOTIFY, or lacks what a NOTIFY must have, is refused"
  ) {
    assertEquals(
      NotifyInfo.from(notify(method = SipMethod.Message)),
      Left(NotifyError.NotANotify)
    )
    assertEquals(
      NotifyInfo.from(
        notify(others = List(header("Subscription-State", "active")))
      ),
      Left(NotifyError.MissingEvent)
    )
    assertEquals(
      NotifyInfo.from(notify(others = List(header("Event", "presence")))),
      Left(NotifyError.MissingSubscriptionState)
    )
    assert(
      NotifyInfo
        .from(
          notify(others =
            List(header("Event", ""), header("Subscription-State", "active"))
          )
        )
        .left
        .exists(_.isInstanceOf[NotifyError.BadEvent])
    )
    assert(
      NotifyInfo
        .from(
          notify(others =
            List(
              header("Event", "presence"),
              header("Subscription-State", "sleeping")
            )
          )
        )
        .left
        .exists(_.isInstanceOf[NotifyError.BadSubscriptionState])
    )
    assertEquals(
      NotifyInfo.from(notify(body = "x")),
      Left(NotifyError.MissingContentType)
    )
  }

  test(
    "a NOTIFY and a SUBSCRIBE go over the wire and are read back as what they are"
  ) {
    val xml =
      MediaType.parse("application/pidf+xml").fold(e => fail(e), identity)
    val sent = notify(contentType = Some(xml), body = "<presence/>")
    softphone4s.codec.SipCodec.decode(
      softphone4s.codec.SipCodec.encode(sent)
    ) match {
      case Right(read: SipRequest) =>
        assertEquals(read.method, SipMethod.Notify)
        assertEquals(NotifyInfo.from(read).map(_.body), Right("<presence/>"))
      case other => fail(s"not read back as a request: $other")
    }
    val subscribe = sent.copy(
      method = SipMethod.Subscribe,
      headers = sent.headers.copy(cseq = cseq(1, SipMethod.Subscribe))
    )
    val text =
      new String(softphone4s.codec.SipCodec.encode(subscribe).toArray, "UTF-8")
    assert(text.startsWith("SUBSCRIBE sip:alice@192.0.2.10:5060 SIP/2.0"), text)
    assert(text.contains("CSeq: 1 SUBSCRIBE"), text)
    assertEquals(
      softphone4s.codec.SipCodec
        .decode(softphone4s.codec.SipCodec.encode(subscribe))
        .map {
          case r: SipRequest => r.method
          case other         => other
        },
      Right(SipMethod.Subscribe)
    )
  }

  test("every NotifyError says what is wrong") {
    List(
      NotifyError.NotANotify,
      NotifyError.MissingEvent,
      NotifyError.BadEvent("x"),
      NotifyError.MissingSubscriptionState,
      NotifyError.BadSubscriptionState("x"),
      NotifyError.MissingContentType
    ).foreach(e => assert(e.message.nonEmpty))
  }

  // ---- Ending ---------------------------------------------------------------------------

  private val backoff = 20.seconds

  test("whether to subscribe again depends on how it ended") {
    import Ending.*
    assertEquals(Unsubscribed.resubscribeAfter(backoff), None)
    assertEquals(Rejected(403, "Forbidden").resubscribeAfter(backoff), None)
    assertEquals(
      Failed(SubscriptionFailure.TimedOut).resubscribeAfter(backoff),
      Some(backoff)
    )
    assertEquals(Expired.resubscribeAfter(backoff), Some(Duration.Zero))
    assertEquals(DialogLost.resubscribeAfter(backoff), Some(Duration.Zero))
  }

  test("a terminated NOTIFY's reason says: at once, after a wait, or never") {
    def after(reason: EndReason, retry: Option[FiniteDuration] = None) =
      Ending.Terminated(reason, retry).resubscribeAfter(backoff)
    assertEquals(after(EndReason.Deactivated), Some(Duration.Zero))
    assertEquals(after(EndReason.Timeout), Some(Duration.Zero))
    assertEquals(after(EndReason.Probation, Some(45.seconds)), Some(45.seconds))
    assertEquals(after(EndReason.Probation), Some(backoff))
    assertEquals(after(EndReason.GiveUp, Some(10.seconds)), Some(10.seconds))
    assertEquals(after(EndReason.Rejected), None)
    assertEquals(after(EndReason.NoResource), None)
    assertEquals(after(EndReason.Invariant), None)
    assertEquals(after(EndReason.Other("odd")), None)
  }
}
