package softphone4s.subscription

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.model.*

class SubscriptionDialogsSuite extends FunSuite {

  private val presence = EventHeader("presence").fold(e => fail(e), identity)

  private val context = SubscriptionContext(
    callId = callId("sub@host"),
    localTag = tag("ltag"),
    remoteTag = None,
    cseq = cseqNumber(1),
    event = presence,
    requested = Expires(3600).fold(e => fail(e), identity)
  )

  private val account = uri("sip:alice@test.local")

  private def accepted(
      contacts: List[NameAddr] = List(nameAddr("<sip:bob@198.51.100.7:5070>")),
      routes: List[NameAddr] = Nil
  ) =
    SipResponse(
      200,
      "OK",
      SipHeaders(
        callId = context.callId,
        from = nameAddr("<sip:alice@test.local>;tag=ltag"),
        to = nameAddr("<sip:bob@test.local>;tag=srv"),
        vias =
          NonEmptyList.one(via("SIP/2.0/UDP 192.0.2.10:5060;branch=z9hG4bKs")),
        cseq = cseq(1, SipMethod.Subscribe),
        contacts = contacts,
        recordRoutes = routes
      )
    )

  private def notify(
      from: String = "<sip:bob@test.local>;tag=srv",
      contacts: List[NameAddr] = List(nameAddr("<sip:bob@198.51.100.7:5070>")),
      routes: List[NameAddr] = Nil,
      number: Int = 1
  ) =
    SipRequest(
      SipMethod.Notify,
      uri("sip:alice@192.0.2.10:5060"),
      SipHeaders(
        callId = context.callId,
        from = nameAddr(from),
        to = nameAddr("<sip:alice@test.local>;tag=ltag"),
        vias =
          NonEmptyList.one(via("SIP/2.0/UDP 192.0.2.1:5060;branch=z9hG4bKn")),
        cseq = cseq(number, SipMethod.Notify),
        contacts = contacts,
        recordRoutes = routes
      )
    )

  private val p1 = nameAddr("<sip:p1.example.com;lr>")
  private val p2 = nameAddr("<sip:p2.example.com;lr>")

  test(
    "the 2xx makes a dialog in which we are the caller: the notifier's tag and Contact"
  ) {
    val dialog = SubscriptionDialogs.accepted(context, account, accepted())
    assertEquals(dialog.role, DialogRole.Uac)
    assertEquals(dialog.remoteTag, Some(tag("srv")))
    assertEquals(dialog.localUri, account)
    assertEquals(dialog.remoteUri.render, "sip:bob@test.local")
    assertEquals(dialog.remoteTarget.render, "sip:bob@198.51.100.7:5070")
    assertEquals(dialog.callId, context.callId)
    assertEquals(dialog.localTag, context.localTag)
  }

  test(
    "with no Contact in the 2xx the notifier is reached at the address of the resource"
  ) {
    val dialog =
      SubscriptionDialogs.accepted(context, account, accepted(contacts = Nil))
    assertEquals(dialog.remoteTarget.render, "sip:bob@test.local")
  }

  test("the caller's route set is the Record-Routes reversed") {
    val dialog = SubscriptionDialogs.accepted(
      context,
      account,
      accepted(routes = List(p1, p2))
    )
    assertEquals(
      dialog.routeHeader.map(_.uri.render),
      List("sip:p2.example.com;lr", "sip:p1.example.com;lr")
    )
  }

  test(
    "a NOTIFY before the 2xx makes a dialog in which we answer: their tag, our To, the route set as it came"
  ) {
    val dialog = SubscriptionDialogs
      .notified(context, notify(routes = List(p1, p2), number = 4))
      .getOrElse(fail("no dialog"))
    assertEquals(dialog.role, DialogRole.Uas)
    assertEquals(dialog.remoteTag, Some(tag("srv")))
    assertEquals(dialog.localUri.render, "sip:alice@test.local")
    assertEquals(dialog.remoteUri.render, "sip:bob@test.local")
    assertEquals(dialog.remoteTarget.render, "sip:bob@198.51.100.7:5070")
    assertEquals(
      dialog.routeHeader.map(_.uri.render),
      List("sip:p1.example.com;lr", "sip:p2.example.com;lr")
    )
    assertEquals(dialog.remoteCseq.map(_.value), Some(4))
  }

  test("the dialog from a NOTIFY is one that its later NOTIFYs belong to") {
    val dialog = SubscriptionDialogs
      .notified(context, notify())
      .getOrElse(fail("no dialog"))
    assert(dialog.matches(notify(number = 2)))
    assert(!dialog.matches(notify(from = "<sip:bob@test.local>;tag=other")))
  }

  test("a NOTIFY with no From tag makes no dialog") {
    assertEquals(
      SubscriptionDialogs
        .notified(context, notify(from = "<sip:bob@test.local>")),
      None
    )
  }

  test(
    "a later NOTIFY with a Contact moves the target, and one without leaves it"
  ) {
    val dialog = SubscriptionDialogs.accepted(context, account, accepted())
    val moved  = SubscriptionDialogs.retargeted(
      dialog,
      notify(contacts = List(nameAddr("<sip:bob@203.0.113.9>")))
    )
    assertEquals(moved.remoteTarget.render, "sip:bob@203.0.113.9")
    assertEquals(
      SubscriptionDialogs.retargeted(dialog, notify(contacts = Nil)),
      dialog
    )
  }
}
