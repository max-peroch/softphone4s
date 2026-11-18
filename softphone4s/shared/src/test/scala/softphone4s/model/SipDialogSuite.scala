package softphone4s.model

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*

class SipDialogSuite extends FunSuite {

  private def invite(
      from: String = "<sip:bob@example.com>;tag=ft"
  ): SipRequest =
    SipRequest(
      SipMethod.Invite,
      uri("sip:alice@example.com"),
      SipHeaders(
        callId = callId("d@host"),
        from = nameAddr(from),
        to = nameAddr("<sip:alice@example.com>"),
        vias = NonEmptyList.one(via("SIP/2.0/UDP 192.0.2.5;branch=z9hG4bKd")),
        cseq = cseq(7, SipMethod.Invite),
        contacts = List(nameAddr("<sip:bob@192.0.2.5:5070>")),
        recordRoutes = List(nameAddr("<sip:p1;lr>"), nameAddr("<sip:p2;lr>"))
      )
    )

  private def dialog = SipDialog.forInboundInvite(invite(), tag("lt")).get

  test("the answerer's dialog takes tags, URIs and target from the INVITE") {
    val d = dialog
    assertEquals(d.role, DialogRole.Uas)
    assertEquals(d.callId, callId("d@host"))
    assertEquals(d.localTag, tag("lt"))
    assertEquals(d.remoteTag, Some(tag("ft")))
    assertEquals(d.localUri, uri("sip:alice@example.com"))
    assertEquals(d.remoteUri, uri("sip:bob@example.com"))
    assertEquals(d.remoteTarget, uri("sip:bob@192.0.2.5:5070"))
    assertEquals(d.remoteCseq.map(_.value), Some(7))
  }

  test("without a Contact the target is the caller's address") {
    val noContact =
      invite().copy(headers = invite().headers.copy(contacts = Nil))
    assertEquals(
      SipDialog.forInboundInvite(noContact, tag("lt")).map(_.remoteTarget),
      Some(uri("sip:bob@example.com"))
    )
  }

  test("a caller with no From-tag cannot form a dialog") {
    assertEquals(
      SipDialog.forInboundInvite(invite("<sip:bob@example.com>"), tag("lt")),
      None
    )
  }

  test("the answerer sends Route as received, the caller reversed") {
    assertEquals(
      dialog.routeHeader.map(_.render),
      List("<sip:p1;lr>", "<sip:p2;lr>")
    )
    assertEquals(
      dialog.copy(role = DialogRole.Uac).routeHeader.map(_.render),
      List("<sip:p2;lr>", "<sip:p1;lr>")
    )
  }

  test("a request is accepted only if its CSeq is newer than the last seen") {
    val d = dialog
    assert(!d.accepts(cseqNumber(7)))
    assert(!d.accepts(cseqNumber(3)))
    assert(d.accepts(cseqNumber(8)))
    assert(d.seen(cseqNumber(8)).accepts(cseqNumber(9)))
    assert(!d.seen(cseqNumber(8)).accepts(cseqNumber(8)))
    assert(d.copy(remoteCseq = None).accepts(cseqNumber(1)))
  }

  // ---- Routing a request within the dialog (RFC 3261 §12.2.1.1) -------------

  private def withRoute(routes: String*): SipDialog =
    dialog.copy(
      routeSet = routes.toList.map(nameAddr),
      remoteTarget = uri("sip:bob@192.0.2.5:5070"),
      role = DialogRole.Uas
    )

  test("with no route set a request goes to the remote target") {
    val r = withRoute().route
    assertEquals(r.requestUri, uri("sip:bob@192.0.2.5:5070"))
    assertEquals(r.routes, Nil)
    assertEquals(r.nextHop, uri("sip:bob@192.0.2.5:5070"))
  }

  test(
    "with a loose router first the request-URI stays the target and the first route is the hop"
  ) {
    val r =
      withRoute("<sip:p1.example.com;lr>", "<sip:p2.example.com;lr>").route
    assertEquals(r.requestUri, uri("sip:bob@192.0.2.5:5070"))
    assertEquals(
      r.routes.map(_.render),
      List("<sip:p1.example.com;lr>", "<sip:p2.example.com;lr>")
    )
    assertEquals(r.nextHop, uri("sip:p1.example.com;lr"))
  }

  test(
    "with a strict router first its URI becomes the request-URI and the target joins the routes"
  ) {
    val r =
      withRoute("<sip:strict.example.com>", "<sip:p2.example.com;lr>").route
    assertEquals(r.requestUri, uri("sip:strict.example.com"))
    assertEquals(
      r.routes.map(_.render),
      List("<sip:p2.example.com;lr>", "<sip:bob@192.0.2.5:5070>")
    )
    assertEquals(r.nextHop, uri("sip:strict.example.com"))
  }

  test("the caller reverses the Record-Route set before routing") {
    val d = withRoute("<sip:p1.example.com;lr>", "<sip:p2.example.com;lr>")
      .copy(role = DialogRole.Uac)
    assertEquals(d.route.nextHop, uri("sip:p2.example.com;lr"))
    assertEquals(
      d.route.routes.map(_.render),
      List("<sip:p2.example.com;lr>", "<sip:p1.example.com;lr>")
    )
  }

  // ---- Which dialog a request belongs to ------------------------------------

  private def request(to: String, from: String) =
    invite().copy(headers =
      invite().headers.copy(to = nameAddr(to), from = nameAddr(from))
    )

  test(
    "a request belongs to the dialog if it carries our tag in To and theirs in From"
  ) {
    val d = dialog
    assert(
      d.matches(
        request(
          "<sip:alice@example.com>;tag=lt",
          "<sip:bob@example.com>;tag=ft"
        )
      )
    )
    assert(
      !d.matches(
        request(
          "<sip:alice@example.com>;tag=other",
          "<sip:bob@example.com>;tag=ft"
        )
      )
    )
    assert(
      !d.matches(
        request(
          "<sip:alice@example.com>;tag=lt",
          "<sip:bob@example.com>;tag=other"
        )
      )
    )
    assert(
      !d.matches(
        request("<sip:alice@example.com>", "<sip:bob@example.com>;tag=ft")
      )
    )
    assert(
      !d.matches(
        request("<sip:alice@example.com>;tag=lt", "<sip:bob@example.com>")
      )
    )
  }
}
