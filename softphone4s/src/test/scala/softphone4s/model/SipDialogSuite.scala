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
}
