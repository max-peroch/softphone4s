package softphone4s.fsm

import cats.data.NonEmptyList
import softphone4s.codec.{DigestAuth, SdpCodec}
import softphone4s.model.*
import softphone4s.transaction.TransactionMessages

/** Pure construction of the SIP messages of a call from a [[LocalEndpoint]]:
  * every function is deterministic in its arguments, with branches and tags
  * supplied by the caller.
  */
object SipMessages {

  private def vias(e: LocalEndpoint, branch: Branch): NonEmptyList[Via] =
    NonEmptyList.one(Via.request(e.transport, e.ip, e.sipPort, branch))

  private def contacts(e: LocalEndpoint): List[NameAddr] =
    List(NameAddr(e.contact))

  extension (e: LocalEndpoint) {

    // -----------------------------------------------------------------------
    // INVITE
    // -----------------------------------------------------------------------

    /** The SDP offer an INVITE from this endpoint carries. */
    def offerBody: String = SdpCodec.buildOffer(e.ip, e.rtpPort)

    def buildInvite(
        callee: SipUri,
        callId: CallId,
        localTag: Tag,
        cseq: CSeqNumber,
        branch: Branch,
        authHeader: Option[ExtensionHeader] = None,
        extraHeaders: List[ExtensionHeader] = Nil
    ): SipRequest =
      SipRequest(
        method = SipMethod.Invite,
        requestUri = callee,
        headers = SipHeaders(
          callId = callId,
          from = NameAddr.withTag(e.accountUri, localTag),
          to = NameAddr(callee),
          vias = vias(e, branch),
          cseq = CSeq(cseq, SipMethod.Invite),
          contacts = contacts(e),
          contentType = Some(MediaType.Sdp),
          others = authHeader.toList ++ extraHeaders
        ),
        body = SdpCodec.buildOffer(e.ip, e.rtpPort)
      )

    /** The Authorization or Proxy-Authorization header answering `challenge` on
      * a retried `method` request to `target` carrying `body` (which `auth-int`
      * hashes).
      */
    def authorizationFor(
        kind: AuthKind,
        method: SipMethod,
        target: SipUri,
        body: String,
        challenge: DigestChallenge,
        cnonce: Cnonce,
        nonceCount: NonceCount
    ): ExtensionHeader =
      ExtensionHeader(
        kind.responseHeader,
        DigestAuth
          .respond(
            challenge,
            method.toString.toUpperCase,
            target.render,
            body,
            e.credentials,
            cnonce,
            nonceCount
          )
          .render
      )

    // -----------------------------------------------------------------------
    // Responses to an incoming INVITE
    // -----------------------------------------------------------------------

    /** A response to `invite` that belongs to the dialog we are forming: our
      * tag in To, our Contact and the Record-Route set copied (RFC 3261
      * §12.1.1) on 1xx and 2xx, and `answer` as the SDP body if given.
      */
    def buildInviteResponse(
        invite: SipRequest,
        code: Int,
        reason: String,
        localTag: Tag,
        answer: Option[String] = None
    ): SipResponse = {
      val base =
        TransactionMessages.responseTo(invite, code, reason, Some(localTag))
      val dialogForming = code > 100 && code < 300
      base.copy(
        headers = base.headers.copy(
          contacts = if dialogForming then contacts(e) else Nil,
          recordRoutes =
            if dialogForming then invite.headers.recordRoutes else Nil,
          contentType = answer.map(_ => MediaType.Sdp)
        ),
        body = answer.getOrElse("")
      )
    }

    // -----------------------------------------------------------------------
    // REGISTER
    // -----------------------------------------------------------------------

    /** A REGISTER binding our Contact at `registrar` for `lifetime` (RFC 3261
      * §10.2). The To is the address of record, without a tag; every
      * registration of the binding keeps one Call-ID and a rising CSeq.
      * `Expires.Zero` removes the binding.
      */
    def buildRegister(
        registrar: SipUri,
        callId: CallId,
        localTag: Tag,
        cseq: CSeqNumber,
        branch: Branch,
        lifetime: Expires,
        authHeader: Option[ExtensionHeader] = None
    ): SipRequest =
      SipRequest(
        method = SipMethod.Register,
        requestUri = registrar,
        headers = SipHeaders(
          callId = callId,
          from = NameAddr.withTag(e.accountUri, localTag),
          to = NameAddr(e.accountUri),
          vias = vias(e, branch),
          cseq = CSeq(cseq, SipMethod.Register),
          contacts = List(NameAddr(e.contact).withExpires(lifetime)),
          expires = Some(lifetime),
          others = authHeader.toList
        )
      )

    // -----------------------------------------------------------------------
    // ACK
    // -----------------------------------------------------------------------

    def buildAck(
        dialog: SipDialog,
        branch: Branch,
        sdpAnswer: Option[String]
    ): SipRequest =
      SipRequest(
        method = SipMethod.Ack,
        requestUri = dialog.route.requestUri,
        headers = SipHeaders(
          callId = dialog.callId,
          from = NameAddr.withTag(dialog.localUri, dialog.localTag),
          to = remoteParty(dialog),
          vias = vias(e, branch),
          cseq = CSeq(dialog.localCseq, SipMethod.Ack),
          contacts = contacts(e),
          routes = dialog.route.routes,
          contentType = sdpAnswer.map(_ => MediaType.Sdp)
        ),
        body = sdpAnswer.getOrElse("")
      )

    // -----------------------------------------------------------------------
    // CANCEL
    // -----------------------------------------------------------------------

    def buildCancel(ctx: InviteCtx, branch: Branch): SipRequest =
      SipRequest(
        method = SipMethod.Cancel,
        requestUri = ctx.callee,
        headers = SipHeaders(
          callId = ctx.callId,
          from = NameAddr.withTag(e.accountUri, ctx.localTag),
          to = NameAddr(ctx.callee),
          vias = vias(e, branch),
          cseq = CSeq(ctx.cseq, SipMethod.Cancel),
          contacts = contacts(e)
        )
      )

    // -----------------------------------------------------------------------
    // BYE
    // -----------------------------------------------------------------------

    def buildBye(dialog: SipDialog, branch: Branch): SipRequest =
      SipRequest(
        method = SipMethod.Bye,
        requestUri = dialog.route.requestUri,
        headers = SipHeaders(
          callId = dialog.callId,
          from = NameAddr.withTag(dialog.localUri, dialog.localTag),
          to = remoteParty(dialog),
          vias = vias(e, branch),
          cseq = CSeq(dialog.localCseq.next, SipMethod.Bye),
          contacts = contacts(e),
          routes = dialog.route.routes
        )
      )

    // -----------------------------------------------------------------------
    // re-INVITE and UPDATE
    // -----------------------------------------------------------------------

    /** A re-INVITE within `dialog` offering `direction` (RFC 3261 §14.1). Its
      * CSeq is the dialog's current local CSeq, which the caller has already
      * advanced for it.
      */
    def buildReinvite(
        dialog: SipDialog,
        branch: Branch,
        direction: MediaDirection
    ): SipRequest =
      buildSessionRequest(dialog, branch, SessionChange(direction))

    /** The re-INVITE or UPDATE that carries `change`, with the dialog's current
      * local CSeq (already advanced for it) and an SDP offer.
      */
    def buildSessionRequest(
        dialog: SipDialog,
        branch: Branch,
        change: SessionChange
    ): SipRequest =
      SipRequest(
        method = change.method.sipMethod,
        requestUri = dialog.route.requestUri,
        headers = SipHeaders(
          callId = dialog.callId,
          from = NameAddr.withTag(dialog.localUri, dialog.localTag),
          to = remoteParty(dialog),
          vias = vias(e, branch),
          cseq = CSeq(dialog.localCseq, change.method.sipMethod),
          contacts = contacts(e),
          contentType = Some(MediaType.Sdp),
          routes = dialog.route.routes
        ),
        body = SdpCodec.buildOffer(e.ip, e.rtpPort, change.direction)
      )

    def buildByeOk(req: SipRequest): SipResponse =
      SipResponse(
        statusCode = 200,
        reason = "OK",
        headers = req.headers.copy(contacts = contacts(e))
      )

    // -----------------------------------------------------------------------
    // INFO 200 OK
    // -----------------------------------------------------------------------

    def buildInfoOk(req: SipRequest): SipResponse =
      SipResponse(
        200,
        "OK",
        req.headers.copy(contacts = contacts(e), contentType = None)
      )

    /** The 200 to an OPTIONS query: what we can do (RFC 3261 §11.2). */
    def buildOptionsOk(req: SipRequest): SipResponse = {
      val ok = buildInfoOk(req)
      ok.copy(headers =
        ok.headers.copy(others =
          ExtensionHeader(FieldName.Allow, AllowedMethods) :: ok.headers.others
        )
      )
    }

    /** The 200 to a re-INVITE or UPDATE, carrying our SDP `answer`. */
    def buildSessionOk(req: SipRequest, answer: String): SipResponse = {
      val ok = buildInfoOk(req)
      ok.copy(
        headers = ok.headers.copy(contentType = Some(MediaType.Sdp)),
        body = answer
      )
    }

    // -----------------------------------------------------------------------
    // SDP answer (Mode B — offer was in 200, answer goes in ACK)
    // -----------------------------------------------------------------------

    def buildSdpAnswer: String =
      SdpCodec.buildAnswer(e.ip, e.rtpPort)
  }

  private val AllowedMethods: String =
    "INVITE, ACK, BYE, CANCEL, OPTIONS, INFO, UPDATE"

  private def remoteParty(dialog: SipDialog): NameAddr =
    NameAddr(
      None,
      dialog.remoteUri,
      dialog.remoteTag,
      scala.collection.immutable.ListMap.empty
    )
}
