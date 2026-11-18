package softphone4s.fsm

import cats.data.NonEmptyList
import softphone4s.codec.{DigestAuth, SdpCodec}
import softphone4s.messaging.MessageContent
import softphone4s.publication.{EntityTag, PublicationBody}
import softphone4s.subscription.EventHeader
import softphone4s.model.*
import softphone4s.transaction.TransactionMessages

import scala.concurrent.duration.FiniteDuration

/** Pure construction of the SIP messages of a call from a
  * [[softphone4s.model.LocalEndpoint]]: every function is deterministic in its
  * arguments, with branches and tags supplied by the caller.
  */
object SipMessages {

  private def vias(e: LocalEndpoint, branch: Branch): NonEmptyList[Via] =
    NonEmptyList.one {
      val via = Via.request(e.transport, e.ip, e.sipPort, branch)
      // On a connection we opened, ask the peer to send its requests back on it.
      if e.transport == TransportProtocol.Udp then via.requestingRport
      else via.withAlias
    }

  private def contacts(e: LocalEndpoint): List[NameAddr] =
    List(NameAddr(e.contact))

  // -------------------------------------------------------------------------
  // Answers to a MESSAGE, which need nothing of ours
  // -------------------------------------------------------------------------

  /** The 200 to a MESSAGE we take. */
  def messageOk(req: SipRequest): SipResponse =
    TransactionMessages.responseTo(req, 200, "OK")

  /** The 415 to a MESSAGE whose content we cannot take, saying what we can (RFC
    * 3261 §21.4.13).
    */
  def unsupportedMessage(
      req: SipRequest,
      accepted: List[MediaType]
  ): SipResponse = {
    val refusal = TransactionMessages.responseTo(
      req,
      415,
      "Unsupported Media Type"
    )
    refusal.copy(headers =
      refusal.headers.copy(others =
        ExtensionHeader(
          FieldName.Accept,
          accepted.map(_.render).mkString(", ")
        ) :: refusal.headers.others
      )
    )
  }

  extension (e: LocalEndpoint) {

    // -----------------------------------------------------------------------
    // INVITE
    // -----------------------------------------------------------------------

    /** The SDP offer an INVITE from this endpoint carries. */
    def offerBody: String =
      SdpCodec.buildOffer(e.mediaIp, e.mediaPort, e.capabilities)

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
        body = SdpCodec.buildOffer(e.mediaIp, e.mediaPort, e.capabilities)
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

    /** An INFO within `dialog` carrying `digit` the way most phones do, in an
      * `application/dtmf-relay` body. Its CSeq is the dialog's current local
      * CSeq, which the caller has already advanced for it.
      */
    def buildInfo(
        dialog: SipDialog,
        branch: Branch,
        digit: DtmfDigit,
        duration: FiniteDuration
    ): SipRequest =
      SipRequest(
        method = SipMethod.Info,
        requestUri = dialog.route.requestUri,
        headers = SipHeaders(
          callId = dialog.callId,
          from = NameAddr.withTag(dialog.localUri, dialog.localTag),
          to = remoteParty(dialog),
          vias = vias(e, branch),
          cseq = CSeq(dialog.localCseq, SipMethod.Info),
          contacts = contacts(e),
          contentType = Some(MediaType.DtmfRelay),
          routes = dialog.route.routes
        ),
        body = s"Signal=${digit.symbol}\r\nDuration=${duration.toMillis}\r\n"
      )

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
        body = SdpCodec.buildOffer(
          e.mediaIp,
          e.mediaPort,
          e.capabilities,
          change.direction
        )
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

    // -----------------------------------------------------------------------
    // SUBSCRIBE
    // -----------------------------------------------------------------------

    /** The SUBSCRIBE that begins a subscription (RFC 6665 §4.1.1): outside any
      * dialog, to the resource, with our Contact (which the NOTIFYs come to),
      * the event, the lifetime asked for and the types of body we take.
      * Resending it after a challenge keeps the Call-ID and tag and raises the
      * CSeq.
      */
    def buildSubscribe(
        resource: SipUri,
        callId: CallId,
        localTag: Tag,
        cseq: CSeqNumber,
        branch: Branch,
        event: EventHeader,
        expires: Expires,
        accept: List[MediaType],
        authHeader: Option[ExtensionHeader] = None
    ): SipRequest =
      SipRequest(
        method = SipMethod.Subscribe,
        requestUri = resource,
        headers = SipHeaders(
          callId = callId,
          from = NameAddr.withTag(e.accountUri, localTag),
          to = NameAddr(resource),
          vias = vias(e, branch),
          cseq = CSeq(cseq, SipMethod.Subscribe),
          contacts = contacts(e),
          expires = Some(expires),
          others = subscribeHeaders(event, accept) ++ authHeader.toList
        )
      )

    /** A PUBLISH of the state of `presentity` (RFC 3903), outside any dialog.
      *
      * With no `tag` and a `body` it publishes the state for the first time.
      * With a `tag` it acts on the state that tag names: with a `body` it
      * changes it, with none it refreshes it, and with `Expires: 0` it removes
      * it. A PUBLISH with neither tag nor body says nothing and is refused by
      * notifiers; the caller does not send one.
      */
    def buildPublish(
        presentity: SipUri,
        callId: CallId,
        localTag: Tag,
        cseq: CSeqNumber,
        branch: Branch,
        event: EventHeader,
        expires: Expires,
        tag: Option[EntityTag],
        body: Option[PublicationBody],
        authHeader: Option[ExtensionHeader] = None
    ): SipRequest =
      SipRequest(
        method = SipMethod.Publish,
        requestUri = presentity,
        headers = SipHeaders(
          callId = callId,
          from = NameAddr.withTag(e.accountUri, localTag),
          to = NameAddr(presentity),
          vias = vias(e, branch),
          cseq = CSeq(cseq, SipMethod.Publish),
          expires = Some(expires),
          contentType = body.map(_.contentType),
          others = ExtensionHeader(FieldName.Event, event.render) ::
            tag.toList.map(t =>
              ExtensionHeader(FieldName.SipIfMatch, t.value)
            ) ++ authHeader.toList
        ),
        body = body.fold("")(_.text)
      )

    /** A SUBSCRIBE within the subscription's `dialog`, to refresh it or to end
      * it with `Expires: 0`. `cseq` is the one the subscription is at.
      */
    def buildSubscribeIn(
        dialog: SipDialog,
        cseq: CSeqNumber,
        branch: Branch,
        event: EventHeader,
        expires: Expires,
        accept: List[MediaType],
        authHeader: Option[ExtensionHeader] = None
    ): SipRequest =
      SipRequest(
        method = SipMethod.Subscribe,
        requestUri = dialog.route.requestUri,
        headers = SipHeaders(
          callId = dialog.callId,
          from = NameAddr.withTag(dialog.localUri, dialog.localTag),
          to = remoteParty(dialog),
          vias = vias(e, branch),
          cseq = CSeq(cseq, SipMethod.Subscribe),
          contacts = contacts(e),
          expires = Some(expires),
          routes = dialog.route.routes,
          others = subscribeHeaders(event, accept) ++ authHeader.toList
        )
      )

    // -----------------------------------------------------------------------
    // MESSAGE
    // -----------------------------------------------------------------------

    /** A pager-mode instant message (RFC 3428) to `recipient`: a request
      * outside any dialog, with the text in its body. Resending it after a
      * challenge keeps the Call-ID and tag and raises the CSeq.
      */
    def buildMessage(
        recipient: SipUri,
        callId: CallId,
        localTag: Tag,
        cseq: CSeqNumber,
        branch: Branch,
        content: MessageContent,
        authHeader: Option[ExtensionHeader] = None
    ): SipRequest =
      SipRequest(
        method = SipMethod.Message,
        requestUri = recipient,
        headers = SipHeaders(
          callId = callId,
          from = NameAddr.withTag(e.accountUri, localTag),
          to = NameAddr(recipient),
          vias = vias(e, branch),
          cseq = CSeq(cseq, SipMethod.Message),
          contentType = Some(content.contentType),
          others = authHeader.toList
        ),
        body = content.text
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

    def buildSdpAnswer(remoteSdp: String): String =
      SdpCodec.buildAnswerToBody(
        remoteSdp,
        e.mediaIp,
        e.mediaPort,
        e.capabilities
      )
  }

  private val AllowedMethods: String =
    "INVITE, ACK, BYE, CANCEL, OPTIONS, INFO, UPDATE, MESSAGE"

  private def subscribeHeaders(
      event: EventHeader,
      accept: List[MediaType]
  ): List[ExtensionHeader] =
    ExtensionHeader(FieldName.Event, event.render) ::
      Option
        .when(accept.nonEmpty)(
          ExtensionHeader(FieldName.Accept, accept.map(_.render).mkString(", "))
        )
        .toList

  private def remoteParty(dialog: SipDialog): NameAddr =
    NameAddr(
      None,
      dialog.remoteUri,
      dialog.remoteTag,
      scala.collection.immutable.ListMap.empty
    )
}
