package softphone4s.fsm

import cats.data.NonEmptyList
import softphone4s.codec.{DigestAuth, SdpCodec}
import softphone4s.model.*

/** Pure construction of the SIP messages of a call from a [[LocalEndpoint]]:
  * every function is deterministic in its arguments, with branches and tags
  * supplied by the caller.
  */
object SipMessages {

  private def vias(e: LocalEndpoint, branch: Branch): NonEmptyList[Via] =
    NonEmptyList.one(Via.request(Transport.Udp, e.ip, e.sipPort, branch))

  private def contacts(e: LocalEndpoint): List[NameAddr] =
    List(NameAddr(e.contact))

  extension (e: LocalEndpoint) {

    // -----------------------------------------------------------------------
    // INVITE
    // -----------------------------------------------------------------------

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
      * a retried INVITE to `callee`.
      */
    def authorizationFor(
        kind: AuthKind,
        callee: SipUri,
        challenge: DigestChallenge,
        cnonce: Cnonce,
        nonceCount: NonceCount
    ): ExtensionHeader =
      ExtensionHeader(
        kind.responseHeader,
        DigestAuth
          .respond(
            challenge,
            SipMethod.Invite.toString.toUpperCase,
            callee.render,
            e.credentials,
            cnonce,
            nonceCount
          )
          .render
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
        requestUri = dialog.remoteTarget,
        headers = SipHeaders(
          callId = dialog.callId,
          from = NameAddr.withTag(dialog.localUri, dialog.localTag),
          to = remoteParty(dialog),
          vias = vias(e, branch),
          cseq = CSeq(dialog.localCseq, SipMethod.Ack),
          contacts = contacts(e),
          routes = dialog.routeHeader,
          contentType = sdpAnswer.map(_ => MediaType.Sdp)
        ),
        body = sdpAnswer.getOrElse("")
      )

    // RFC 3261 §17.1.1.3: transaction-layer ACK for non-2xx INVITE responses.
    // Uses the same Via branch as the original INVITE (same transaction).
    def buildAckForNon2xx(
        ctx: InviteCtx,
        response: SipResponse,
        branch: Branch
    ): SipRequest =
      SipRequest(
        method = SipMethod.Ack,
        requestUri = ctx.callee,
        headers = SipHeaders(
          callId = ctx.callId,
          from = NameAddr.withTag(e.accountUri, ctx.localTag),
          to = response.headers.to,
          vias = vias(e, branch),
          cseq = CSeq(ctx.cseq, SipMethod.Ack),
          contacts = contacts(e)
        )
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
        requestUri = dialog.remoteTarget,
        headers = SipHeaders(
          callId = dialog.callId,
          from = NameAddr.withTag(dialog.localUri, dialog.localTag),
          to = remoteParty(dialog),
          vias = vias(e, branch),
          cseq = CSeq(dialog.localCseq.next, SipMethod.Bye),
          contacts = contacts(e),
          routes = dialog.routeHeader
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
    // SDP answer (Mode B — offer was in 200, answer goes in ACK)
    // -----------------------------------------------------------------------

    def buildSdpAnswer: String =
      SdpCodec.buildAnswer(e.ip, e.rtpPort)
  }

  private def remoteParty(dialog: SipDialog): NameAddr =
    NameAddr(
      None,
      dialog.remoteUri,
      dialog.remoteTag,
      scala.collection.immutable.ListMap.empty
    )
}
