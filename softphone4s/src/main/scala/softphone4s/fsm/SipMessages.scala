package softphone4s.fsm

import softphone4s.codec.{DigestAuth, SdpCodec}
import softphone4s.model.*

/** Pure construction of the SIP messages of a call from a [[LocalEndpoint]]:
  * every function is deterministic in its arguments, with branches and tags
  * supplied by the caller.
  */
object SipMessages {

  private def contactHeader(e: LocalEndpoint): String = s"<${e.contact.render}>"
  private def viaHost(e: LocalEndpoint): String       = s"${e.ip}:${e.sipPort}"

  private def authHeaderMap(
      authHeader: Option[(String, String)]
  ): Map[String, List[String]] =
    authHeader.fold(Map.empty) { case (name, value) =>
      Map(name -> List(value))
    }

  extension (e: LocalEndpoint) {

    // -------------------------------------------------------------------------
    // INVITE
    // -------------------------------------------------------------------------

    def buildInvite(
        callee: SipUri,
        callId: String,
        localTag: String,
        cseq: Int,
        branch: String,
        authHeader: Option[(String, String)] = None,
        extraHeaders: Map[String, List[String]] = Map.empty
    ): SipRequest = {
      val sessionDescription = SdpCodec.buildOffer(e.ip, e.rtpPort)
      SipRequest(
        method = SipMethod.Invite,
        requestUri = callee,
        headers = SipHeaders(
          callId = callId,
          from = s"<${e.accountUri.render}>;tag=$localTag",
          to = s"<${callee.render}>",
          via = s"${viaHost(e)};branch=$branch",
          cseq = CSeq(cseq, SipMethod.Invite),
          contact = contactHeader(e),
          contentType = Some("application/sdp"),
          extraHeaders = authHeaderMap(authHeader) ++ extraHeaders
        ),
        body = sessionDescription
      )
    }

    def computeAuthHeader(
        callee: SipUri,
        headerName: String,
        challengeValue: Option[String],
        cnonce: String,
        nonceCount: Int = 1
    ): Option[(String, String)] =
      challengeValue.map { challengeHeaderValue =>
        val authValue = DigestAuth.compute(
          "INVITE",
          callee.render,
          e.credentials.username,
          e.credentials.password,
          challengeHeaderValue,
          cnonce,
          nonceCount
        )
        headerName -> authValue
      }

    // -------------------------------------------------------------------------
    // ACK
    // -------------------------------------------------------------------------

    def buildAck(
        dialog: SipDialog,
        branch: String,
        sdpAnswer: Option[String]
    ): SipRequest = {
      val (body, contentType) = sdpAnswer match {
        case Some(sessionDescription) =>
          (sessionDescription, Some("application/sdp"))
        case None => ("", None)
      }
      SipRequest(
        method = SipMethod.Ack,
        requestUri = dialog.remoteTarget,
        headers = SipHeaders(
          callId = dialog.callId,
          from = s"<${dialog.localUri.render}>;tag=${dialog.localTag}",
          to = s"<${dialog.remoteUri.render}>;tag=${dialog.remoteTag}",
          via = s"${viaHost(e)};branch=$branch",
          cseq = CSeq(dialog.localCseq, SipMethod.Ack),
          contact = contactHeader(e),
          route = dialog.routeHeader,
          contentType = contentType
        ),
        body = body
      )
    }

    // RFC 3261 §17.1.1.3: transaction-layer ACK for non-2xx INVITE responses.
    // Uses the same Via branch as the original INVITE (same transaction).
    def buildAckForNon2xx(
        ctx: InviteCtx,
        response: SipResponse,
        branch: String
    ): SipRequest =
      SipRequest(
        method = SipMethod.Ack,
        requestUri = ctx.callee,
        headers = SipHeaders(
          callId = ctx.callId,
          from = s"<${e.accountUri.render}>;tag=${ctx.localTag}",
          to = response.headers.to,
          via = s"${viaHost(e)};branch=$branch",
          cseq = CSeq(ctx.cseq, SipMethod.Ack),
          contact = contactHeader(e)
        )
      )

    // -------------------------------------------------------------------------
    // CANCEL
    // -------------------------------------------------------------------------

    def buildCancel(ctx: InviteCtx, branch: String): SipRequest =
      SipRequest(
        method = SipMethod.Cancel,
        requestUri = ctx.callee,
        headers = SipHeaders(
          callId = ctx.callId,
          from = s"<${e.accountUri.render}>;tag=${ctx.localTag}",
          to = s"<${ctx.callee.render}>",
          via = s"${viaHost(e)};branch=$branch",
          cseq = CSeq(ctx.cseq, SipMethod.Cancel),
          contact = contactHeader(e)
        )
      )

    // -------------------------------------------------------------------------
    // BYE
    // -------------------------------------------------------------------------

    def buildBye(dialog: SipDialog, branch: String): SipRequest =
      SipRequest(
        method = SipMethod.Bye,
        requestUri = dialog.remoteTarget,
        headers = SipHeaders(
          callId = dialog.callId,
          from = s"<${dialog.localUri.render}>;tag=${dialog.localTag}",
          to = s"<${dialog.remoteUri.render}>;tag=${dialog.remoteTag}",
          via = s"${viaHost(e)};branch=$branch",
          cseq = CSeq(dialog.localCseq + 1, SipMethod.Bye),
          contact = contactHeader(e),
          route = dialog.routeHeader
        )
      )

    def buildByeOk(req: SipRequest): SipResponse =
      SipResponse(
        statusCode = 200,
        reason = "OK",
        headers = req.headers.copy(contact = contactHeader(e))
      )

    // -------------------------------------------------------------------------
    // INFO 200 OK
    // -------------------------------------------------------------------------

    def buildInfoOk(req: SipRequest): SipResponse =
      SipResponse(
        200,
        "OK",
        req.headers
          .copy(
            contact = contactHeader(e),
            contentType = None,
            contentLength = 0
          )
      )

    // -------------------------------------------------------------------------
    // SDP answer (Mode B — offer was in 200, answer goes in ACK)
    // -------------------------------------------------------------------------

    def buildSdpAnswer: String =
      SdpCodec.buildAnswer(e.ip, e.rtpPort)
  }
}
