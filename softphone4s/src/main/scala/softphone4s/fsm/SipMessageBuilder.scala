package softphone4s.fsm

import com.comcast.ip4s.{IpAddress, Port}
import softphone4s.codec.{DigestAuth, SdpCodec}
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*

class SipMessageBuilder(
    config: SoftphoneConfig,
    localIp: IpAddress,
    val localSipPort: Port,
    val localRtpPort: Port
) {
  val bindAddress: IpAddress                     = config.bindAddress
  private def sipUri(user: String, host: String) = s"sip:$user@$host"
  def calleeUri(callee: String): String          =
    if callee.contains("@") then s"sip:$callee"
    else sipUri(callee, config.realm)
  private[softphone4s] val localSipAddress: String =
    sipUri(config.user, config.realm)
  private def contactUri = s"<sip:${config.user}@$localIp:$localSipPort>"
  private def viaHost    = s"$localIp:$localSipPort"

  // -------------------------------------------------------------------------
  // INVITE
  // -------------------------------------------------------------------------

  def buildInvite(
      callee: String,
      callId: String,
      localTag: String,
      cseq: Int,
      branch: String,
      authHeader: Option[(String, String)] = None,
      extraHeaders: Map[String, List[String]] = Map.empty
  ): SipRequest = {
    val sessionDescription = SdpCodec.buildOffer(localIp, localRtpPort)
    SipRequest(
      method = SipMethod.Invite,
      requestUri = calleeUri(callee),
      headers = SipHeaders(
        callId = callId,
        from = s"<$localSipAddress>;tag=$localTag",
        to = s"<${calleeUri(callee)}>",
        via = s"$viaHost;branch=$branch",
        cseq = CSeq(cseq, SipMethod.Invite),
        contact = contactUri,
        contentType = Some("application/sdp"),
        extraHeaders = authHeaderMap(authHeader) ++ extraHeaders
      ),
      body = sessionDescription
    )
  }

  def computeAuthHeader(
      callee: String,
      headerName: String,
      challengeValue: Option[String],
      cnonce: String,
      nonceCount: Int = 1
  ): Option[(String, String)] =
    challengeValue.map { challengeHeaderValue =>
      val authValue = DigestAuth.compute(
        "INVITE",
        calleeUri(callee),
        config.user,
        config.password,
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
        from = s"<${dialog.localUri}>;tag=${dialog.localTag}",
        to = s"<${dialog.remoteUri}>;tag=${dialog.remoteTag}",
        via = s"$viaHost;branch=$branch",
        cseq = CSeq(dialog.localCseq, SipMethod.Ack),
        contact = contactUri,
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
      requestUri = calleeUri(ctx.callee),
      headers = SipHeaders(
        callId = ctx.callId,
        from = s"<$localSipAddress>;tag=${ctx.localTag}",
        to = response.headers.to,
        via = s"$viaHost;branch=$branch",
        cseq = CSeq(ctx.cseq, SipMethod.Ack),
        contact = contactUri
      )
    )

  // -------------------------------------------------------------------------
  // CANCEL
  // -------------------------------------------------------------------------

  def buildCancel(ctx: InviteCtx, branch: String): SipRequest =
    SipRequest(
      method = SipMethod.Cancel,
      requestUri = calleeUri(ctx.callee),
      headers = SipHeaders(
        callId = ctx.callId,
        from = s"<$localSipAddress>;tag=${ctx.localTag}",
        to = s"<${calleeUri(ctx.callee)}>",
        via = s"$viaHost;branch=$branch",
        cseq = CSeq(ctx.cseq, SipMethod.Cancel),
        contact = contactUri
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
        from = s"<${dialog.localUri}>;tag=${dialog.localTag}",
        to = s"<${dialog.remoteUri}>;tag=${dialog.remoteTag}",
        via = s"$viaHost;branch=$branch",
        cseq = CSeq(dialog.localCseq + 1, SipMethod.Bye),
        contact = contactUri,
        route = dialog.routeHeader
      )
    )

  def buildByeOk(req: SipRequest): SipResponse =
    SipResponse(
      statusCode = 200,
      reason = "OK",
      headers = req.headers.copy(contact = contactUri)
    )

  // -------------------------------------------------------------------------
  // INFO 200 OK
  // -------------------------------------------------------------------------

  def buildInfoOk(req: SipRequest): SipResponse =
    SipResponse(
      200,
      "OK",
      req.headers
        .copy(contact = contactUri, contentType = None, contentLength = 0)
    )

  // -------------------------------------------------------------------------
  // SDP answer (Mode B — offer was in 200, answer goes in ACK)
  // -------------------------------------------------------------------------

  def buildSdpAnswer: String =
    SdpCodec.buildAnswer(localIp, localRtpPort)

  private def authHeaderMap(
      authHeader: Option[(String, String)]
  ): Map[String, List[String]] =
    authHeader.fold(Map.empty) { case (name, value) =>
      Map(name -> List(value))
    }
}
