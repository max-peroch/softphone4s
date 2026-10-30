package softphone4s.fsm

import com.comcast.ip4s.{Host, IpAddress, Port}
import softphone4s.codec.{DigestAuth, SdpCodec}
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*

/** Builds the SIP messages of a call. Construct via
  * [[SipMessageBuilder.create]], which validates the account's user and realm
  * as a SIP URI.
  */
final class SipMessageBuilder private (
    config: SoftphoneConfig,
    localIp: IpAddress,
    val localSipPort: Port,
    val localRtpPort: Port,
    private[softphone4s] val localSipAddress: SipUri,
    contact: SipUri
) {
  val bindAddress: IpAddress = config.bindAddress
  private def contactUri     = s"<${contact.render}>"
  private def viaHost        = s"$localIp:$localSipPort"

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
    val sessionDescription = SdpCodec.buildOffer(localIp, localRtpPort)
    SipRequest(
      method = SipMethod.Invite,
      requestUri = callee,
      headers = SipHeaders(
        callId = callId,
        from = s"<${localSipAddress.render}>;tag=$localTag",
        to = s"<${callee.render}>",
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
        from = s"<${dialog.localUri.render}>;tag=${dialog.localTag}",
        to = s"<${dialog.remoteUri.render}>;tag=${dialog.remoteTag}",
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
      requestUri = ctx.callee,
      headers = SipHeaders(
        callId = ctx.callId,
        from = s"<${localSipAddress.render}>;tag=${ctx.localTag}",
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
      requestUri = ctx.callee,
      headers = SipHeaders(
        callId = ctx.callId,
        from = s"<${localSipAddress.render}>;tag=${ctx.localTag}",
        to = s"<${ctx.callee.render}>",
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
        from = s"<${dialog.localUri.render}>;tag=${dialog.localTag}",
        to = s"<${dialog.remoteUri.render}>;tag=${dialog.remoteTag}",
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

object SipMessageBuilder {

  /** Validates the account (`config.user` and `config.realm`) as a SIP URI and
    * the advertised Contact; fails with `InvalidSipUri` if either is unusable.
    */
  def create(
      config: SoftphoneConfig,
      localIp: IpAddress,
      localSipPort: Port,
      localRtpPort: Port
  ): Either[SoftphoneError, SipMessageBuilder] =
    for {
      user  <- User(config.user).left.map(invalid(config.user, _))
      realm <- Host
        .fromString(config.realm)
        .toRight(invalid(config.realm, "not a valid host"))
      address <- SipUri
        .from(SipScheme.Sip, realm, Some(UserInfo(user, None)))
        .left
        .map(invalid(config.user, _))
      contact <- SipUri
        .from(
          SipScheme.Sip,
          localIp,
          Some(UserInfo(user, None)),
          Some(localSipPort)
        )
        .left
        .map(invalid(config.user, _))
    } yield new SipMessageBuilder(
      config,
      localIp,
      localSipPort,
      localRtpPort,
      address,
      contact
    )

  /** The URI to dial: `callee` as given when it names a host
    * (`alice@sip.example.com`), otherwise at the account's `realm`. A `#` is
    * percent-encoded, as the user part of a SIP URI requires.
    */
  def calleeUri(
      callee: Callee,
      realm: String
  ): Either[SoftphoneError, SipUri] = {
    val user  = callee.value.replace("#", "%23")
    val input = if user.contains('@') then s"sip:$user" else s"sip:$user@$realm"
    SipUri.parse(input).left.map(invalid(input, _))
  }

  private def invalid(input: String, reason: String): SoftphoneError =
    SoftphoneError.InvalidSipUri(input, reason)
}
