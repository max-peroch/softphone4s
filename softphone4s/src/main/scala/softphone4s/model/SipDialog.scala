package softphone4s.model

/** Context kept while an INVITE is in flight, before a dialog is established.
  */
case class InviteCtx(
    callId: String,
    localTag: String,
    cseq: Int,
    callee: String,
    localUri: String,
    offerInInvite: Boolean // false → INVITE had no SDP, answer goes in ACK
)

/** Sufficient state to reconstruct correct headers for all mid-dialog requests
  * (BYE, re-INVITE, ACK to re-INVITE).
  *
  * @param remoteTarget
  *   Contact URI from the remote's 200 OK — used as request-URI for subsequent
  *   in-dialog requests.
  * @param routeSet
  *   Record-Route values from the 200 OK in the order they appeared. For a UAC
  *   these are reversed when building the Route header (RFC 3261 §12.1.2).
  */
case class SipDialog(
    callId: String,
    localTag: String,
    remoteTag: String,
    localUri: String,
    remoteUri: String,
    remoteTarget: String,
    localCseq: Int,
    routeSet: List[String],
    isSecure: Boolean = false
) {
  def routeHeader: List[String] = routeSet.reverse
}

object SipDialog {

  def fromProvisional(ctx: InviteCtx, r: SipResponse): SipDialog =
    SipDialog(
      callId = ctx.callId,
      localTag = ctx.localTag,
      remoteTag = tagOf(r.headers.to),
      localUri = ctx.localUri,
      remoteUri = uriOf(r.headers.to),
      remoteTarget = uriOf(r.headers.to),
      localCseq = ctx.cseq,
      routeSet = r.headers.recordRoute
    )

  def fromOk(ctx: InviteCtx, r: SipResponse): SipDialog =
    SipDialog(
      callId = ctx.callId,
      localTag = ctx.localTag,
      remoteTag = tagOf(r.headers.to),
      localUri = ctx.localUri,
      remoteUri = uriOf(r.headers.to),
      remoteTarget = contactUri(r.headers).getOrElse(uriOf(r.headers.to)),
      localCseq = ctx.cseq,
      routeSet = r.headers.recordRoute
    )

  private def tagOf(headerValue: String): String =
    headerValue
      .split(";")
      .map(_.trim)
      .find(_.startsWith("tag="))
      .map(_.stripPrefix("tag="))
      .getOrElse("")

  private def uriOf(headerValue: String): String = {
    val v = headerValue.trim
    if v.startsWith("<") then v.stripPrefix("<").takeWhile(_ != '>')
    else v.split(";").head.trim
  }

  private def contactUri(headers: SipHeaders): Option[String] =
    Option(headers.contact)
      .filter(_.nonEmpty)
      .map(v =>
        if v.trim.startsWith("<") then
          v.trim.stripPrefix("<").takeWhile(_ != '>')
        else v.trim
      )
}
