package softphone4s.model

/** Context kept while an INVITE is in flight, before a dialog is established.
  */
final case class InviteCtx(
    callId: String,
    localTag: String,
    cseq: Int,
    callee: SipUri,
    localUri: SipUri,
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
final case class SipDialog(
    callId: String,
    localTag: String,
    remoteTag: String,
    localUri: SipUri,
    remoteUri: SipUri,
    remoteTarget: SipUri,
    localCseq: Int,
    routeSet: List[String],
    isSecure: Boolean = false
) {
  def routeHeader: List[String] = routeSet.reverse
}

object SipDialog {

  // The remote URI falls back to the one we dialled when the response's To
  // header does not parse.
  def fromProvisional(ctx: InviteCtx, r: SipResponse): SipDialog = {
    val remote = SipUri.fromHeaderValue(r.headers.to).getOrElse(ctx.callee)
    SipDialog(
      callId = ctx.callId,
      localTag = ctx.localTag,
      remoteTag = tagOf(r.headers.to),
      localUri = ctx.localUri,
      remoteUri = remote,
      remoteTarget = remote,
      localCseq = ctx.cseq,
      routeSet = r.headers.recordRoute
    )
  }

  def fromOk(ctx: InviteCtx, r: SipResponse): SipDialog = {
    val remote = SipUri.fromHeaderValue(r.headers.to).getOrElse(ctx.callee)
    SipDialog(
      callId = ctx.callId,
      localTag = ctx.localTag,
      remoteTag = tagOf(r.headers.to),
      localUri = ctx.localUri,
      remoteUri = remote,
      remoteTarget = contactUri(r.headers).getOrElse(remote),
      localCseq = ctx.cseq,
      routeSet = r.headers.recordRoute
    )
  }

  /** The dialog of a 200 OK that arrives after the call was cancelled, when the
    * INVITE context is gone: everything is read back from the response, whose
    * `From` is ours and whose `To` is the remote party (RFC 3261 §9.1).
    */
  def fromLateOk(r: SipResponse): Option[SipDialog] =
    for {
      local  <- SipUri.fromHeaderValue(r.headers.from).toOption
      remote <- SipUri.fromHeaderValue(r.headers.to).toOption
    } yield SipDialog(
      callId = r.headers.callId,
      localTag = tagOf(r.headers.from),
      remoteTag = tagOf(r.headers.to),
      localUri = local,
      remoteUri = remote,
      remoteTarget = contactUri(r.headers).getOrElse(remote),
      localCseq = r.headers.cseq.seq,
      routeSet = r.headers.recordRoute
    )

  private def tagOf(headerValue: String): String =
    headerValue
      .split(";")
      .map(_.trim)
      .find(_.startsWith("tag="))
      .map(_.stripPrefix("tag="))
      .getOrElse("")

  private def contactUri(headers: SipHeaders): Option[SipUri] =
    Option(headers.contact)
      .filter(_.nonEmpty)
      .flatMap(SipUri.fromHeaderValue(_).toOption)
}
