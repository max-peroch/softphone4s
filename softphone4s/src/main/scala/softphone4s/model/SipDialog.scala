package softphone4s.model

/** Context kept while an INVITE is in flight, before a dialog is established.
  */
final case class InviteCtx(
    callId: CallId,
    localTag: Tag,
    cseq: Int,
    callee: SipUri,
    localUri: SipUri,
    offerInInvite: Boolean // false → INVITE had no SDP, answer goes in ACK
)

/** Sufficient state to reconstruct correct headers for all mid-dialog requests
  * (BYE, re-INVITE, ACK to re-INVITE).
  *
  * @param remoteTag
  *   The To-tag of the remote party; absent on a provisional response without
  *   one.
  * @param remoteTarget
  *   Contact URI from the remote's 200 OK — used as request-URI for subsequent
  *   in-dialog requests.
  * @param routeSet
  *   Record-Route values from the 200 OK in the order they appeared. For a UAC
  *   these are reversed when building the Route header (RFC 3261 §12.1.2).
  */
final case class SipDialog(
    callId: CallId,
    localTag: Tag,
    remoteTag: Option[Tag],
    localUri: SipUri,
    remoteUri: SipUri,
    remoteTarget: SipUri,
    localCseq: Int,
    routeSet: List[NameAddr],
    isSecure: Boolean = false
) {
  def routeHeader: List[NameAddr] = routeSet.reverse
}

object SipDialog {

  def fromProvisional(ctx: InviteCtx, r: SipResponse): SipDialog =
    SipDialog(
      callId = ctx.callId,
      localTag = ctx.localTag,
      remoteTag = r.headers.to.tag,
      localUri = ctx.localUri,
      remoteUri = r.headers.to.uri,
      remoteTarget = r.headers.to.uri,
      localCseq = ctx.cseq,
      routeSet = r.headers.recordRoutes
    )

  def fromOk(ctx: InviteCtx, r: SipResponse): SipDialog =
    SipDialog(
      callId = ctx.callId,
      localTag = ctx.localTag,
      remoteTag = r.headers.to.tag,
      localUri = ctx.localUri,
      remoteUri = r.headers.to.uri,
      remoteTarget =
        r.headers.contacts.headOption.fold(r.headers.to.uri)(_.uri),
      localCseq = ctx.cseq,
      routeSet = r.headers.recordRoutes
    )

  /** The dialog of a 200 OK that arrives after the call was cancelled, when the
    * INVITE context is gone: everything is read back from the response, whose
    * `From` is ours and whose `To` is the remote party (RFC 3261 §9.1). Absent
    * if our From tag is missing.
    */
  def fromLateOk(r: SipResponse): Option[SipDialog] =
    r.headers.from.tag.map { localTag =>
      SipDialog(
        callId = r.headers.callId,
        localTag = localTag,
        remoteTag = r.headers.to.tag,
        localUri = r.headers.from.uri,
        remoteUri = r.headers.to.uri,
        remoteTarget =
          r.headers.contacts.headOption.fold(r.headers.to.uri)(_.uri),
        localCseq = r.headers.cseq.seq,
        routeSet = r.headers.recordRoutes
      )
    }
}
