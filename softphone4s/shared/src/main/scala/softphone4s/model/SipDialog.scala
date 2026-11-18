package softphone4s.model

import scala.concurrent.duration.FiniteDuration

/** Context kept while an INVITE is in flight, before a dialog is established.
  */
final case class InviteCtx(
    callId: CallId,
    localTag: Tag,
    cseq: CSeqNumber,
    callee: SipUri,
    localUri: SipUri,
    offerInInvite: Boolean, // false → INVITE had no SDP, answer goes in ACK
    inviteTimeout: FiniteDuration, // how long to wait for an answer (Timer B)
    authAttempts: Map[AuthKind, Int] = Map.empty // challenges answered so far
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
    localCseq: CSeqNumber,
    routeSet: List[NameAddr],
    isSecure: Boolean = false,
    role: DialogRole = DialogRole.Uac,
    remoteCseq: Option[CSeqNumber] = None
) {

  /** The Route headers of a request within the dialog: the route set reversed
    * for the party that placed the INVITE, as received for the one that
    * answered it (RFC 3261 §12.1).
    */
  def routeHeader: List[NameAddr] = role match {
    case DialogRole.Uac => routeSet.reverse
    case DialogRole.Uas => routeSet
  }

  /** Where a request within the dialog goes, and how it is addressed (RFC 3261
    * §12.2.1.1). With no route set it goes to the remote target. With a loose
    * router first (`lr`) the Request-URI stays the remote target, the whole
    * route set becomes Route headers, and the first route is the next hop. With
    * a strict router first, its URI becomes the Request-URI and the remote
    * target is appended to the routes.
    */
  def route: RequestRoute = routeHeader match {
    case Nil => RequestRoute(remoteTarget, Nil, remoteTarget)
    case routes @ (first :: _) if first.uri.isLooseRouting =>
      RequestRoute(remoteTarget, routes, first.uri)
    case first :: rest =>
      RequestRoute(first.uri, rest :+ NameAddr(remoteTarget), first.uri)
  }

  /** Whether a request from the remote party belongs to this dialog: our tag in
    * its To and theirs in its From (RFC 3261 §12.2.2: otherwise `481`).
    */
  def matches(request: SipRequest): Boolean =
    request.headers.to.tag.contains(localTag) &&
      remoteTag.forall(t => request.headers.from.tag.contains(t))

  /** Whether a request with this CSeq is newer than every one seen so far (RFC
    * 3261 §12.2.2); a first request always is.
    */
  def accepts(cseq: CSeqNumber): Boolean =
    remoteCseq.forall(_.value < cseq.value)

  def seen(cseq: CSeqNumber): SipDialog = copy(remoteCseq = Some(cseq))
}

object SipDialog {

  /** The dialog the answerer of `invite` forms (RFC 3261 §12.1.1): our tag
    * becomes the To-tag, the caller's From-tag is the remote tag, and requests
    * go to the caller's Contact. Absent if the caller sent no From-tag, which a
    * dialog cannot be built without.
    */
  def forInboundInvite(invite: SipRequest, localTag: Tag): Option[SipDialog] =
    invite.headers.from.tag.map { remoteTag =>
      SipDialog(
        callId = invite.headers.callId,
        localTag = localTag,
        remoteTag = Some(remoteTag),
        localUri = invite.headers.to.uri,
        remoteUri = invite.headers.from.uri,
        remoteTarget = invite.headers.contacts.headOption
          .fold(invite.headers.from.uri)(_.uri),
        localCseq = CSeqNumber.One,
        routeSet = invite.headers.recordRoutes,
        role = DialogRole.Uas,
        remoteCseq = Some(invite.headers.cseq.seq)
      )
    }

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
