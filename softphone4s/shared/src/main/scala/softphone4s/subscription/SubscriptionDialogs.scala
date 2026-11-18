package softphone4s.subscription

import softphone4s.model.*

/** The dialog of a subscription (RFC 6665 §4.4.1), which the 2xx to the
  * SUBSCRIBE makes or, if it comes first, the NOTIFY. Requests within it (the
  * refresh, the end) are addressed from it.
  */
object SubscriptionDialogs {

  /** From the 2xx to our SUBSCRIBE: we are the caller, so the route set is the
    * Record-Routes reversed and requests go to the notifier's Contact.
    */
  def accepted(
      context: SubscriptionContext,
      accountUri: SipUri,
      response: SipResponse
  ): SipDialog =
    SipDialog(
      callId = context.callId,
      localTag = context.localTag,
      remoteTag = response.headers.to.tag,
      localUri = accountUri,
      remoteUri = response.headers.to.uri,
      remoteTarget = response.headers.contacts.headOption
        .fold(response.headers.to.uri)(_.uri),
      localCseq = context.cseq,
      routeSet = response.headers.recordRoutes,
      role = DialogRole.Uac
    )

  /** From a NOTIFY that came before the 2xx: the dialog is made as the answerer
    * of a request makes it, with the route set as the NOTIFY had it. Absent if
    * the NOTIFY has no From tag, which a dialog cannot be had without.
    */
  def notified(
      context: SubscriptionContext,
      request: SipRequest
  ): Option[SipDialog] =
    request.headers.from.tag.map { remoteTag =>
      SipDialog(
        callId = context.callId,
        localTag = context.localTag,
        remoteTag = Some(remoteTag),
        localUri = request.headers.to.uri,
        remoteUri = request.headers.from.uri,
        remoteTarget = request.headers.contacts.headOption
          .fold(request.headers.from.uri)(_.uri),
        localCseq = context.cseq,
        routeSet = request.headers.recordRoutes,
        role = DialogRole.Uas,
        remoteCseq = Some(request.headers.cseq.seq)
      )
    }

  /** `dialog` with the notifier's Contact from a later NOTIFY, if it has one: a
    * subscription's target may be refreshed (RFC 6665 §4.4.1).
    */
  def retargeted(dialog: SipDialog, request: SipRequest): SipDialog =
    request.headers.contacts.headOption.fold(dialog)(contact =>
      dialog.copy(remoteTarget = contact.uri)
    )
}
