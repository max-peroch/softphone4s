package softphone4s.subscription

import softphone4s.model.*

/** What a subscription remembers between its SUBSCRIBEs: the dialog's
  * identifiers (one Call-ID and local tag, a rising CSeq), the event, and what
  * is needed to answer a challenge.
  *
  * @param remoteTag
  *   the notifier's tag, once a 2xx or a NOTIFY has given it.
  * @param remoteCseq
  *   the CSeq of the last NOTIFY taken, so that one out of order is told from
  *   one in order.
  * @param refreshFailures
  *   refreshes that failed since the last that did not.
  */
final case class SubscriptionContext(
    callId: CallId,
    localTag: Tag,
    remoteTag: Option[Tag],
    cseq: CSeqNumber,
    event: EventHeader,
    requested: Expires,
    remoteCseq: Option[Long] = None,
    refreshFailures: Int = 0,
    authAttempts: Map[AuthKind, Int] = Map.empty,
    cached: Option[(AuthKind, DigestChallenge)] = None
) {

  /** The context for a new SUBSCRIBE in the dialog: the next CSeq, and no
    * challenge answered yet. The credentials that last worked are kept.
    */
  def next: SubscriptionContext =
    copy(cseq = cseq.next, authAttempts = Map.empty)
}
