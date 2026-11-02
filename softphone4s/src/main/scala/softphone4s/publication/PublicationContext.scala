package softphone4s.publication

import softphone4s.model.*
import softphone4s.subscription.EventHeader

/** What a publication remembers between its PUBLISHes: the identifiers (one
  * Call-ID and tag, a rising CSeq), the event, the state it means to publish,
  * and what is needed to answer a challenge.
  *
  * @param requested
  *   the lifetime asked for, which a `423` may raise.
  * @param document
  *   the state to publish, the latest the program gave.
  * @param dirty
  *   whether `document` has changed since the last PUBLISH that carried it, so
  *   that another is owed once the one in flight is answered.
  * @param stopping
  *   whether the program has asked to end it, to be done once the tag the
  *   notifier gives is known.
  * @param refreshFailures
  *   requests that failed since the last that did not.
  */
final case class PublicationContext(
    callId: CallId,
    localTag: Tag,
    cseq: CSeqNumber,
    event: EventHeader,
    requested: Expires,
    document: PublicationBody,
    dirty: Boolean = false,
    stopping: Boolean = false,
    refreshFailures: Int = 0,
    authAttempts: Map[AuthKind, Int] = Map.empty,
    cached: Option[(AuthKind, DigestChallenge)] = None
) {

  /** The context for a new PUBLISH: the next CSeq, and no challenge answered
    * yet. The credentials that last worked are kept.
    */
  def next: PublicationContext =
    copy(cseq = cseq.next, authAttempts = Map.empty)
}
