package softphone4s.registration

import softphone4s.model.*

/** What a registration remembers between REGISTERs. */
final case class RegistrationContext(
    callId: CallId,
    localTag: Tag,
    cseq: CSeqNumber,
    contact: SipUri,
    requested: Expires,
    authAttempts: Map[AuthKind, Int] = Map.empty,
    cached: Option[(AuthKind, DigestChallenge)] = None
) {

  /** The context for a new REGISTER: the next CSeq, and no challenge answered
    * yet (RFC 3261 §10.2: one Call-ID, a rising CSeq). The credentials that
    * last worked are kept, so the REGISTER can carry them at once instead of
    * waiting for a 401 (RFC 7616 §3.3: a nonce may be reused with a higher
    * `nc`).
    */
  def next: RegistrationContext =
    copy(cseq = cseq.next, authAttempts = Map.empty)
}
