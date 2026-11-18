package softphone4s.ua

/** What the ACK to a 2xx is expected to carry. */
private[softphone4s] enum AckContent {

  /** Nothing of interest: the offer was in the INVITE and answered in the 2xx.
    */
  case Empty

  /** Our answer-to-be: the 2xx carried our offer, so the ACK has the answer. */
  case Answer
}
