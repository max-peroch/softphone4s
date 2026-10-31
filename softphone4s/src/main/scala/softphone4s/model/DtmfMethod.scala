package softphone4s.model

/** How a digit was sent. */
enum DtmfMethod {

  /** As telephone-event packets in the media stream (RFC 4733), when the other
    * side agreed to receive them.
    */
  case Rfc4733

  /** In a SIP INFO request, when it did not, or there is no media to carry
    * them.
    */
  case SipInfo
}
