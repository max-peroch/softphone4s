package softphone4s.subscription

/** Why a subscription could not be had, where trying again may help. */
enum SubscriptionFailure {
  case TimedOut
  case TransportFailed

  /** A SUBSCRIBE answered with a status that may pass, such as 503 or 408. */
  case Refused(status: Int, reason: String)

  /** The SUBSCRIBE was accepted and no NOTIFY followed (RFC 6665 §4.1.2.4). */
  case NoNotify
}
