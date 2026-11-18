package softphone4s.subscription

/** Why a notifier ended a subscription: the `reason` of a `terminated`
  * `Subscription-State` (RFC 6665 §4.2.2), with what each lets the subscriber
  * do.
  */
enum EndReason {

  /** The subscription is over for now; subscribe again at once. */
  case Deactivated

  /** The notifier is not ready; subscribe again after the `retry-after`. */
  case Probation

  /** The subscription was refused; do not subscribe again. */
  case Rejected

  /** It ran out; subscribe again at once if it is still wanted. */
  case Timeout

  /** The notifier can no longer give the state; subscribe again after the
    * `retry-after`, if there is one.
    */
  case GiveUp

  /** The resource is gone; do not subscribe again. */
  case NoResource

  /** The state will not change again (RFC 6665 §4.2.2). */
  case Invariant

  /** The NOTIFY gave no reason; treated as a refusal. */
  case Unspecified

  /** A reason this stack does not know; treated as a refusal. */
  case Other(text: String)
}

object EndReason {

  def parse(text: String): EndReason = text.trim.toLowerCase match {
    case "deactivated" => Deactivated
    case "probation"   => Probation
    case "rejected"    => Rejected
    case "timeout"     => Timeout
    case "giveup"      => GiveUp
    case "noresource"  => NoResource
    case "invariant"   => Invariant
    case other         => Other(other)
  }
}
