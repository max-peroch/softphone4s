package softphone4s.subscription

/** Why a request cannot be taken as a NOTIFY. */
enum NotifyError {
  case NotANotify
  case MissingEvent
  case BadEvent(text: String)
  case MissingSubscriptionState
  case BadSubscriptionState(text: String)

  /** A body with no `Content-Type`. */
  case MissingContentType

  def message: String = this match {
    case NotANotify                 => "not a NOTIFY"
    case MissingEvent               => "no Event header"
    case BadEvent(text)             => s"bad Event header: $text"
    case MissingSubscriptionState   => "no Subscription-State header"
    case BadSubscriptionState(text) => s"bad Subscription-State header: $text"
    case MissingContentType         => "a body without a Content-Type"
  }
}
