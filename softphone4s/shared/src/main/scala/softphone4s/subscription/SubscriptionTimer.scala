package softphone4s.subscription

enum SubscriptionTimer {

  /** Time to refresh the subscription. */
  case Refresh

  /** The subscription's lifetime is over. */
  case Expiry

  /** The first NOTIFY has not come. */
  case NotifyWait

  /** The last NOTIFY of a subscription we ended has not come. */
  case LeaveWait
}
