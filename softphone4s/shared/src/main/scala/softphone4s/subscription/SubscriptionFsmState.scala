package softphone4s.subscription

import softphone4s.model.Expires

enum SubscriptionFsmState {
  case Idle

  /** The first SUBSCRIBE is in flight. Its NOTIFY may come first (RFC 6665
    * §4.4.1), and is remembered in `early`.
    */
  case Establishing(context: SubscriptionContext, early: Option[Early])

  /** The 2xx has come, with `lifetime`; the first NOTIFY is awaited. */
  case AwaitingNotify(context: SubscriptionContext, lifetime: Expires)

  /** Subscribed, as the last NOTIFY said. */
  case Subscribed(
      context: SubscriptionContext,
      standing: Standing,
      lifetime: Expires
  )

  /** A refresh is in flight. */
  case Refreshing(
      context: SubscriptionContext,
      standing: Standing,
      lifetime: Expires
  )

  /** The SUBSCRIBE that ends the subscription is in flight. */
  case Unsubscribing(context: SubscriptionContext)

  /** It has been answered, and the last NOTIFY is awaited. */
  case Leaving(context: SubscriptionContext)

  case Ended(ending: Ending)
}
