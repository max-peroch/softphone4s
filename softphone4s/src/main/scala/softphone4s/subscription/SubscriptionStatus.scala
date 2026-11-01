package softphone4s.subscription

import scala.concurrent.duration.FiniteDuration

/** Where a subscription stands, as told to the program. */
enum SubscriptionStatus {

  /** Asking, or waiting for the first NOTIFY. */
  case Subscribing

  /** Not yet authorised by the notifier. */
  case Pending(expiresIn: FiniteDuration)

  /** Authorised: NOTIFYs come as the state changes. */
  case Active(expiresIn: FiniteDuration)

  /** We have asked to end it. */
  case Leaving

  /** The dialog ended in a way that allows another, which begins after `in`. */
  case Retrying(because: Ending, in: FiniteDuration)

  /** Over for good: the subscription was ended, or will not be had again. */
  case Ended(ending: Ending)
}
