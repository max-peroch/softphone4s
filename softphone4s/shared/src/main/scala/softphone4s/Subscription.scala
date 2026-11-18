package softphone4s

import fs2.Stream
import fs2.concurrent.Signal
import softphone4s.subscription.{Ending, Notification, SubscriptionStatus}

/** A subscription (RFC 6665) kept alive for as long as the resource that holds
  * it: refreshed before it lapses, and begun again, in a new dialog, when it
  * ends in a way that allows it. Releasing it ends the subscription.
  */
trait Subscription[F[_]] {

  /** What the notifier sends as the state changes, in order. The most recent 64
    * wait for a reader. Ends when the subscription does for good.
    */
  def notifications: Stream[F, Notification]

  /** Where the subscription stands, and every change to it. */
  def status: Signal[F, SubscriptionStatus]

  /** Completes once the subscription is active, with how it ended instead if it
    * ended for good first.
    */
  def awaitActive: F[Either[Ending, Unit]]
}
