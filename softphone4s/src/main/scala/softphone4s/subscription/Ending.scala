package softphone4s.subscription

import scala.concurrent.duration.*

/** How a subscription's dialog ended, which says whether to subscribe again. */
enum Ending {

  /** We ended it. */
  case Unsubscribed

  /** The notifier ended it with a `terminated` NOTIFY. */
  case Terminated(reason: EndReason, retryAfter: Option[FiniteDuration])

  /** A SUBSCRIBE was refused for good, such as with 403 or 489. */
  case Rejected(status: Int, reason: String)

  /** It could not be had; trying again may work. */
  case Failed(failure: SubscriptionFailure)

  /** Its lifetime ran out with no refresh got through. */
  case Expired

  /** The notifier no longer knows the dialog (481). */
  case DialogLost

  /** How long to wait before subscribing again, if it is to be done at all.
    * `backoff` is the wait when the ending names none, for the attempt we are
    * on.
    */
  def resubscribeAfter(backoff: FiniteDuration): Option[FiniteDuration] =
    this match {
      case Unsubscribed | Rejected(_, _)  => None
      case Failed(_)                      => Some(backoff)
      case Expired | DialogLost           => Some(Duration.Zero)
      case Terminated(reason, retryAfter) =>
        reason match {
          case EndReason.Deactivated | EndReason.Timeout => Some(Duration.Zero)
          case EndReason.Probation | EndReason.GiveUp    =>
            Some(retryAfter.getOrElse(backoff))
          case EndReason.Rejected | EndReason.NoResource | EndReason.Invariant |
              EndReason.Other(_) =>
            None
        }
    }
}
