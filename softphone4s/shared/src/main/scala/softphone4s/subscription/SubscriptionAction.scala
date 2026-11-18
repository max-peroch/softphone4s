package softphone4s.subscription

import softphone4s.fsm.TimerAction
import softphone4s.model.*

enum SubscriptionAction {

  /** Send a SUBSCRIBE for `expires`, answering `authorization` if present. In
    * the dialog if `context.remoteTag` is known, else outside any.
    */
  case SendSubscribe(
      context: SubscriptionContext,
      expires: Expires,
      authorization: Option[(AuthKind, DigestChallenge)]
  )

  /** Answer a NOTIFY. */
  case Respond(request: SipRequest, status: Int, reason: String)

  /** Hand a NOTIFY's content to the program. */
  case Deliver(notification: Notification)

  /** Tell the program where the subscription stands. */
  case Report(status: SubscriptionStatus)

  case Timer(action: TimerAction[SubscriptionTimer])
}
