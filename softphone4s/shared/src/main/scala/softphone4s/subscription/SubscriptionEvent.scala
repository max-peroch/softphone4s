package softphone4s.subscription

import softphone4s.fsm.TransactionFailure
import softphone4s.model.*

enum SubscriptionEvent {

  /** Begin subscribing with these identifiers. */
  case Start(context: SubscriptionContext)

  /** A response to our SUBSCRIBE. */
  case Response(response: SipResponse)

  case TransactionFailed(failure: TransactionFailure)

  /** A NOTIFY of this dialog and event, which the program has read. */
  case NotifyReceived(request: SipRequest, info: NotifyInfo)

  case TimerFired(timer: SubscriptionTimer)

  /** The subscription is released: end it. */
  case Stop
}
