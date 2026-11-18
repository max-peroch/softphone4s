package softphone4s.publication

import softphone4s.fsm.TransactionFailure
import softphone4s.model.*

enum PublicationEvent {

  /** Begin publishing the document of this context. */
  case Start(context: PublicationContext)

  /** The program has a new state to publish. */
  case Update(document: PublicationBody)

  /** A response to our PUBLISH. */
  case Response(response: SipResponse)

  case TransactionFailed(failure: TransactionFailure)

  case TimerFired(timer: PublicationTimer)

  /** The publication is released: remove the state. */
  case Stop
}
