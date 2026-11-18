package softphone4s.registration

import softphone4s.fsm.TransactionFailure
import softphone4s.model.*

enum RegistrationEvent {

  /** Begin registering with these identifiers. */
  case Start(context: RegistrationContext)
  case Response(response: SipResponse)
  case TransactionFailed(failure: TransactionFailure)
  case TimerFired(timer: RegistrationTimer)

  /** The Contact we can be reached at has changed: a server has told us what we
    * look like from outside (RFC 3581), and it is not what we thought.
    */
  case AddressChanged(contact: SipUri)

  /** The registration is released: remove the binding and stop. */
  case Stop
}
