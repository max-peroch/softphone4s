package softphone4s.fsm

import softphone4s.model.*

enum InboundEvent {
  case UserRinging
  case UserAccept
  case UserReject(status: FailureStatus)

  /** The application ends the call, whatever its state. */
  case HangupRequested

  /** The application asks to hold or resume the call. */
  case SessionChangeRequested(change: SessionChange)

  /** A request within the call: CANCEL, BYE, INFO, or something we do not
    * support.
    */
  case RequestReceived(request: SipRequest)

  /** The ACK of our 200 OK. */
  case AckReceived

  /** The response to our own BYE. */
  case ResponseReceived(response: SipResponse)
  case TransactionFailed(failure: TransactionFailure)
  case TimerFired(timer: CallTimer)
}
