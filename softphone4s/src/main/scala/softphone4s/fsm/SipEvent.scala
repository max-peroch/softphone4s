package softphone4s.fsm

import softphone4s.model.{SipRequest, SipResponse}

/** What drives the outgoing call's state machine. */
enum SipEvent {
  case ResponseReceived(response: SipResponse)
  case RequestReceived(request: SipRequest)
  case HangupRequested
  case SessionChangeRequested(change: SessionChange)
  case TimerFired(timer: CallTimer)

  /** A client transaction of the call ended without a final response. */
  case TransactionFailed(failure: TransactionFailure)
}
