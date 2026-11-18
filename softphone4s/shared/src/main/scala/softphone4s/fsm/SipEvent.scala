package softphone4s.fsm

import softphone4s.model.{DtmfDigit, SipRequest, SipResponse}

import scala.concurrent.duration.FiniteDuration

/** What drives the outgoing call's state machine. */
enum SipEvent {
  case ResponseReceived(response: SipResponse)
  case RequestReceived(request: SipRequest)
  case HangupRequested
  case SessionChangeRequested(change: SessionChange)

  /** The application sends a digit in a SIP INFO. */
  case DtmfRequested(digit: DtmfDigit, duration: FiniteDuration)
  case TimerFired(timer: CallTimer)

  /** A client transaction of the call ended without a final response. */
  case TransactionFailed(failure: TransactionFailure)
}
