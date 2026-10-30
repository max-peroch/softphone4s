package softphone4s.transaction

import softphone4s.fsm.TimerAction
import softphone4s.model.{SipRequest, SipResponse}

/** What a client transaction (INVITE or non-INVITE) reacts to. */
enum ClientEvent {
  case Response(response: SipResponse)
  case TimerFired(timer: TransactionTimer)
  case TransportError
}

/** What a client transaction asks its environment to do. */
enum ClientAction {

  /** Send the request (or retransmit it, or send the ACK for a failure). */
  case Transmit(request: SipRequest)

  /** Hand a response to the transaction user. */
  case PassUp(response: SipResponse)

  case Timer(action: TimerAction[TransactionTimer])

  /** No response arrived in time (Timer B or F): tell the user. */
  case TimedOut

  /** The transport failed: tell the user. */
  case TransportFailed
}
