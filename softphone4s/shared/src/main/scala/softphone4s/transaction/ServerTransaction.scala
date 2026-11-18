package softphone4s.transaction

import softphone4s.fsm.TimerAction
import softphone4s.model.{SipRequest, SipResponse}

/** What a server transaction (INVITE or non-INVITE) reacts to. */
enum ServerEvent {

  /** The request arrived again. */
  case RequestRetransmitted

  /** The transaction user produced a response. */
  case Respond(response: SipResponse)

  /** The ACK of a failure response (INVITE server transactions only). */
  case AckReceived

  case TimerFired(timer: TransactionTimer)
  case TransportError
}

/** What a server transaction asks its environment to do. */
enum ServerAction {

  /** Send a response (or retransmit the last one). */
  case Transmit(response: SipResponse)

  /** Hand the request to the transaction user. */
  case PassUp(request: SipRequest)

  case Timer(action: TimerAction[TransactionTimer])

  /** No ACK arrived for a failure response (Timer H): tell the user. */
  case AckTimedOut

  /** The transport failed: tell the user. */
  case TransportFailed
}
