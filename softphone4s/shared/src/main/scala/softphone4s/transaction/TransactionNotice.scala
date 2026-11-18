package softphone4s.transaction

import softphone4s.model.SipResponse

/** What a client transaction tells its user. */
enum ClientNotice {
  case Response(response: SipResponse)

  /** No response in time (Timer B or F). */
  case TimedOut

  case TransportFailed
}

/** What a server transaction tells its user besides the request itself. */
enum ServerNotice {

  /** No ACK came for a failure response (Timer H). */
  case AckTimedOut

  case TransportFailed
}
