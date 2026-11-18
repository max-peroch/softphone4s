package softphone4s.fsm

/** Why a call ended. */
enum TerminationReason {
  case LocalHangup
  case RemoteBye
  case Rejected(code: Int, reason: String)
  case TimedOut
  case TransportFailure

  /** The caller cancelled an incoming call before it was answered. */
  case CancelledByCaller

  /** We turned an incoming call away. */
  case RejectedLocally
}
