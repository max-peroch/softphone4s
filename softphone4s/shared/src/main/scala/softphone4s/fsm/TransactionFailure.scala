package softphone4s.fsm

/** Why a client transaction ended without a final response. */
enum TransactionFailure {

  /** Timer B or F expired. */
  case TimedOut

  /** The transport could not send. */
  case TransportFailed
}
