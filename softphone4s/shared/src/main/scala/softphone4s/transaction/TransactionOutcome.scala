package softphone4s.transaction

/** How the wait of a transaction for its final answer came out. */
enum TransactionOutcome {

  /** A client transaction received a final response. */
  case Answered(status: Int)

  /** A server transaction sent a final response. */
  case Responded(status: Int)

  /** No final response came (client), or no ACK for a failure (server). */
  case TimedOut

  /** The request or the response could not be sent. */
  case SendFailed

  /** The transaction ended with no final answer, for none of the above. */
  case Abandoned
}
