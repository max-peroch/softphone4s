package softphone4s.transaction

import softphone4s.model.SipMethod

/** Which end of a transaction. */
enum TransactionSide {

  /** The one that sent the request. */
  case Client

  /** The one that received it. */
  case Server
}

/** What the transaction layer tells of the trouble it has with the network. */
enum TransactionEvent {

  /** A request (client) or a response (server) was sent again, because nothing
    * had come back, or because the other side sent its request again.
    */
  case Retransmitted(side: TransactionSide, method: SipMethod)

  /** A transaction ended without what it waited for: the final response of a
    * client transaction, the ACK of a failure of a server one.
    */
  case TimedOut(side: TransactionSide, method: SipMethod)
}
