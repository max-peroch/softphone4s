package softphone4s.transaction

import softphone4s.model.SipMethod

/** Why a request could not start a client transaction. */
enum TransactionError {
  case InvalidKey(error: TransactionKeyError)

  /** A transaction with this key is already running. */
  case Duplicate(key: ClientKey)

  /** An ACK has no transaction of its own: for a 2xx it belongs to the dialog,
    * for a failure to the INVITE transaction.
    */
  case NoTransactionFor(method: SipMethod)

  def message: String = this match {
    case InvalidKey(error) => error.message
    case Duplicate(key)    =>
      s"a ${key.method} transaction with branch ${key.branch.value} is already running"
    case NoTransactionFor(method) => s"$method has no transaction of its own"
  }
}
