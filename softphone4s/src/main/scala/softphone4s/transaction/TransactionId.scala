package softphone4s.transaction

/** A transaction in the table, on either side. */
enum TransactionId {
  case Client(key: ClientKey)
  case Server(key: ServerKey)
}

/** A timer of one transaction. */
final case class TimerKey(transaction: TransactionId, timer: TransactionTimer)
