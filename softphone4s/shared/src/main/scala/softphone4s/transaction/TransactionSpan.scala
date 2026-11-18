package softphone4s.transaction

/** The span of one transaction, from its request to its final answer. */
trait TransactionSpan[F[_]] {

  /** The transaction has its final answer, or has none to wait for. Called once
    * per span, as the transaction layer sees it.
    */
  def end(outcome: TransactionOutcome): F[Unit]
}
