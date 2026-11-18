package softphone4s.ua

import softphone4s.transaction.ServerTransaction
import softphone4s.transport.Inbound

/** What reaches a call from the transaction layer besides the responses to its
  * own client transactions.
  */
private[softphone4s] sealed trait DialogInput[F[_]]

private[softphone4s] object DialogInput {

  /** A request within the dialog (BYE, INFO), in its server transaction. */
  final case class Request[F[_]](transaction: ServerTransaction[F])
      extends DialogInput[F]

  /** A message no transaction claimed, such as a retransmitted 2xx. */
  final case class Unmatched[F[_]](inbound: Inbound) extends DialogInput[F]
}
