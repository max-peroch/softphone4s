package softphone4s.transaction

import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import softphone4s.model.{SipMessage, SipRequest, SipResponse}
import softphone4s.transport.Inbound

/** A client transaction in progress, seen by its user. */
final case class ClientTransaction[F[_]](
    /** Responses passed up, then a timeout or transport failure if there is
      * one; ends when the transaction does.
      */
    notices: Stream[F, ClientNotice]
)

/** A server transaction in progress, seen by its user. */
final case class ServerTransaction[F[_]](
    request: SipRequest,
    from: SocketAddress[IpAddress],
    /** Sends a response through the transaction, which retransmits it as the
      * RFC requires. A response to a transaction that has ended is dropped.
      */
    respond: SipResponse => F[Unit],
    notices: Stream[F, ServerNotice]
)

/** RFC 3261 §17: matches messages to transactions, runs their state machines,
  * retransmits, and tells the transaction user what happened. One fiber owns
  * the table of transactions, so every event is handled in order.
  */
trait TransactionLayer[F[_]] {

  /** Sends `request` to `to` in a new client transaction. */
  def client(
      request: SipRequest,
      to: SocketAddress[IpAddress]
  ): F[Either[TransactionError, ClientTransaction[F]]]

  /** Requests that start a server transaction; answer through `respond`. */
  def incoming: Stream[F, ServerTransaction[F]]

  /** Messages no transaction claims, which the user agent must handle: the ACK
    * of a 2xx, a retransmitted 2xx, and messages whose Via has no usable
    * branch.
    */
  def unmatched: Stream[F, Inbound]

  /** Sends `message` outside any transaction, as for the ACK of a 2xx or the
    * retransmission of a 2xx (RFC 3261 §13).
    */
  def transmit(message: SipMessage, to: SocketAddress[IpAddress]): F[Unit]
}
