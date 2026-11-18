package softphone4s

import cats.data.NonEmptyList
import fs2.Stream
import softphone4s.messaging.{IncomingMessage, MessageContent, MessageFailure}
import softphone4s.model.{AccountId, Callee}

/** Instant messages (RFC 3428), sent and received. Reached as
  * `Softphone.messages`.
  */
trait Messaging[F[_]] {

  /** The accounts messages can be sent as, the default first. */
  def accounts: NonEmptyList[AccountId]

  /** Sends a message to `to` as `account`, and says whether it was delivered: a
    * 2xx is delivery, and anything else is a
    * [[softphone4s.messaging.MessageFailure]]. A challenge for credentials is
    * answered once. Each way of reaching the recipient is tried in turn when
    * one gets no answer. Fails with `UnknownAccount` if `account` is not one of
    * [[accounts]], and with `InvalidSipUri` if `to` names no address.
    */
  def sendAs(
      account: AccountId,
      to: Callee,
      content: MessageContent
  ): F[Either[MessageFailure, Unit]]

  /** Sends a message as the default account. */
  def send(
      to: Callee,
      content: MessageContent
  ): F[Either[MessageFailure, Unit]] =
    sendAs(accounts.head, to, content)

  /** Messages that arrive for any of the accounts, in the order they come;
    * `IncomingMessage.account` says which. Each is answered 200 once it is held
    * here, so that a message seen as delivered is a message the program has. Up
    * to 64 wait for a reader; beyond that the sender is told 503. A message
    * whose type we cannot read is answered 415 and does not appear.
    */
  def incoming: Stream[F, IncomingMessage]
}
