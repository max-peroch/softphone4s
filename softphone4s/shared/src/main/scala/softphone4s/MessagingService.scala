package softphone4s

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import softphone4s.messaging.{IncomingMessage, MessageContent, MessageFailure}
import softphone4s.model.{AccountId, Callee}
import softphone4s.ua.MessageSender

/** [[Messaging]] over the stack: the way to reach a recipient comes from
  * [[Uplinks]], the delivery from [[MessageSender]], and what arrives from the
  * [[IncomingRouter]].
  */
private[softphone4s] final class MessagingService[F[_]: Async](
    val accounts: NonEmptyList[AccountId],
    accountNamed: AccountId => F[Account],
    uplinks: Uplinks[F],
    sender: MessageSender[F],
    arrivals: Stream[F, IncomingMessage]
) extends Messaging[F] {

  def sendAs(
      id: AccountId,
      to: Callee,
      content: MessageContent
  ): F[Either[MessageFailure, Unit]] =
    for {
      account <- accountNamed(id)
      target  <- Async[F].fromEither(to.toSipUri(account.config.realm))
      found   <- uplinks.allTargets(account, target)
      result  <- sender.deliver(
        found.toList.map(uplinks.uplinkTo(account, target, _)),
        target,
        content
      )
    } yield result

  def incoming: Stream[F, IncomingMessage] = arrivals
}
