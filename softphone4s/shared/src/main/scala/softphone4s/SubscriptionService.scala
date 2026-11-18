package softphone4s

import cats.data.NonEmptyList
import cats.effect.{Async, Ref, Resource}
import fs2.concurrent.Channel
import org.typelevel.log4cats.StructuredLogger
import softphone4s.model.*
import softphone4s.runtime.Entropy
import softphone4s.subscription.EventHeader
import softphone4s.transaction.TransactionLayer
import softphone4s.ua.{DialogInput, Subscriber}

import scala.concurrent.duration.FiniteDuration

/** [[Subscriptions]] over the stack: the ways to reach the notifier come from
  * [[Uplinks]], the subscription from [[Subscriber]], and its NOTIFYs arrive
  * through the table by which [[IncomingRouter]] sends requests to their calls.
  */
private[softphone4s] final class SubscriptionService[F[_]: Async](
    val accounts: NonEmptyList[AccountId],
    accountNamed: AccountId => F[Account],
    uplinks: Uplinks[F],
    layer: TransactionLayer[F],
    routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
    entropy: Entropy[F],
    stopTimeout: FiniteDuration,
    logger: StructuredLogger[F]
) extends Subscriptions[F] {

  def subscribeAs(
      id: AccountId,
      callee: Callee,
      event: EventHeader,
      accept: List[MediaType],
      expires: Expires
  ): Resource[F, Subscription[F]] =
    for {
      account <- Resource.eval(accountNamed(id))
      target  <- Resource.eval(
        Async[F].fromEither(callee.toSipUri(account.config.realm))
      )
      ways         <- Resource.eval(uplinks.waysTo(account, target))
      subscription <- Subscriber.resource[F](
        layer,
        ways,
        routes,
        target,
        event,
        accept,
        expires,
        (transport, uri) => uplinks.nextHopOver(transport)(uri),
        entropy,
        stopTimeout,
        logger
      )
    } yield subscription
}
