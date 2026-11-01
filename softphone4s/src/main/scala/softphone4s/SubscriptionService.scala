package softphone4s

import cats.data.NonEmptyList
import cats.effect.{Async, Ref, Resource}
import cats.syntax.all.*
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
      found <- Resource.eval(uplinks.allTargets(account, target))
      // The first way must work; the others are there for when it does not.
      first  <- Resource.eval(uplinks.uplinkTo(account, target, found.head))
      others <- Resource.eval(
        found.tail.flatTraverse(other =>
          uplinks
            .uplinkTo(account, target, other)
            .map(List(_))
            .handleErrorWith(error =>
              logger
                .debug(
                  Map(
                    "target" -> other.address.toString,
                    "error"  -> String.valueOf(error.getMessage)
                  )
                )("Not using this target as a fallback")
                .as(Nil)
            )
        )
      )
      subscription <- Subscriber.resource[F](
        layer,
        NonEmptyList(first, others),
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
