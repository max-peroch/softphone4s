package softphone4s

import cats.data.NonEmptyList
import cats.effect.{Async, Resource}
import cats.syntax.all.*
import org.typelevel.log4cats.StructuredLogger
import softphone4s.model.*
import softphone4s.presence.Presence
import softphone4s.publication.PublicationBody
import softphone4s.runtime.Entropy
import softphone4s.subscription.EventHeader
import softphone4s.transaction.TransactionLayer
import softphone4s.ua.Publisher

import scala.concurrent.duration.FiniteDuration

/** [[Publications]] over the stack: the ways to reach the server come from
  * [[Uplinks]] and the publication from [[Publisher]].
  */
private[softphone4s] final class PublicationService[F[_]: Async](
    val accounts: NonEmptyList[AccountId],
    accountNamed: AccountId => F[Account],
    uplinks: Uplinks[F],
    layer: TransactionLayer[F],
    entropy: Entropy[F],
    stopTimeout: FiniteDuration,
    logger: StructuredLogger[F]
) extends Publications[F] {

  def publishAs(
      id: AccountId,
      event: EventHeader,
      document: PublicationBody,
      expires: Expires
  ): Resource[F, Publication[F]] =
    for {
      account <- Resource.eval(accountNamed(id))
      // The state is that of the account's own address.
      presentity = id.addressOfRecord
      ways        <- Resource.eval(uplinks.waysTo(account, presentity))
      publication <- Publisher.resource[F](
        layer,
        ways,
        presentity,
        event,
        document,
        expires,
        entropy,
        stopTimeout,
        logger
      )
    } yield publication

  def publishPresenceAs(
      id: AccountId,
      presence: Presence,
      expires: Expires
  ): Resource[F, Publication[F]] =
    for {
      document <- Resource.eval(
        Async[F].fromEither(
          Presence
            .document(presence)
            .leftMap(SoftphoneError.InvalidPublication(_))
        )
      )
      publication <- publishAs(id, EventHeader.Presence, document, expires)
    } yield publication
}
