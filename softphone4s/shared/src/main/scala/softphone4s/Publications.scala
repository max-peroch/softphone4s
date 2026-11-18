package softphone4s

import cats.data.NonEmptyList
import cats.effect.Resource
import softphone4s.model.{AccountId, Expires}
import softphone4s.presence.Presence
import softphone4s.publication.PublicationBody
import softphone4s.subscription.EventHeader

import scala.concurrent.duration.*

/** State published about ourselves (RFC 3903): presence, and any other event
  * package. Reached as `Softphone.publications`. The state is that of the
  * account's own address.
  */
trait Publications[F[_]] {

  /** The accounts state can be published as, the default first. */
  def accounts: NonEmptyList[AccountId]

  /** Publishes `document` as the state of `account` for `event` for as long as
    * the resource is held: acquiring it sends the first PUBLISH and returns at
    * once, with a [[Publication]] whose `awaitPublished` says when the notifier
    * holds the state. The state is refreshed before it lapses and published
    * again if the notifier loses it; releasing the resource removes it.
    *
    * @param expires
    *   the lifetime to ask for; the notifier may grant another.
    *
    * Fails with `UnknownAccount` if `account` is not one of [[accounts]].
    */
  def publishAs(
      account: AccountId,
      event: EventHeader,
      document: PublicationBody,
      expires: Expires = Publications.DefaultExpires
  ): Resource[F, Publication[F]]

  /** Publishes as the default account. */
  def publish(
      event: EventHeader,
      document: PublicationBody,
      expires: Expires = Publications.DefaultExpires
  ): Resource[F, Publication[F]] =
    publishAs(accounts.head, event, document, expires)

  /** Publishes `presence` as `account`. It is changed with
    * `Publication.updatePresence`.
    *
    * Fails with `InvalidPublication` if `presence` cannot be written as PIDF,
    * and with `UnknownAccount` if `account` is not one of [[accounts]].
    */
  def publishPresenceAs(
      account: AccountId,
      presence: Presence,
      expires: Expires = Publications.DefaultExpires
  ): Resource[F, Publication[F]]

  /** Publishes presence as the default account. */
  def publishPresence(
      presence: Presence,
      expires: Expires = Publications.DefaultExpires
  ): Resource[F, Publication[F]] =
    publishPresenceAs(accounts.head, presence, expires)
}

object Publications {

  /** An hour, which is what RFC 3903 suggests. */
  val DefaultExpires: Expires = Expires.of(1.hour)
}
