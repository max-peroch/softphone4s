package softphone4s

import cats.data.NonEmptyList
import cats.effect.Resource
import softphone4s.model.{AccountId, Callee, Expires, MediaType}
import softphone4s.subscription.{EventHeader, EventPackage}

import scala.concurrent.duration.*

/** Subscriptions to the state of a resource elsewhere (RFC 6665): presence,
  * message waiting, and the like. Reached as `Softphone.subscriptions`.
  */
trait Subscriptions[F[_]] {

  /** The accounts subscriptions can be made as, the default first. */
  def accounts: NonEmptyList[AccountId]

  /** Subscribes to `event` of `target` as `account` for as long as the resource
    * is held: acquiring it sends the SUBSCRIBE and returns at once, with a
    * [[Subscription]] whose `awaitActive` says when the notifier has agreed.
    * The subscription is refreshed before it lapses, and begun again in a new
    * dialog when it ends in a way that allows it; releasing the resource ends
    * it.
    *
    * @param accept
    *   the types of body to take, which the notifier chooses among.
    * @param expires
    *   the lifetime to ask for; the notifier may grant less.
    *
    * Fails with `UnknownAccount` if `account` is not one of [[accounts]], and
    * with `InvalidSipUri` if `target` names no address.
    */
  def subscribeAs(
      account: AccountId,
      target: Callee,
      event: EventHeader,
      accept: List[MediaType] = Nil,
      expires: Expires = Subscriptions.DefaultExpires
  ): Resource[F, Subscription[F]]

  /** Subscribes to an event package as `account`: the `Event` and `Accept` are
    * the package's, and the states it describes come from [[Watch.states]]. A
    * NOTIFY that cannot be read as the package says (the one that ends a
    * subscription often has no body) is a `Left` there and does not end the
    * stream.
    */
  def watchAs[A](
      account: AccountId,
      target: Callee,
      pkg: EventPackage[A],
      expires: Expires = Subscriptions.DefaultExpires
  ): Resource[F, Watch[F, A]] =
    subscribeAs(account, target, pkg.event, pkg.accept, expires).map(s =>
      Watch(s, s.notifications.map(pkg.decode))
    )

  /** Subscribes to an event package as the default account. */
  def watch[A](
      target: Callee,
      pkg: EventPackage[A],
      expires: Expires = Subscriptions.DefaultExpires
  ): Resource[F, Watch[F, A]] =
    watchAs(accounts.head, target, pkg, expires)

  /** Subscribes as the default account. */
  def subscribe(
      target: Callee,
      event: EventHeader,
      accept: List[MediaType] = Nil,
      expires: Expires = Subscriptions.DefaultExpires
  ): Resource[F, Subscription[F]] =
    subscribeAs(accounts.head, target, event, accept, expires)
}

object Subscriptions {

  /** An hour, which is what most notifiers grant. */
  val DefaultExpires: Expires = Expires.of(1.hour)
}
