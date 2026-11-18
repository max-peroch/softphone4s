package softphone4s

import cats.effect.Resource
import softphone4s.model.{AccountId, FailureStatus, NameAddr}

/** A call someone placed to us, not yet answered. The application decides what
  * to do with it: `ringing`, then `accept` or `reject`. One that nobody answers
  * is turned away with `480 Temporarily Unavailable` after `answer-timeout`.
  */
trait IncomingCall[F[_]] {

  /** The account that was called. */
  def account: AccountId

  /** Who is calling: the From header. */
  def caller: NameAddr

  /** Who they called: the To header. */
  def callee: NameAddr

  /** Tells the caller we are ringing (`180 Ringing`). */
  def ringing: F[Unit]

  /** Answers the call. Acquiring the resource sends `200 OK`; the returned
    * `Call` is established once the caller's ACK arrives (`awaitPickup`).
    * Releasing it hangs up with a BYE. If the call was cancelled meanwhile,
    * `awaitPickup` fails.
    */
  def accept: Resource[F, Call[F]]

  /** Turns the call away with a final failure status. */
  def reject(status: FailureStatus = FailureStatus.Decline): F[Unit]

  /** Suspends until the caller cancels before the call is answered. */
  def cancelled: F[Unit]
}
