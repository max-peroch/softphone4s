package softphone4s

import cats.data.NonEmptyList
import cats.effect.Resource
import fs2.{Pipe, Stream}
import softphone4s.fsm.SessionChange
import softphone4s.model.{AccountId, Callee, Dtmf, DtmfDigit, DtmfMethod}
import softphone4s.registration.Registration

import scala.concurrent.duration.FiniteDuration

trait Call[F[_]] {

  /** PCM audio received from the remote party. */
  def source: Stream[F, Byte]

  /** Pipe PCM audio into this to send it to the remote party. */
  def sink: Pipe[F, Byte, Unit]

  /** Stream of DTMF digits; terminates when the call ends. */
  def dtmfEvents: Stream[F, DtmfDigit]

  /** Sends `digit` to the remote party, taking `duration` (from 20 ms to 8 s):
    * in the media stream as telephone events (RFC 4733) if the other side
    * agreed to receive them and the call is not on hold, else in a SIP INFO.
    * Says which it was. Fails with `CallNotEstablished` before the call is
    * answered, and with `InvalidDtmfDuration` for a duration a digit cannot
    * have. A digit by INFO is sent only when the call is not changing its
    * session; one that comes then is not sent.
    */
  def sendDtmf(
      digit: DtmfDigit,
      duration: FiniteDuration = Dtmf.DefaultDuration
  ): F[DtmfMethod]

  /** Suspends until the remote party answers. Fails if the call is rejected or
    * times out.
    */
  def awaitPickup: F[Unit]

  /** Suspends until the remote party ends the call. */
  def awaitHangup: F[Unit]

  /** Asks the remote party to change the session: a re-INVITE or UPDATE
    * offering the given media direction. Returns once the request is queued;
    * the call carries on if it is refused, and a clash with the remote party's
    * own request (491) is retried after a random delay.
    */
  def changeSession(change: SessionChange): F[Unit]

  /** Puts the call on hold (a re-INVITE offering `sendonly`). */
  def hold: F[Unit] = changeSession(SessionChange.Hold)

  /** Takes the call off hold (a re-INVITE offering `sendrecv`). */
  def resume: F[Unit] = changeSession(SessionChange.Resume)
}

trait Softphone[F[_]] {

  /** The accounts this softphone holds, the default first. */
  def accounts: NonEmptyList[AccountId]

  /** Dials `callee` as `account`. Acquiring the resource sends INVITE and
    * returns immediately with a handle — use `Call.awaitPickup` to wait for the
    * remote to answer. Releasing it sends BYE and drains the event loop. Fails
    * with `UnknownAccount` if `account` is not one of [[accounts]].
    */
  def callAs(
      account: AccountId,
      callee: Callee,
      extraHeaders: Map[String, List[String]] = Map.empty
  ): Resource[F, Call[F]]

  /** Dials `callee` as the default account. */
  def call(
      callee: Callee,
      extraHeaders: Map[String, List[String]] = Map.empty
  ): Resource[F, Call[F]] =
    callAs(accounts.head, callee, extraHeaders)

  /** Registers `account` at its registrar and keeps it registered for as long
    * as the resource is held, refreshing the binding and retrying after
    * failures. Releasing it removes the binding. `Registration.state` reports
    * how it is going; it does not fail the resource if the registrar is
    * unreachable. Fails with `UnknownAccount` if `account` is not one of
    * [[accounts]].
    */
  def registerAs(account: AccountId): Resource[F, Registration[F]]

  /** Registers the default account. */
  def register: Resource[F, Registration[F]] = registerAs(accounts.head)

  /** Calls placed to any of the accounts, as they arrive;
    * `IncomingCall.account` says which. Each is answered with
    * `IncomingCall.accept` or turned away with `reject`; one that is neither is
    * rejected with `480 Temporarily Unavailable` after `answer-timeout`. With
    * several accounts, a call to a user none of them has is refused
    * `404 Not Found`.
    */
  def incoming: Stream[F, IncomingCall[F]]
}
