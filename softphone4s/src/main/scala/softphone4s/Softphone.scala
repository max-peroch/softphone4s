package softphone4s

import cats.effect.Resource
import fs2.{Pipe, Stream}
import softphone4s.model.{Callee, DtmfDigit}
import softphone4s.registration.Registration

trait Call[F[_]] {

  /** PCM audio received from the remote party. */
  def source: Stream[F, Byte]

  /** Pipe PCM audio into this to send it to the remote party. */
  def sink: Pipe[F, Byte, Unit]

  /** Stream of DTMF digits; terminates when the call ends. */
  def dtmfEvents: Stream[F, DtmfDigit]

  /** Suspends until the remote party answers. Fails if the call is rejected or
    * times out.
    */
  def awaitPickup: F[Unit]

  /** Suspends until the remote party ends the call. */
  def awaitHangup: F[Unit]
}

trait Softphone[F[_]] {

  /** Dials `callee`. Acquiring the resource sends INVITE and returns
    * immediately with a handle — use `Call.awaitPickup` to wait for the remote
    * to answer. Releasing it sends BYE and drains the event loop.
    */
  def call(
      callee: Callee,
      extraHeaders: Map[String, List[String]] = Map.empty
  ): Resource[F, Call[F]]

  /** Registers the configured account at its registrar and keeps it registered
    * for as long as the resource is held, refreshing the binding and retrying
    * after failures. Releasing it removes the binding. `Registration.state`
    * reports how it is going; it does not fail the resource if the registrar is
    * unreachable.
    */
  def register: Resource[F, Registration[F]]

  /** Calls placed to this softphone, as they arrive. Each is answered with
    * `IncomingCall.accept` or turned away with `reject`; one that is neither is
    * rejected with `480 Temporarily Unavailable` after `answer-timeout`.
    */
  def incoming: Stream[F, IncomingCall[F]]
}
