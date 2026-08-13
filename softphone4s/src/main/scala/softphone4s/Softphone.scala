package softphone4s

import cats.effect.Resource
import fs2.{Pipe, Stream}
import softphone4s.model.{Callee, DtmfDigit}

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
}
