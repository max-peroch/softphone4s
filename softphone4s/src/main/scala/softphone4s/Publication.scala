package softphone4s

import fs2.concurrent.Signal
import softphone4s.publication.{
  PublicationBody,
  PublicationEnding,
  PublicationStatus
}

/** State published (RFC 3903) and kept alive for as long as the resource that
  * holds it: refreshed before it lapses, published again when the notifier
  * loses it, and begun again after a failure that allows it. Releasing it
  * removes the state.
  */
trait Publication[F[_]] {

  /** Publishes `document` in place of the state now held. It is sent at once if
    * nothing is in flight, else once the request in flight is answered; of
    * several given meanwhile, the last is sent.
    */
  def update(document: PublicationBody): F[Unit]

  /** Where the publication stands, and every change to it. */
  def status: Signal[F, PublicationStatus]

  /** Completes once the notifier holds the state, with how the publication
    * ended instead if it ended for good first.
    */
  def awaitPublished: F[Either[PublicationEnding, Unit]]
}
