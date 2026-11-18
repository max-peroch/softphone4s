package softphone4s

import fs2.Stream
import softphone4s.subscription.PackageError

/** A subscription to an event package: the state each NOTIFY carries, read as
  * the package says, and the subscription itself for its status.
  */
final case class Watch[F[_], A](
    subscription: Subscription[F],
    states: Stream[F, Either[PackageError, A]]
)
