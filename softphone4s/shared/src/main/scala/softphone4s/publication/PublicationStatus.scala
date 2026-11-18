package softphone4s.publication

import scala.concurrent.duration.FiniteDuration

/** Where a publication stands, as told to the program. */
enum PublicationStatus {

  /** Asking for the state to be published. */
  case Publishing

  /** The notifier holds the state, for `expiresIn` unless refreshed. */
  case Published(expiresIn: FiniteDuration)

  /** We have asked for the state to be removed. */
  case Removing

  /** It ended in a way that allows another try, which begins after `in`. */
  case Retrying(because: PublicationEnding, in: FiniteDuration)

  /** Over: removed, or not to be had. */
  case Ended(ending: PublicationEnding)
}
