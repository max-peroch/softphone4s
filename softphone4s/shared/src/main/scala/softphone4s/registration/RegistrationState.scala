package softphone4s.registration

import scala.concurrent.duration.FiniteDuration

/** Why a registration attempt failed. */
enum RegistrationFailure {

  /** The registrar answered with a final error. */
  case Rejected(code: Int, reason: String)

  /** The credentials were refused, or the challenge could not be answered. */
  case AuthenticationFailed

  /** The registrar did not answer in time. */
  case TimedOut

  /** The REGISTER could not be sent. */
  case TransportFailed
}

/** Where a registration stands, as the application sees it. */
enum RegistrationState {
  case Unregistered
  case Registering

  /** Bound at the registrar for `lifetime`; it is refreshed before then. */
  case Registered(lifetime: FiniteDuration)

  /** The last attempt failed and another starts in `retryIn`. */
  case Failed(failure: RegistrationFailure, retryIn: FiniteDuration)

  /** The binding is being removed. */
  case Unregistering
}
