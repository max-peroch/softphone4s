package softphone4s.registration

import softphone4s.model.*

enum RegistrationFsmState {
  case Idle

  /** A REGISTER is in flight; `attempt` counts failures since the last success,
    * for the backoff.
    */
  case Registering(
      context: RegistrationContext,
      attempt: Int,
      phase: RegistrationPhase
  )

  case Registered(context: RegistrationContext, lifetime: Expires)

  /** The last attempt failed; the retry timer is running. */
  case Backoff(context: RegistrationContext, attempt: Int)

  /** A REGISTER with `Expires: 0` is in flight. */
  case Unregistering(context: RegistrationContext)

  case Stopped
}
