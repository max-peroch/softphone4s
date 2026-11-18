package softphone4s.registration

import softphone4s.fsm.TimerAction
import softphone4s.model.*

enum RegistrationAction {

  /** Send a REGISTER for `lifetime`, answering `authorization` if present. */
  case SendRegister(
      context: RegistrationContext,
      lifetime: Expires,
      authorization: Option[(AuthKind, DigestChallenge)]
  )

  case Notify(state: RegistrationState)
  case Timer(action: TimerAction[RegistrationTimer])
}
