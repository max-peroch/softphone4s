package softphone4s.registration

enum RegistrationTimer {

  /** Time to refresh the binding. */
  case Refresh

  /** Time to try again after a failure. */
  case Retry
}
