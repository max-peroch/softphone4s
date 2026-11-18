package softphone4s.registration

/** Whether a REGISTER in flight sets up the binding or refreshes it. */
enum RegistrationPhase {
  case Establishing, Refreshing
}
