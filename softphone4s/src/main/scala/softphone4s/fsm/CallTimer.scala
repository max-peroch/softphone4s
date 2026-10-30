package softphone4s.fsm

/** The timers of a call, named so that [[TimerAction]] and `TimerFired` events
  * can refer to them.
  */
enum CallTimer {

  /** Max wait for an answer (RFC 3261 Timer B). */
  case InviteTimeout

  /** Max wait for the response to our BYE (Timer F). */
  case ByeTimeout
}
