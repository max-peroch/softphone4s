package softphone4s.fsm

/** The timers of a call, named so that [[TimerAction]] and `TimerFired` events
  * can refer to them. Retransmission and the transaction timeouts (Timers A to
  * K) belong to the transaction layer.
  */
enum CallTimer {

  /** Max wait for an answer to the INVITE, ringing included. */
  case InviteTimeout
}
