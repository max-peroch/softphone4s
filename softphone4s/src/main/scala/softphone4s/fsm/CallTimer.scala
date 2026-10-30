package softphone4s.fsm

/** The timers of a call, named so that [[TimerAction]] and `TimerFired` events
  * can refer to them. Retransmission and the transaction timeouts (Timers A to
  * K) belong to the transaction layer.
  */
enum CallTimer {

  /** Max wait for an answer to the INVITE, ringing included. */
  case InviteTimeout

  /** How long an incoming call may go unanswered before we turn it away. */
  case AnswerTimeout

  /** Time to send the 200 OK again, until the ACK arrives (RFC 3261 §13.3.1.4).
    */
  case RetransmitOk

  /** No ACK for our 200 OK: the dialog is considered over. */
  case AckTimeout
}
