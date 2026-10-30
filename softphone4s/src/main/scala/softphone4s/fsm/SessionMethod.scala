package softphone4s.fsm

import softphone4s.model.SipMethod

/** The request that carries a session change. */
enum SessionMethod {

  /** A re-INVITE (RFC 3261 §14): answered in the 2xx and ACKed. */
  case Invite

  /** An UPDATE (RFC 3311): a plain request and response, with no ACK. */
  case Update

  def sipMethod: SipMethod = this match {
    case Invite => SipMethod.Invite
    case Update => SipMethod.Update
  }
}
