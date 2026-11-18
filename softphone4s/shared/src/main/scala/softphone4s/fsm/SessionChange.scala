package softphone4s.fsm

import softphone4s.model.MediaDirection

/** A change to the session that we ask for: the direction we offer and the
  * request that carries it.
  */
final case class SessionChange(
    direction: MediaDirection,
    method: SessionMethod = SessionMethod.Invite
)

object SessionChange {

  /** Put the call on hold: we offer `sendonly`. */
  val Hold: SessionChange = SessionChange(MediaDirection.SendOnly)

  /** Take the call off hold: we offer `sendrecv`. */
  val Resume: SessionChange = SessionChange(MediaDirection.SendRecv)

  /** [[Hold]] in an UPDATE, for a peer that wants no second INVITE. */
  val HoldByUpdate: SessionChange = Hold.copy(method = SessionMethod.Update)

  /** [[Resume]] in an UPDATE. */
  val ResumeByUpdate: SessionChange = Resume.copy(method = SessionMethod.Update)
}
