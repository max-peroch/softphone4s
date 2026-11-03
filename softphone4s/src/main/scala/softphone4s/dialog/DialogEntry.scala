package softphone4s.dialog

import scala.concurrent.duration.FiniteDuration

/** One dialog of the entity (RFC 4235 §4.1.2): its state, how it came to be
  * there, how long it has lasted, and who is at each end. `id` is the
  * notifier's name for it, which stays the same however the identifiers of the
  * dialog change.
  */
final case class DialogEntry(
    id: String,
    callId: Option[String],
    localTag: Option[String],
    remoteTag: Option[String],
    direction: Option[DialogDirection],
    state: DialogState,
    event: Option[EndEvent],
    code: Option[Int],
    duration: Option[FiniteDuration],
    replaces: Option[Replaces],
    referredBy: Option[String],
    local: Option[Party],
    remote: Option[Party]
) {

  /** Whether the dialog has ended. */
  def isTerminated: Boolean = state == DialogState.Terminated
}
