package softphone4s.publication

import softphone4s.fsm.TimerAction
import softphone4s.model.*

enum PublicationAction {

  /** Send a PUBLISH for `expires`: with `tag` and no `body` it refreshes, with
    * both it changes, with `tag` and `Expires: 0` it removes, with a `body` and
    * no tag it publishes anew. Answers `authorization` if present.
    */
  case SendPublish(
      context: PublicationContext,
      expires: Expires,
      tag: Option[EntityTag],
      body: Option[PublicationBody],
      authorization: Option[(AuthKind, DigestChallenge)]
  )

  /** Tell the program where the publication stands. */
  case Report(status: PublicationStatus)

  case Timer(action: TimerAction[PublicationTimer])
}
