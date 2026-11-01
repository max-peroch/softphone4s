package softphone4s.subscription

import softphone4s.model.MediaType

/** What a NOTIFY brought: where the subscription stands, and the state of the
  * resource it carries, if it carries any. The program decodes `body` as the
  * event package says.
  */
final case class Notification(
    state: NotifyState,
    contentType: Option[MediaType],
    body: String
)
