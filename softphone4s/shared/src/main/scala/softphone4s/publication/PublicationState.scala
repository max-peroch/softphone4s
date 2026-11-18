package softphone4s.publication

import softphone4s.model.Expires

/** Where a publication is. The three that wait for an answer say what the
  * request in flight is: the first PUBLISH (no tag), a refresh, or a change.
  */
enum PublicationState {
  case Idle

  /** The first PUBLISH, or one made anew after the state lapsed. */
  case Publishing(context: PublicationContext)

  /** The notifier holds the state under `tag`, with `lifetime` to run. */
  case Published(context: PublicationContext, tag: EntityTag, lifetime: Expires)

  case Refreshing(
      context: PublicationContext,
      tag: EntityTag,
      lifetime: Expires
  )

  case Changing(context: PublicationContext, tag: EntityTag, lifetime: Expires)

  /** A PUBLISH with `Expires: 0` is in flight. */
  case Removing(context: PublicationContext, tag: EntityTag)

  case Ended(ending: PublicationEnding)
}
