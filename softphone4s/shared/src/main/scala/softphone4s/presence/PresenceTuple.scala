package softphone4s.presence

/** One way to reach the presentity (RFC 3863 §4.1): its `id`, whether it is
  * open, where to reach it, and any notes.
  *
  * @param basic
  *   `None` when the tuple says nothing about it, as an extension may.
  */
final case class PresenceTuple(
    id: String,
    basic: Option[Availability],
    contact: Option[String],
    notes: List[String]
)
