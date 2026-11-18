package softphone4s.dialog

/** A partial notification that cannot be applied, because one before it was
  * missed or there was no full one to amend: the state is not known until a
  * full one comes, which a new subscription gets.
  *
  * @param have
  *   the version of the state held, if there is one.
  * @param got
  *   the version of the notification.
  */
final case class MissedUpdate(have: Option[Long], got: Long)
