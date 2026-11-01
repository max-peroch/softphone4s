package softphone4s.subscription

import softphone4s.model.MediaType

/** An event package (RFC 6665 §4): the `Event` a subscription asks for, the
  * kinds of body it takes, and how a NOTIFY's body is read as the state `A`.
  * Packages are plain values, so a program can define its own.
  */
trait EventPackage[A] {

  /** The `Event` header of a subscription to this package. */
  def event: EventHeader

  /** The body types to accept, for the `Accept` header. */
  def accept: List[MediaType]

  /** The state a NOTIFY carries. */
  def decode(notification: Notification): Either[PackageError, A]
}

object EventPackage {

  /** The body of `notification`, if it has one of the `expected` type. */
  def bodyOf(
      notification: Notification,
      expected: MediaType
  ): Either[PackageError, String] =
    if notification.body.isEmpty then Left(PackageError.NoBody)
    else
      notification.contentType match {
        case Some(t)
            if t.mainType == expected.mainType && t.subType == expected.subType =>
          Right(notification.body)
        case other => Left(PackageError.UnexpectedType(other))
      }
}
