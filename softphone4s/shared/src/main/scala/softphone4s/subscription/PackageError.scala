package softphone4s.subscription

import softphone4s.model.MediaType

/** Why a NOTIFY cannot be read as the state an event package describes. */
enum PackageError {

  /** The NOTIFY carries no body, as the one that ends a subscription often
    * does.
    */
  case NoBody

  /** A body of a type the package does not define. */
  case UnexpectedType(got: Option[MediaType])

  /** A body of the right type that does not follow its format. */
  case Malformed(reason: String)

  /** A body longer than the most an event package reads. Presence and message
    * summaries are a few kilobytes, and what is read is read in time that grows
    * with its length.
    */
  case TooLarge(length: Int, limit: Int)

  def message: String = this match {
    case NoBody                  => "the NOTIFY has no body"
    case UnexpectedType(None)    => "the body has no content type"
    case UnexpectedType(Some(t)) => s"unexpected content type: $t"
    case Malformed(reason)       => s"malformed body: $reason"
    case TooLarge(length, limit) =>
      s"a body of $length characters; the most read is $limit"
  }
}
