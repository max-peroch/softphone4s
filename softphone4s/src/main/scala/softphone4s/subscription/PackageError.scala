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

  def message: String = this match {
    case NoBody                  => "the NOTIFY has no body"
    case UnexpectedType(None)    => "the body has no content type"
    case UnexpectedType(Some(t)) => s"unexpected content type: $t"
    case Malformed(reason)       => s"malformed body: $reason"
  }
}
