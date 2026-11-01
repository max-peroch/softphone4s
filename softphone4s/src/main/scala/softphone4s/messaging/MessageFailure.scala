package softphone4s.messaging

import softphone4s.model.MediaType

/** Why an instant message was not delivered: it never arrived, or the other
  * side (or a server on the way) refused it. A 2xx is delivery.
  */
enum MessageFailure {

  /** Refused with `status`, such as 403 or 404, after answering a challenge if
    * there was one.
    */
  case Rejected(status: Int, reason: String)

  /** Refused with 415: the other side takes other types, which it lists if it
    * says so.
    */
  case UnsupportedContent(accepted: List[MediaType])

  /** Challenged for credentials that we gave, or that we could not answer. */
  case Unauthorized

  /** No final answer in time. */
  case TimedOut

  /** It could not be sent, or the connection failed. */
  case TransportFailed

  def message: String = this match {
    case Rejected(status, reason)     => s"message refused: $status $reason"
    case UnsupportedContent(accepted) =>
      s"content type not accepted (accepted: ${accepted.map(_.render).mkString(", ")})"
    case Unauthorized    => "message not authorised"
    case TimedOut        => "no answer to the message"
    case TransportFailed => "message could not be sent"
  }
}
