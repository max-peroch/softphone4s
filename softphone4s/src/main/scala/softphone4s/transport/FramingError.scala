package softphone4s.transport

/** Why a byte stream cannot be cut into SIP messages. The connection is closed:
  * after one of these there is no telling where the next message starts.
  */
enum FramingError {

  /** No blank line within the header limit. */
  case HeadersTooLarge(limit: Int)

  /** `Content-Length` is not a number. */
  case BadContentLength(text: String)

  /** A message with no `Content-Length`, which a stream needs (RFC 3261 §18.3).
    */
  case MissingContentLength

  /** The body is longer than we accept. */
  case BodyTooLarge(length: Long, limit: Int)

  def message: String = this match {
    case HeadersTooLarge(limit)    => s"headers longer than $limit bytes"
    case BadContentLength(text)    => s"bad Content-Length '$text'"
    case MissingContentLength      => "no Content-Length"
    case BodyTooLarge(length, max) => s"body of $length bytes (limit $max)"
  }
}
