package softphone4s.model

/** Which way media flows, as one party's SDP states it (RFC 3264 §5.1). */
enum MediaDirection {
  case SendRecv, SendOnly, RecvOnly, Inactive

  /** Whether the party that stated this direction is willing to receive. */
  def receives: Boolean = this match {
    case SendRecv | RecvOnly => true
    case SendOnly | Inactive => false
  }

  /** Whether the party that stated this direction is willing to send. */
  def sends: Boolean = this match {
    case SendRecv | SendOnly => true
    case RecvOnly | Inactive => false
  }

  /** The direction to answer an offer of this direction with (RFC 3264 §6.1):
    * what they send we receive, and the other way round.
    */
  def answer: MediaDirection = this match {
    case SendRecv => SendRecv
    case SendOnly => RecvOnly
    case RecvOnly => SendOnly
    case Inactive => Inactive
  }

  def sdpAttribute: String = this match {
    case SendRecv => "sendrecv"
    case SendOnly => "sendonly"
    case RecvOnly => "recvonly"
    case Inactive => "inactive"
  }
}

object MediaDirection {
  def fromAttribute(s: String): Option[MediaDirection] = s match {
    case "sendrecv" => Some(SendRecv)
    case "sendonly" => Some(SendOnly)
    case "recvonly" => Some(RecvOnly)
    case "inactive" => Some(Inactive)
    case _          => None
  }
}
