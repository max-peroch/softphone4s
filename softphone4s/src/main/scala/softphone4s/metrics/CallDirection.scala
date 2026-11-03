package softphone4s.metrics

/** Which way a call was made. */
enum CallDirection {

  /** We dialled. */
  case Outgoing

  /** It was made to us. */
  case Incoming

  /** As logs and metrics spell it: `outgoing` or `incoming`. */
  def token: String = this match {
    case Outgoing => "outgoing"
    case Incoming => "incoming"
  }
}
