package softphone4s.dialog

/** What caused a dialog to move to its state: the `event` of its `state`
  * element (RFC 4235 §4.1.2.1). A value that is not one of these is left out.
  */
enum EndEvent {
  case Cancelled
  case Rejected
  case Replaced
  case LocalBye
  case RemoteBye
  case Error
  case Timeout
}

object EndEvent {

  def parse(text: String): Option[EndEvent] = text.trim.toLowerCase match {
    case "cancelled"  => Some(Cancelled)
    case "rejected"   => Some(Rejected)
    case "replaced"   => Some(Replaced)
    case "local-bye"  => Some(LocalBye)
    case "remote-bye" => Some(RemoteBye)
    case "error"      => Some(Error)
    case "timeout"    => Some(Timeout)
    case _            => None
  }
}
