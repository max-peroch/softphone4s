package softphone4s.dialog

/** How far a dialog has got (RFC 4235 §4.1.2). */
enum DialogState {

  /** An INVITE has been sent or received and nothing has answered it. */
  case Trying

  /** A provisional response has been sent or received, with no To tag. */
  case Proceeding

  /** A provisional response with a To tag: an early dialog. */
  case Early

  /** The dialog is set up: a 2xx and its ACK. */
  case Confirmed

  /** The dialog is over. */
  case Terminated
}

object DialogState {

  def parse(text: String): Option[DialogState] = text.trim.toLowerCase match {
    case "trying"     => Some(Trying)
    case "proceeding" => Some(Proceeding)
    case "early"      => Some(Early)
    case "confirmed"  => Some(Confirmed)
    case "terminated" => Some(Terminated)
    case _            => None
  }
}
