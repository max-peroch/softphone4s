package softphone4s.dialog

/** Which end of the dialog the entity is (RFC 4235 §4.1.1). */
enum DialogDirection {

  /** It sent the INVITE. */
  case Initiator

  /** It received the INVITE. */
  case Recipient
}

object DialogDirection {

  def parse(text: String): Option[DialogDirection] =
    text.trim.toLowerCase match {
      case "initiator" => Some(Initiator)
      case "recipient" => Some(Recipient)
      case _           => None
    }
}
