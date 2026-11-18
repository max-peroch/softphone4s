package softphone4s.dialog

/** What a dialog-info document covers (RFC 4235 §4.1.1): all the dialogs of the
  * entity, or only those that changed.
  */
enum Coverage {

  /** Every dialog there is: it replaces what was known. */
  case Full

  /** The dialogs that changed since the last: it amends what was known.
    */
  case Partial
}

object Coverage {

  def parse(text: String): Option[Coverage] = text.trim.toLowerCase match {
    case "full"    => Some(Full)
    case "partial" => Some(Partial)
    case _         => None
  }
}
