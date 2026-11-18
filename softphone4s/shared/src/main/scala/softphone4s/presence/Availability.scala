package softphone4s.presence

/** The `basic` status of a tuple (RFC 3863 §4.1.4): whether the service can be
  * reached.
  */
enum Availability {
  case Open
  case Closed
}

object Availability {

  def parse(text: String): Option[Availability] = text.trim.toLowerCase match {
    case "open"   => Some(Open)
    case "closed" => Some(Closed)
    case _        => None
  }
}
