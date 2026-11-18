package softphone4s.transport

/** What is known of one packet of a telephone event (RFC 4733) when it is made.
  */
private[transport] object DigitPacket {

  /** Whether the event is still going on, or this is one of the packets that
    * end it.
    */
  enum Phase {
    case Running
    case Ending
  }

  /** Whether this is the first packet of the event, which carries the marker.
    */
  enum Place {
    case First
    case Later
  }
}
