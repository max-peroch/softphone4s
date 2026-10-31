package softphone4s.sdp

/** Why a body is not an SDP description we can use. */
enum SdpError {

  /** A line that is not what RFC 4566 allows there; `line` counts from 1. */
  case Malformed(line: Int, reason: String)

  /** A line every description has to carry. */
  case Missing(field: Char)

  /** A connection address that is a name, not an IP address: it would have to
    * be resolved before media could be sent.
    */
  case NameAddress(text: String)

  def message: String = this match {
    case Malformed(line, reason) => s"SDP line $line: $reason"
    case Missing(field)          => s"SDP has no $field= line"
    case NameAddress(text)       => s"SDP connection address is a name: $text"
  }
}
