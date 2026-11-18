package softphone4s.codec

/** The outcome of looking for one message at the start of a stream buffer. */
enum Framing {

  /** The buffer holds only part of a message. */
  case NeedMore

  /** `consumed` bytes of blank lines (RFC 5626 keep-alive pings and pongs). */
  case KeepAlive(consumed: Int)

  /** The first `length` bytes are one complete message. */
  case Frame(length: Int)

  /** The stream cannot be a SIP message; the connection should be closed. */
  case Invalid(error: ParseError)
}
