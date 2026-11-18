package softphone4s.transport

import fs2.Chunk

/** What a stream connection carries (RFC 3261 §18.3, RFC 5626 §4.4.1). */
enum Frame {

  /** A SIP message, bytes as received. */
  case Message(bytes: Chunk[Byte])

  /** A keep-alive ping, the double CRLF that asks to be answered with a single
    * CRLF, the pong.
    */
  case Ping
}
