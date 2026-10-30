package softphone4s.transport

import com.comcast.ip4s.{IpAddress, SocketAddress}
import softphone4s.model.SipMessage

/** A message and the address it came from (needed for `rport`/`received`). */
final case class Inbound(from: SocketAddress[IpAddress], message: SipMessage)
