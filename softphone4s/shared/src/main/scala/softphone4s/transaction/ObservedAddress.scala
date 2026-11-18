package softphone4s.transaction

import com.comcast.ip4s.{IpAddress, SocketAddress}
import softphone4s.model.TransportProtocol

/** What a server says we look like from where it is: the address it saw a
  * request of ours come from, over `transport` (RFC 3581). Behind a NAT this is
  * not the address we bound.
  */
final case class ObservedAddress(
    transport: TransportProtocol,
    address: SocketAddress[IpAddress]
)
