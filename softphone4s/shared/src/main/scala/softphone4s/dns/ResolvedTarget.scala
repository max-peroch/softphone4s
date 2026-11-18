package softphone4s.dns

import com.comcast.ip4s.{IpAddress, SocketAddress}
import softphone4s.model.TransportProtocol

/** One place a request can be sent, and the transport to send it with. */
final case class ResolvedTarget(
    transport: TransportProtocol,
    address: SocketAddress[IpAddress]
)
