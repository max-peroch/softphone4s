package softphone4s.model

import com.comcast.ip4s.{IpAddress, SocketAddress}

/** One way to reach a server: where to send, and as what (the transport, local
  * address and Contact the messages carry).
  */
final case class Uplink(
    endpoint: LocalEndpoint,
    peer: SocketAddress[IpAddress]
)
