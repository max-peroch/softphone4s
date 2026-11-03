package softphone4s.dns

import softphone4s.model.PositiveDuration
import com.comcast.ip4s.{IpAddress, SocketAddress}
import pureconfig.ConfigReader
import softphone4s.config.NetworkReaders.given

import scala.concurrent.duration.*

/** Which DNS servers to ask for SRV and NAPTR records.
  *
  * @param servers
  *   the servers to ask, in order; the system's own when empty. A server is
  *   written `address` or `address:port`, the port being 53 if left out.
  * @param timeout
  *   how long to wait for one server's answer.
  */
final case class DnsConfig(
    servers: List[SocketAddress[IpAddress]] = Nil,
    timeout: PositiveDuration = PositiveDuration.constant(5.seconds)
) derives ConfigReader
