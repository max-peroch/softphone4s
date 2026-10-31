package softphone4s.transport

import pureconfig.ConfigReader

import scala.concurrent.duration.*

/** Keeping a TCP or TLS connection alive with CRLF pings (RFC 5626 §4.4.1), so
  * that a NAT or a firewall does not forget it and a dead peer is noticed.
  *
  * @param interval
  *   how long a connection may be quiet before it is pinged. RFC 5626 suggests
  *   95 to 120 seconds for TCP.
  * @param timeout
  *   how long to wait for anything at all to come back after a ping before the
  *   connection is given up on.
  */
final case class KeepAlive(
    interval: FiniteDuration = 2.minutes,
    timeout: FiniteDuration = 10.seconds
) derives ConfigReader
