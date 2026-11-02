package softphone4s.config

import com.comcast.ip4s.{Host, Port, port}
import pureconfig.ConfigReader

import scala.concurrent.duration.*

/** The STUN server (RFC 8489) that tells us the address we have outside a NAT,
  * and how often to ask again.
  *
  * @param host
  *   the server's name or address.
  * @param retransmit
  *   how long to wait for an answer before asking again; doubled each time.
  * @param attempts
  *   how many times to ask before giving up on a round.
  * @param refresh
  *   how long between rounds, since a NAT can change its mind.
  */
final case class StunConfig(
    host: Host,
    port: Port = port"3478",
    retransmit: FiniteDuration = 500.millis,
    attempts: Attempts = Attempts.Default,
    refresh: FiniteDuration = 5.minutes
) derives ConfigReader

object StunConfig {
  import NetworkReaders.given
}
