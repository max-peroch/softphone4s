package softphone4s.config

import softphone4s.model.PositiveDuration
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
    retransmit: PositiveDuration = PositiveDuration.constant(500.millis),
    attempts: Attempts = Attempts.Default,
    refresh: PositiveDuration = PositiveDuration.constant(5.minutes)
) derives ConfigReader

object StunConfig {
  import NetworkReaders.given
}
