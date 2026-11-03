package softphone4s.config

import com.comcast.ip4s.*
import pureconfig.*
import pureconfig.generic.semiauto.*
import softphone4s.model.{PositiveDuration, Secret, TransportProtocol, User}

import scala.concurrent.duration.*

/** One SIP account: who we are at which server. */
final case class AccountConfig(
    user: User,
    password: Secret,
    realm: Host,
    serverPort: Port = port"5060",
    registerExpires: PositiveDuration = PositiveDuration.constant(1.hour),
    transport: TransportProtocol = TransportProtocol.Udp
)

object AccountConfig {
  import NetworkReaders.given

  given ConfigReader[AccountConfig] = deriveReader
}
