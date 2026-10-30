package softphone4s.config

import com.comcast.ip4s.*
import pureconfig.*
import pureconfig.generic.semiauto.*
import softphone4s.model.TransportProtocol

import scala.concurrent.duration.*

/** One SIP account: who we are at which server. */
final case class AccountConfig(
    user: String,
    password: String,
    realm: String,
    serverPort: Port = port"5060",
    registerExpires: FiniteDuration = 1.hour,
    transport: TransportProtocol = TransportProtocol.Udp
)

object AccountConfig {
  import NetworkReaders.given

  given ConfigReader[AccountConfig] = deriveReader
}
