package softphone4s.config

import com.comcast.ip4s.*
import pureconfig.*
import pureconfig.generic.semiauto.*

import scala.concurrent.duration.*

/** SIP account and network settings for a `SoftphoneStreaming`. Loadable via
  * pureconfig — see the `given ConfigReader[SoftphoneConfig]` below.
  */
case class SoftphoneConfig(
    user: String,
    password: String,
    realm: String,
    localSipPort: Port = port"5060",
    serverPort: Port = port"5060",
    baseRtpPort: Port = port"8000",
    bindAddress: IpAddress = ip"0.0.0.0",
    localIpFallback: IpAddress = ip"127.0.0.1",
    hangupTimeout: FiniteDuration = 5.seconds,
    inviteTimeout: FiniteDuration = 32.seconds
)

object SoftphoneConfig {
  given ConfigReader[Port] = ConfigReader[Int].emap(i =>
    Port
      .fromInt(i)
      .toRight(error.CannotConvert(i.toString, "Port", "not in range 0-65535"))
  )
  given ConfigReader[IpAddress] = ConfigReader[String].emap(s =>
    IpAddress
      .fromString(s)
      .toRight(error.CannotConvert(s, "IpAddress", "invalid IP address"))
  )

  given ConfigReader[SoftphoneConfig] = deriveReader
}
