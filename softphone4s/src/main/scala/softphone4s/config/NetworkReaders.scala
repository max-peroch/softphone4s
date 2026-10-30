package softphone4s.config

import com.comcast.ip4s.*
import pureconfig.*
import softphone4s.model.TransportProtocol

/** How ports and IP addresses are read from configuration. */
private[config] object NetworkReaders {
  given ConfigReader[Port] = ConfigReader[Int].emap(i =>
    Port
      .fromInt(i)
      .toRight(error.CannotConvert(i.toString, "Port", "not in range 0-65535"))
  )
  given ConfigReader[TransportProtocol] = ConfigReader[String].emap(s =>
    TransportProtocol
      .fromToken(s)
      .toRight(
        error.CannotConvert(s, "TransportProtocol", "not a known transport")
      )
  )
  given ConfigReader[IpAddress] = ConfigReader[String].emap(s =>
    IpAddress
      .fromString(s)
      .toRight(error.CannotConvert(s, "IpAddress", "invalid IP address"))
  )
}
