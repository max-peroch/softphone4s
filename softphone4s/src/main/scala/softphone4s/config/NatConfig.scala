package softphone4s.config

import pureconfig.ConfigReader

/** How to get along behind a NAT: whether to believe the address servers see us
  * at, and a STUN server to ask for it.
  */
final case class NatConfig(
    observedAddress: ObservedAddresses = ObservedAddresses.Use,
    stun: Option[StunConfig] = None
) derives ConfigReader
