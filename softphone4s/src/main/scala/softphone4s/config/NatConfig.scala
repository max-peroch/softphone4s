package softphone4s.config

import pureconfig.ConfigReader

/** How to get along behind a NAT. */
final case class NatConfig(
    observedAddress: ObservedAddresses = ObservedAddresses.Use
) derives ConfigReader
