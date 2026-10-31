package softphone4s.config

import pureconfig.ConfigReader

/** How to get along behind a NAT: whether to believe the address servers see us
  * at, a STUN server to ask for it, and whether media follows the address the
  * other side's media comes from.
  */
final case class NatConfig(
    observedAddress: ObservedAddresses = ObservedAddresses.Use,
    stun: Option[StunConfig] = None,
    rtpLatching: RtpLatching = RtpLatching.Follow
) derives ConfigReader
