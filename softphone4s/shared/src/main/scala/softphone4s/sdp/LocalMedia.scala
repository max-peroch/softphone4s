package softphone4s.sdp

import com.comcast.ip4s.{IpAddress, Port}

/** Where we receive media, and which version of our description this is. */
final case class LocalMedia(
    address: IpAddress,
    port: Port,
    sessionId: BigInt,
    sessionVersion: BigInt
) {

  def origin: Origin = Origin("-", sessionId, sessionVersion, address)
}
