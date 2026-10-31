package softphone4s.sdp

import com.comcast.ip4s.{Host, ip}

/** The `o=` line: who made the description, and which version of it this is.
  * The version has to go up whenever the description changes (RFC 3264 §8).
  */
final case class Origin(
    username: String,
    sessionId: BigInt,
    sessionVersion: BigInt,
    address: Host
)

object Origin {

  /** What a description without an `o=` line is taken to come from. Peers
    * always send one, but nothing here depends on theirs.
    */
  val Unknown: Origin = Origin("-", 0, 0, ip"0.0.0.0")
}
