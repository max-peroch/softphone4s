package softphone4s.sdp

/** An audio codec as SDP names it. `staticPayloadType` is the number RFC 3551
  * gives it, if it has one; any other codec gets a dynamic number (96 to 127)
  * when it is offered.
  */
final case class Codec(
    encoding: String,
    clockRate: Int,
    channels: Int,
    staticPayloadType: Option[Int],
    fmtp: Option[String]
)

object Codec {
  val Pcmu: Codec = Codec("PCMU", 8000, 1, Some(0), None)
  val Pcma: Codec = Codec("PCMA", 8000, 1, Some(8), None)
}
