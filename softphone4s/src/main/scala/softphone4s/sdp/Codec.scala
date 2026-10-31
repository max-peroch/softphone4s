package softphone4s.sdp

import softphone4s.model.PayloadType

/** An audio codec as SDP names it. `staticPayloadType` is the number RFC 3551
  * gives it, if it has one; any other codec gets a dynamic number (96 to 127)
  * when it is offered.
  */
final case class Codec(
    encoding: String,
    clockRate: Int,
    channels: Int,
    staticPayloadType: Option[PayloadType],
    fmtp: Option[String]
)

object Codec {
  val Pcmu: Codec = Codec("PCMU", 8000, 1, Some(PayloadType.Pcmu), None)
  val Pcma: Codec = Codec("PCMA", 8000, 1, Some(PayloadType.Pcma), None)

  /** Sampled at 16 kHz, but its RTP clock rate is 8 000, a mistake in the RFC
    * that stayed (RFC 3551 §4.5.2).
    */
  val G722: Codec = Codec("G722", 8000, 1, Some(PayloadType.G722), None)
}
