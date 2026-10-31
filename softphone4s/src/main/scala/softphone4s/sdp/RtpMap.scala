package softphone4s.sdp

import softphone4s.model.PayloadType

/** What a payload type stands for (RFC 4566 §6, `a=rtpmap`). */
final case class RtpMap(
    payloadType: PayloadType,
    encoding: String,
    clockRate: Int,
    channels: Option[Int]
) {

  /** The number of channels, which is one if the line does not say. */
  def channelCount: Int = channels.getOrElse(1)

  /** Whether this is `codec`: encoding names are not case-sensitive. */
  def is(codec: Codec): Boolean =
    encoding.equalsIgnoreCase(codec.encoding) &&
      clockRate == codec.clockRate && channelCount == codec.channels

  def attribute: Attribute =
    Attribute.valued(
      "rtpmap",
      s"$payloadType $encoding/$clockRate${channels.fold("")(c => s"/$c")}"
    )
}

object RtpMap {

  /** `0 PCMU/8000` or `96 opus/48000/2`. */
  def parse(value: String): Option[RtpMap] =
    value.trim.split("\\s+", 2).toList match {
      case pt :: description :: Nil =>
        (
          pt.toIntOption.flatMap(PayloadType.fromInt),
          description.split('/').toList
        ) match {
          case (Some(payloadType), encoding :: rate :: rest) =>
            (rate.toIntOption, rest) match {
              case (Some(clockRate), Nil) =>
                Some(RtpMap(payloadType, encoding, clockRate, None))
              case (Some(clockRate), channels :: Nil) =>
                channels.toIntOption.map(c =>
                  RtpMap(payloadType, encoding, clockRate, Some(c))
                )
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

  /** The payload types of RFC 3551 that have a fixed meaning, for the ones that
    * need no `rtpmap`.
    */
  val Static: Map[PayloadType, RtpMap] = List(
    RtpMap(PayloadType.Pcmu, "PCMU", 8000, None),
    RtpMap(PayloadType.Gsm, "GSM", 8000, None),
    RtpMap(PayloadType.G723, "G723", 8000, None),
    RtpMap(PayloadType.Pcma, "PCMA", 8000, None),
    RtpMap(PayloadType.G722, "G722", 8000, None),
    RtpMap(PayloadType.G729, "G729", 8000, None)
  ).map(m => m.payloadType -> m).toMap
}
