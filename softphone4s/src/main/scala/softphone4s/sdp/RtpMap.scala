package softphone4s.sdp

/** What a payload type stands for (RFC 4566 §6, `a=rtpmap`). */
final case class RtpMap(
    payloadType: Int,
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
        (pt.toIntOption, description.split('/').toList) match {
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
  val Static: Map[Int, RtpMap] = Map(
    0  -> RtpMap(0, "PCMU", 8000, None),
    3  -> RtpMap(3, "GSM", 8000, None),
    4  -> RtpMap(4, "G723", 8000, None),
    8  -> RtpMap(8, "PCMA", 8000, None),
    9  -> RtpMap(9, "G722", 8000, None),
    18 -> RtpMap(18, "G729", 8000, None)
  )
}
