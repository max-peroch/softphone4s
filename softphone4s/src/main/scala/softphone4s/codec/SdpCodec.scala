package softphone4s.codec

import com.comcast.ip4s.{IpAddress, Port}

/** Minimal SDP offer/answer codec for a single audio stream. */
object SdpCodec {

  /** Extract the remote RTP endpoint from an SDP body. */
  def parseAudioEndpoint(
      sdp: String,
      defaultIp: IpAddress,
      defaultPort: Port
  ): (IpAddress, Port) = {
    val lines = sdp.linesIterator.map(_.trim).toList

    val remoteIp = lines
      .find(_.startsWith("c=IN IP4 "))
      .flatMap(l => IpAddress.fromString(l.stripPrefix("c=IN IP4 ").trim))
      .getOrElse(defaultIp)

    val remotePort = lines
      .find(_.startsWith("m=audio "))
      .flatMap(_.split(" ").lift(1))
      .flatMap(_.toIntOption)
      .flatMap(Port.fromInt)
      .getOrElse(defaultPort)

    (remoteIp, remotePort)
  }

  /** What a caller's SDP offers for audio. */
  final case class AudioOffer(
      ip: IpAddress,
      port: Port,
      payloadTypes: List[Int],
      offersTelephoneEvent: Boolean
  ) {

    /** We can only speak PCMU (payload type 0). */
    def isAcceptable: Boolean = payloadTypes.contains(PcmuPayloadType)
  }

  private val PcmuPayloadType           = 0
  private val TelephoneEventPayloadType = 101

  /** The audio an SDP body offers, or `None` if it has no audio stream we can
    * address. Telephone-event counts only on payload type 101, the one the RTP
    * code receives.
    */
  def parseOffer(sdp: String): Option[AudioOffer] = {
    val lines = sdp.linesIterator.map(_.trim).toList
    for {
      media <- lines.find(_.startsWith("m=audio "))
      parts = media.split(' ').toList
      port <- parts.lift(1).flatMap(_.toIntOption).flatMap(Port.fromInt)
      payloads = parts.drop(3).flatMap(_.toIntOption)
      ip <- lines
        .find(_.startsWith("c=IN IP4 "))
        .flatMap(l => IpAddress.fromString(l.stripPrefix("c=IN IP4 ").trim))
    } yield AudioOffer(
      ip,
      port,
      payloads,
      offersTelephoneEvent = payloads.contains(TelephoneEventPayloadType) &&
        lines.exists(l =>
          l.startsWith(s"a=rtpmap:$TelephoneEventPayloadType ") &&
            l.toLowerCase.contains("telephone-event")
        )
    )
  }

  /** An answer to `offer`: PCMU, and telephone-event only if it was offered, so
    * the answer stays within what the caller proposed (RFC 3264 §6).
    */
  def buildAnswerTo(
      offer: AudioOffer,
      localIp: IpAddress,
      localRtpPort: Port
  ): String = {
    val payloads =
      if offer.offersTelephoneEvent then
        s"$PcmuPayloadType $TelephoneEventPayloadType"
      else s"$PcmuPayloadType"
    val events =
      if offer.offersTelephoneEvent then
        s"""|a=rtpmap:$TelephoneEventPayloadType telephone-event/8000
            |a=fmtp:$TelephoneEventPayloadType 0-16
            |""".stripMargin
      else ""
    s"""|v=0
        |o=- 0 0 IN IP4 $localIp
        |s=-
        |c=IN IP4 $localIp
        |t=0 0
        |m=audio $localRtpPort RTP/AVP $payloads
        |a=rtpmap:$PcmuPayloadType PCMU/8000
        |${events}a=sendrecv
        |""".stripMargin.replace("\n", "\r\n")
  }

  /** Build a minimal SDP offer. */
  def buildOffer(localIp: IpAddress, localRtpPort: Port): String =
    s"""|v=0
        |o=- 0 0 IN IP4 $localIp
        |s=-
        |c=IN IP4 $localIp
        |t=0 0
        |m=audio $localRtpPort RTP/AVP 0 101
        |a=rtpmap:0 PCMU/8000
        |a=rtpmap:101 telephone-event/8000
        |a=fmtp:101 0-16
        |a=sendrecv
        |""".stripMargin.replace("\n", "\r\n")

  /** Build a minimal SDP answer, offering the same fixed codec set as
    * `buildOffer`.
    */
  def buildAnswer(localIp: IpAddress, localRtpPort: Port): String =
    buildOffer(localIp, localRtpPort)
}
