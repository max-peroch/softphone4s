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
