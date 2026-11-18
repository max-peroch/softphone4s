package softphone4s.sdp

/** The transport protocol of an `m=` line. */
enum MediaProtocol(val token: String) {

  /** RTP with the audio/video profile (RFC 3551). */
  case RtpAvp extends MediaProtocol("RTP/AVP")

  /** The same, secured (RFC 3711). */
  case RtpSavp             extends MediaProtocol("RTP/SAVP")
  case Other(name: String) extends MediaProtocol(name)
}

object MediaProtocol {
  def fromToken(token: String): MediaProtocol =
    List(RtpAvp, RtpSavp).find(_.token == token).getOrElse(Other(token))
}
