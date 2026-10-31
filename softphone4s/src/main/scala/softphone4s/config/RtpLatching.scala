package softphone4s.config

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** Whether to send media to where the remote party's media comes from, when
  * that is not where their SDP said (symmetric RTP, RFC 4961).
  */
enum RtpLatching(val token: String) {

  /** Follow it: it is what makes media work when the remote party, or we, are
    * behind a NAT that changes the port.
    */
  case Follow extends RtpLatching("follow")

  /** Send only where the SDP says. */
  case Ignore extends RtpLatching("ignore")
}

object RtpLatching {
  given ConfigReader[RtpLatching] = ConfigReader[String].emap(s =>
    values
      .find(_.token == s)
      .toRight(CannotConvert(s, "RtpLatching", "expected follow or ignore"))
  )
}
