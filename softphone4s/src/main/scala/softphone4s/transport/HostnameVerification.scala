package softphone4s.transport

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** Whether the certificate a server shows must be valid for the name we meant
  * to reach (RFC 5922). Turning it off leaves a connection open to anyone who
  * can present a trusted certificate, so it is a choice you have to make.
  */
enum HostnameVerification(val token: String) {
  case Verify extends HostnameVerification("verify")
  case Skip   extends HostnameVerification("skip")
}

object HostnameVerification {
  given ConfigReader[HostnameVerification] = ConfigReader[String].emap(s =>
    values
      .find(_.token == s)
      .toRight(
        CannotConvert(s, "HostnameVerification", "expected verify or skip")
      )
  )
}
