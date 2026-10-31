package softphone4s.srtp

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** Whether a call is encrypted, as the configuration asks. */
enum SrtpPolicy(val token: String) {

  /** Plain RTP only. A stream offered as SRTP is not accepted. */
  case Off extends SrtpPolicy("off")

  /** Encrypt if the other side will: offer SRTP keys alongside plain RTP, and
    * take them if the answer does. A call whose other side does not is plain.
    */
  case Optional extends SrtpPolicy("optional")

  /** Encrypt or refuse the call: offer SRTP only, and accept only what is
    * encrypted.
    */
  case Required extends SrtpPolicy("required")
}

object SrtpPolicy {

  given ConfigReader[SrtpPolicy] = ConfigReader[String].emap(name =>
    values
      .find(_.token == name)
      .toRight(
        CannotConvert(name, "SrtpPolicy", "expected off, optional or required")
      )
  )
}
