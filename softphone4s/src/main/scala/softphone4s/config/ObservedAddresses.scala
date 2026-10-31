package softphone4s.config

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** Whether to believe what servers say we look like from outside (their
  * `received` and `rport`, RFC 3581) and advertise it.
  */
enum ObservedAddresses(val token: String) {

  /** Put the observed address in the Contact and in SDP, so that a registrar
    * and the people we call can reach us behind a NAT.
    */
  case Use extends ObservedAddresses("use")

  /** Keep to the address we bound. */
  case Ignore extends ObservedAddresses("ignore")
}

object ObservedAddresses {
  given ConfigReader[ObservedAddresses] = ConfigReader[String].emap(s =>
    values
      .find(_.token == s)
      .toRight(CannotConvert(s, "ObservedAddresses", "expected use or ignore"))
  )
}
