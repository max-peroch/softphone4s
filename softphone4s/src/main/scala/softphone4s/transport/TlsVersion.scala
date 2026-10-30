package softphone4s.transport

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** The oldest TLS version we will speak; anything newer is allowed too. */
enum TlsVersion(val token: String) {
  case Tls12 extends TlsVersion("1.2")
  case Tls13 extends TlsVersion("1.3")

  /** The protocol names Java wants, this version and every later one. */
  def protocols: List[String] = this match {
    case Tls12 => List("TLSv1.2", "TLSv1.3")
    case Tls13 => List("TLSv1.3")
  }
}

object TlsVersion {
  given ConfigReader[TlsVersion] = ConfigReader[String].emap(s =>
    values
      .find(_.token == s)
      .toRight(CannotConvert(s, "TlsVersion", "expected 1.2 or 1.3"))
  )
}
