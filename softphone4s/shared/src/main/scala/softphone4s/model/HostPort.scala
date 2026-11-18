package softphone4s.model

import cats.syntax.all.*
import com.comcast.ip4s.{Host, Hostname, Ipv4Address, Port}

/** The `host[:port]` of a SIP URI or Via sent-by; IPv6 hosts are bracketed. */
private[model] object HostPort {

  def parse(text: String): Either[String, (Host, Option[Port])] = {
    val t                    = text.trim
    val (hostText, portText) =
      if t.startsWith("[") then
        t.indexOf(']') match {
          case -1  => (t, None)
          case end =>
            (t.slice(1, end), Some(t.drop(end + 1)).filter(_.nonEmpty))
        }
      else
        t.lastIndexOf(':') match {
          case -1  => (t, None)
          case idx => (t.take(idx), Some(t.drop(idx)))
        }
    for {
      host <- parseHost(hostText).toRight("invalid host")
      port <- portText.traverse(p =>
        p.stripPrefix(":")
          .toIntOption
          .flatMap(Port.fromInt)
          .toRight("invalid port")
      )
    } yield (host, port)
  }

  /** Same result as `Host.fromString`, which tries every name as an IP address
    * first. A dotted quad is read directly, and a name with a letter and no
    * colon can only be a hostname.
    */
  private[model] def parseHost(text: String): Option[Host] =
    if isDottedDigits(text) then ipv4(text).orElse(Host.fromString(text))
    else if text.indexOf(':') < 0 && text.exists(_.isLetter) then
      Hostname.fromString(text).orElse(Host.fromString(text))
    else Host.fromString(text)

  private def isDottedDigits(s: String): Boolean =
    s.nonEmpty && s.forall(c => (c >= '0' && c <= '9') || c == '.')

  /** `a.b.c.d` with each part 0-255, or `None`. */
  private def ipv4(s: String): Option[Host] = {
    val parts = s.split('.')
    if parts.length != 4 then None
    else {
      val octets = parts.map(p =>
        Option
          .when(p.nonEmpty && p.length <= 3)(p.toIntOption)
          .flatten
          .filter(_ <= 255)
      )
      if octets.forall(_.isDefined) then
        octets.flatten match {
          case Array(a, b, c, d) =>
            Some(Ipv4Address.fromBytes(a, b, c, d))
          case _ => None
        }
      else None
    }
  }

  def render(host: Host, port: Option[Port]): String = {
    val hostText = host.toString match {
      case h if h.contains(':') => s"[$h]"
      case h                    => h
    }
    hostText + port.fold("")(p => s":${p.value}")
  }
}
