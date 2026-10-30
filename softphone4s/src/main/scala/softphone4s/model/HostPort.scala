package softphone4s.model

import cats.syntax.all.*
import com.comcast.ip4s.{Host, Port}

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
      host <- Host.fromString(hostText).toRight("invalid host")
      port <- portText.traverse(p =>
        p.stripPrefix(":")
          .toIntOption
          .flatMap(Port.fromInt)
          .toRight("invalid port")
      )
    } yield (host, port)
  }

  def render(host: Host, port: Option[Port]): String = {
    val hostText = host.toString match {
      case h if h.contains(':') => s"[$h]"
      case h                    => h
    }
    hostText + port.fold("")(p => s":${p.value}")
  }
}
