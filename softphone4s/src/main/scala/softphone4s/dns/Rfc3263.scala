package softphone4s.dns

import com.comcast.ip4s.{Port, port}
import softphone4s.model.TransportProtocol

/** The fixed parts of locating a SIP server (RFC 3263): the SRV names, the
  * NAPTR service tags, and the default ports.
  */
object Rfc3263 {

  def defaultPort(transport: TransportProtocol): Port = transport match {
    case TransportProtocol.Tls | TransportProtocol.Wss => port"5061"
    case _                                             => port"5060"
  }

  /** The SRV name for `domain` over `transport` (RFC 3263 §4.2), such as
    * `_sip._udp.example.com.`; absent for a transport with no SIP SRV name
    * here.
    */
  def srvName(transport: TransportProtocol, domain: String): Option[String] = {
    val absolute = if domain.endsWith(".") then domain else s"$domain."
    transport match {
      case TransportProtocol.Udp => Some(s"_sip._udp.$absolute")
      case TransportProtocol.Tcp => Some(s"_sip._tcp.$absolute")
      case TransportProtocol.Tls => Some(s"_sips._tcp.$absolute")
      case _                     => None
    }
  }

  /** The transport a NAPTR service tag stands for: `SIP+D2U`, `SIP+D2T` and
    * `SIPS+D2T` (RFC 3263 §4.1), whatever their case.
    */
  def transportOf(service: String): Option[TransportProtocol] =
    service.toUpperCase match {
      case "SIP+D2U"  => Some(TransportProtocol.Udp)
      case "SIP+D2T"  => Some(TransportProtocol.Tcp)
      case "SIPS+D2T" => Some(TransportProtocol.Tls)
      case _          => None
    }

  /** The transports a domain offers, best first, with the SRV name to look each
    * up at: its NAPTR records by order and then preference, keeping only those
    * that name an SRV record (flag `S`) for a transport we support, and each
    * transport once.
    */
  def naptrTransports(
      records: List[NaptrRecord],
      supported: Set[TransportProtocol]
  ): List[(TransportProtocol, String)] =
    records
      .sortBy(r => (r.order, r.preference))
      .filter(_.flags.equalsIgnoreCase("S"))
      .flatMap(r =>
        transportOf(r.service)
          .filter(supported.contains)
          .map(_ -> r.replacement)
      )
      .distinctBy(_._1)

  /** The order to look for SRV records in when the domain has no NAPTR records:
    * the transport we would use anyway first, then the secure one, then the
    * rest.
    */
  def probeOrder(
      preferred: TransportProtocol,
      supported: Set[TransportProtocol]
  ): List[TransportProtocol] =
    (preferred :: List(
      TransportProtocol.Tls,
      TransportProtocol.Tcp,
      TransportProtocol.Udp
    )).distinct.filter(supported.contains)
}
