package softphone4s.dns

import com.comcast.ip4s.{Hostname, Port}

/** A service record (RFC 2782): where a service is, and how to choose between
  * the places that offer it.
  */
final case class SrvRecord(
    priority: Int,
    weight: Int,
    port: Port,
    target: Hostname
)
