package softphone4s.dns

import com.comcast.ip4s.{Host, IpAddress}

/** The DNS lookups SIP needs. A name that does not exist, or has no records of
  * the kind asked for, gives an empty list; any other failure (no server
  * answering, say) fails the effect.
  */
trait DnsRecords[F[_]] {

  /** The SRV records at `name`, such as `_sip._tcp.example.com.`. */
  def srv(name: String): F[List[SrvRecord]]

  /** The NAPTR records at `domain`. */
  def naptr(domain: String): F[List[NaptrRecord]]

  /** The IPv4 and IPv6 addresses of `host`. */
  def addresses(host: Host): F[List[IpAddress]]
}
