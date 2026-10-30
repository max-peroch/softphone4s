package softphone4s.dns

import cats.syntax.all.*
import cats.Monad
import com.comcast.ip4s.{Hostname, IDN, IpAddress, Port, SocketAddress}
import softphone4s.model.{SipScheme, SipUri, TransportProtocol}
import softphone4s.runtime.Entropy

/** Where to send a request for a SIP URI (RFC 3263 §4): the list of places to
  * try, best first, each with the transport to use.
  *
  * @param supported
  *   the transports we can speak.
  */
final class TargetResolver[F[_]: Monad](
    dns: DnsRecords[F],
    entropy: Entropy[F],
    supported: Set[TransportProtocol]
) {

  import TargetResolver.*

  /** The targets for `uri`. `preferred` is the transport to use when nothing
    * else decides it. The list is empty only if nothing resolves.
    */
  def resolve(
      uri: SipUri,
      preferred: TransportProtocol
  ): F[List[ResolvedTarget]] = {
    val named = uri.transport.orElse(
      Option.when(uri.scheme == SipScheme.Sips)(TransportProtocol.Tls)
    )
    uri.host match {
      case ip: IpAddress =>
        val transport = named.getOrElse(preferred)
        List(target(transport, ip, uri.port)).pure[F]
      case host =>
        (hostName(host), uri.port) match {
          case (None, _) => List.empty[ResolvedTarget].pure[F]
          // An explicit port means no SRV lookup (§4.2).
          case (Some(name), Some(port)) =>
            val transport = named.getOrElse(preferred)
            addressesOf(name, transport, port)
          case (Some(name), None) =>
            named match {
              case Some(transport) => viaSrv(name, List(transport), transport)
              case None            => unnamed(name, preferred)
            }
        }
    }
  }

  /** No transport was asked for: the domain's NAPTR records say which it
    * offers; without them each transport's SRV name is tried; without those the
    * domain itself is looked up.
    */
  private def unnamed(
      name: Hostname,
      preferred: TransportProtocol
  ): F[List[ResolvedTarget]] =
    dns.naptr(name.toString).flatMap { records =>
      Rfc3263.naptrTransports(records, supported) match {
        case Nil =>
          viaSrv(name, Rfc3263.probeOrder(preferred, supported), preferred)
        case offered =>
          offered
            .flatTraverse((transport, srv) => fromSrv(transport, srv))
            .flatMap {
              case Nil   => direct(name, preferred)
              case found => found.pure[F]
            }
      }
    }

  /** The SRV records for each of `transports`, in that order; failing those,
    * the name's own addresses over `fallback`.
    */
  private def viaSrv(
      name: Hostname,
      transports: List[TransportProtocol],
      fallback: TransportProtocol
  ): F[List[ResolvedTarget]] =
    transports
      .flatTraverse(transport =>
        Rfc3263
          .srvName(transport, name.toString)
          .fold(List.empty[ResolvedTarget].pure[F])(srv =>
            fromSrv(transport, srv)
          )
      )
      .flatMap {
        case Nil   => direct(name, fallback)
        case found => found.pure[F]
      }

  /** The targets an SRV name leads to, in the order RFC 2782 gives. */
  private def fromSrv(
      transport: TransportProtocol,
      srvName: String
  ): F[List[ResolvedTarget]] =
    for {
      records <- dns.srv(srvName)
      draws   <- List.fill(records.size)(()).traverse(_ => entropy.fraction)
      targets <- SrvOrdering
        .order(records, draws)
        .flatTraverse(record =>
          addressesOf(record.target, transport, record.port)
        )
    } yield targets.distinct

  /** The domain's own addresses at the transport's default port. */
  private def direct(
      name: Hostname,
      transport: TransportProtocol
  ): F[List[ResolvedTarget]] =
    addressesOf(name, transport, Rfc3263.defaultPort(transport))

  private def addressesOf(
      name: Hostname,
      transport: TransportProtocol,
      port: Port
  ): F[List[ResolvedTarget]] =
    dns
      .addresses(name)
      .map(_.map(ip => ResolvedTarget(transport, SocketAddress(ip, port))))
}

object TargetResolver {

  private def target(
      transport: TransportProtocol,
      ip: IpAddress,
      port: Option[Port]
  ): ResolvedTarget =
    ResolvedTarget(
      transport,
      SocketAddress(ip, port.getOrElse(Rfc3263.defaultPort(transport)))
    )

  /** The DNS name of a host; an internationalised name by its ASCII form. */
  private def hostName(host: com.comcast.ip4s.Host): Option[Hostname] =
    host match {
      case name: Hostname => Some(name)
      case idn: IDN       => Some(idn.hostname)
      case _: IpAddress   => None
    }
}
