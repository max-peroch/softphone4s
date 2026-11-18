package softphone4s.dns

import cats.data.NonEmptyList
import cats.effect.{Async, Sync}
import cats.syntax.all.*
import com.comcast.ip4s.{Dns, Host, Hostname, IpAddress, Port, SocketAddress}
import org.xbill.DNS.lookup.{
  LookupResult,
  LookupSession,
  NoSuchDomainException,
  NoSuchRRSetException
}
import org.xbill.DNS.{
  Cache,
  ExtendedResolver,
  Resolver,
  SimpleResolver,
  NAPTRRecord,
  Name,
  Record,
  SRVRecord as DnsSrv,
  Type
}
import softphone4s.model.SoftphoneError

import java.util.concurrent.CompletionException
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

/** [[DnsRecords]]: SRV and NAPTR records from dnsjava, asking the servers the
  * system is configured with and keeping answers for as long as their TTLs
  * allow; host addresses from the system resolver.
  */
object DnsJavaRecords {

  /** Asks the servers the system is configured with. */
  def apply[F[_]: Async]: F[DnsRecords[F]] =
    session(new ExtendedResolver())

  /** Asks the servers `config` names, or the system's if it names none. */
  def configured[F[_]: Async](config: DnsConfig): F[DnsRecords[F]] =
    NonEmptyList
      .fromList(config.servers)
      .fold(apply[F])(against(_, config.timeout.value))

  /** Asks `servers` in turn, waiting at most `timeout` for each answer. */
  def against[F[_]: Async](
      servers: NonEmptyList[SocketAddress[IpAddress]],
      timeout: FiniteDuration
  ): F[DnsRecords[F]] =
    session {
      val resolver = new ExtendedResolver(
        servers.toList.map { server =>
          val one = new SimpleResolver(server.toInetSocketAddress)
          one.setTimeout(java.time.Duration.ofMillis(timeout.toMillis))
          one: Resolver
        }.toArray
      )
      resolver
    }

  private def session[F[_]: Async](
      resolver: => Resolver
  ): F[DnsRecords[F]] =
    Sync[F]
      .blocking(
        LookupSession
          .builder()
          .resolver(resolver)
          .cache(new Cache())
          .build()
      )
      .adaptError { case error =>
        SoftphoneError.HostResolutionFailed(
          s"DNS is not configured: ${error.getMessage}"
        )
      }
      .map(session => new Records[F](session))

  /** A lookup that failed because the name or the records are not there, as
    * dnsjava reports it, whether or not it is wrapped.
    */
  private object NotThere {
    def unapply(error: Throwable): Boolean = error match {
      case _: NoSuchDomainException | _: NoSuchRRSetException => true
      case wrapper: CompletionException => unapply(wrapper.getCause)
      case _                            => false
    }
  }

  private final class Records[F[_]: Async](session: LookupSession)
      extends DnsRecords[F] {

    def srv(name: String): F[List[SrvRecord]] =
      lookup(name, Type.SRV).map(_.flatMap {
        case record: DnsSrv =>
          (
            Port.fromInt(record.getPort),
            Hostname.fromString(record.getTarget.toString(true))
          ).mapN(SrvRecord(record.getPriority, record.getWeight, _, _))
        case _ => None
      })

    def naptr(domain: String): F[List[NaptrRecord]] =
      lookup(domain, Type.NAPTR).map(_.collect { case record: NAPTRRecord =>
        NaptrRecord(
          record.getOrder,
          record.getPreference,
          record.getFlags,
          record.getService,
          record.getReplacement.toString
        )
      })

    /** Host names go to the system resolver, not to dnsjava: it knows the hosts
      * file, split DNS and the rest of what the machine is set up with, none of
      * which a query straight to a DNS server would. A host it cannot resolve
      * has no addresses.
      */
    def addresses(host: Host): F[List[IpAddress]] =
      host.resolveAll[F](using Dns.forAsync[F], Async[F]).handleError(_ => Nil)

    /** The records of `type` at `name`; none if the name or the records are not
      * there, and a failed effect if the lookup itself failed.
      */
    private def lookup(name: String, dnsType: Int): F[List[Record]] =
      Sync[F]
        .delay(
          Name.fromString(
            if name.endsWith(".") then name else s"$name.",
            Name.root
          )
        )
        .attempt
        .flatMap {
          case Left(_)        => List.empty[Record].pure[F]
          case Right(dnsName) =>
            Async[F]
              .fromCompletableFuture(
                Sync[F].delay(
                  session.lookupAsync(dnsName, dnsType).toCompletableFuture
                )
              )
              .map((result: LookupResult) => result.getRecords.asScala.toList)
              .recover { case NotThere() => Nil }
              .adaptError { case error =>
                SoftphoneError.HostResolutionFailed(
                  s"$name: ${error.getMessage}"
                )
              }
        }
  }
}
