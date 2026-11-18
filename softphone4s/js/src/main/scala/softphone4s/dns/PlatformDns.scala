package softphone4s.dns

import cats.effect.Async
import cats.syntax.all.*
import com.comcast.ip4s.{Dns, Host, Hostname, IpAddress, Port}
import softphone4s.model.SoftphoneError

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** The DNS the platform has: the resolver of Node. Node gives no TTL for SRV or
  * NAPTR records, and `DnsRecords` has none to give, so nothing is cached here
  * beyond what the resolver does.
  */
private[softphone4s] object PlatformDns {

  @js.native
  private trait SrvAnswer extends js.Object {
    val priority: Int = js.native
    val weight: Int   = js.native
    val port: Int     = js.native
    val name: String  = js.native
  }

  @js.native
  private trait NaptrAnswer extends js.Object {
    val order: Int          = js.native
    val preference: Int     = js.native
    val flags: String       = js.native
    val service: String     = js.native
    val replacement: String = js.native
  }

  private trait ResolverOptions extends js.Object {
    var timeout: js.UndefOr[Int] = js.undefined
  }

  @js.native
  @JSImport("node:dns/promises", "Resolver")
  private class Resolver(@unused options: ResolverOptions) extends js.Object {
    def setServers(servers: js.Array[String]): Unit               = js.native
    def resolveSrv(name: String): js.Promise[js.Array[SrvAnswer]] =
      js.native
    def resolveNaptr(name: String): js.Promise[js.Array[NaptrAnswer]] =
      js.native
  }

  /** The answers that say there is nothing, not a failure of the lookup. The
    * resolver names them in the message of its error, `queryNaptr ENODATA
    * example.com`, and gives the same error for a server that did not answer.
    */
  private val NotThere = List("ENODATA", "ENOTFOUND")

  /** The servers of `config`, or the system's own if it names none. */
  def configured[F[_]: Async](config: DnsConfig): F[DnsRecords[F]] =
    Async[F]
      .delay {
        val options = new ResolverOptions {
          timeout = config.timeout.value.toMillis.toInt
        }
        val resolver = new Resolver(options)
        if config.servers.nonEmpty then
          resolver.setServers(
            js.Array(
              config.servers.map(server =>
                server.host match {
                  case v6 if v6.toString.contains(':') =>
                    s"[$v6]:${server.port.value}"
                  case v4 => s"$v4:${server.port.value}"
                }
              )*
            )
          )
        resolver
      }
      .adaptError { case error =>
        SoftphoneError.HostResolutionFailed(
          s"DNS is not configured: ${error.getMessage}"
        )
      }
      .map(new Records[F](_))

  private final class Records[F[_]: Async](resolver: Resolver)
      extends DnsRecords[F] {

    def srv(name: String): F[List[SrvRecord]] =
      lookup(name)(resolver.resolveSrv(name)).map(
        _.toList.flatMap(answer =>
          (
            Port.fromInt(answer.port),
            Hostname.fromString(answer.name.stripSuffix("."))
          ).mapN(SrvRecord(answer.priority, answer.weight, _, _))
        )
      )

    def naptr(domain: String): F[List[NaptrRecord]] =
      lookup(domain)(resolver.resolveNaptr(domain)).map(
        _.toList.map(answer =>
          NaptrRecord(
            answer.order,
            answer.preference,
            answer.flags,
            answer.service,
            answer.replacement
          )
        )
      )

    def addresses(host: Host): F[List[IpAddress]] =
      host.resolveAll[F](using Dns.forAsync[F], Async[F]).handleError(_ => Nil)

    private def lookup[A](name: String)(
        query: => js.Promise[js.Array[A]]
    ): F[js.Array[A]] =
      Async[F]
        .fromPromise(Async[F].delay(query))
        .recover {
          case js.JavaScriptException(error: js.Error)
              if NotThere.exists(error.message.contains) =>
            js.Array[A]()
        }
        .adaptError { case error =>
          SoftphoneError.HostResolutionFailed(s"$name: ${error.getMessage}")
        }
  }
}
