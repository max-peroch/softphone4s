package softphone4s.dns

import cats.effect.{IO, Ref}
import com.comcast.ip4s.*

/** Canned DNS answers, and a record of what was asked. */
final class FakeDns private (
    srvs: Map[String, List[SrvRecord]],
    naptrs: Map[String, List[NaptrRecord]],
    hosts: Map[String, List[IpAddress]],
    val asked: Ref[IO, List[String]]
) extends DnsRecords[IO] {

  def srv(name: String): IO[List[SrvRecord]] =
    asked.update(_ :+ s"SRV $name") >> IO.pure(srvs.getOrElse(name, Nil))

  def naptr(domain: String): IO[List[NaptrRecord]] =
    asked.update(_ :+ s"NAPTR $domain") >> IO.pure(
      naptrs.getOrElse(domain, Nil)
    )

  def addresses(host: Host): IO[List[IpAddress]] =
    asked.update(_ :+ s"A $host") >> IO.pure(
      hosts.getOrElse(host.toString, Nil)
    )
}

object FakeDns {

  def apply(
      srvs: Map[String, List[SrvRecord]] = Map.empty,
      naptrs: Map[String, List[NaptrRecord]] = Map.empty,
      hosts: Map[String, List[IpAddress]] = Map.empty
  ): IO[FakeDns] =
    Ref.of[IO, List[String]](Nil).map(new FakeDns(srvs, naptrs, hosts, _))
}
