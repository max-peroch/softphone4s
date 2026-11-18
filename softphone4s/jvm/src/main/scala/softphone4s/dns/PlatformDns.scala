package softphone4s.dns

import cats.effect.Async

/** The DNS the platform has: dnsjava on the JVM. */
private[softphone4s] object PlatformDns {

  /** The servers of `config`, or the system's own if it names none. */
  def configured[F[_]: Async](config: DnsConfig): F[DnsRecords[F]] =
    DnsJavaRecords.configured[F](config)
}
