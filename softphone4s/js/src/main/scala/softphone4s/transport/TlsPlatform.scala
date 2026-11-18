package softphone4s.transport

import cats.effect.Async
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Hostname, IDN, IpAddress}
import fs2.Chunk
import fs2.io.file.Files
import fs2.io.net.Network
import fs2.io.net.tls.SecureContext.{Pfx, SecureVersion}
import fs2.io.net.tls.{SecureContext, TLSContext, TLSParameters, TLSSocket}
import softphone4s.config.KeyStoreType
import softphone4s.model.SoftphoneError

import scala.annotation.unused

/** What `TlsTransport` needs of the TLS of the platform: here Node's, through
  * the `tls` module.
  *
  * What Node cannot do, and what is not done:
  *   - a trust store: Node takes PEM certificates for that, not a key store, so
  *     it trusts its own roots, and a `trustStore` is refused;
  *   - a JKS key store, which only the JVM reads (a PKCS12 one is read);
  *   - `cipherSuites`, which Node names as OpenSSL does and the JVM does not;
  *   - the names in the certificate of the other end: `identitiesOf` is empty,
  *     so a connection serves only the host it was opened for.
  */
private[transport] object TlsPlatform {

  /** The context that shows `identity`. Fails with
    * [[softphone4s.model.SoftphoneError.TlsConfigurationInvalid]] if the
    * configuration asks for what Node cannot do, or a key store cannot be read.
    */
  def context[F[_]: Async: Network](
      config: TlsConfig,
      identity: Option[KeyStoreConfig]
  ): F[TLSContext[F]] =
    (for {
      _   <- Async[F].fromEither(supported(config, identity))
      pfx <- identity.traverse(store =>
        Files
          .forAsync[F]
          .readAll(store.path)
          .compile
          .to(Chunk)
          .map(bytes => Pfx(Left(bytes), Some(store.password.reveal)))
      )
    } yield Network[F].tlsContext.fromSecureContext(
      SecureContext(
        pfx = pfx.map(List(_)),
        minVersion = Some(config.minVersion match {
          case TlsVersion.Tls12 => SecureVersion.`TLSv1.2`
          case TlsVersion.Tls13 => SecureVersion.`TLSv1.3`
        })
      )
    )).adaptError {
      case error: SoftphoneError.TlsConfigurationInvalid => error
      case error                                         =>
        SoftphoneError.TlsConfigurationInvalid(String.valueOf(error.getMessage))
    }

  private def supported(
      config: TlsConfig,
      identity: Option[KeyStoreConfig]
  ): Either[SoftphoneError.TlsConfigurationInvalid, Unit] =
    Either.cond(
      config.trustStore.isEmpty,
      (),
      SoftphoneError.TlsConfigurationInvalid(
        "a trust store is not supported on Node: it trusts its own roots"
      )
    ) *> Either.cond(
      config.cipherSuites.isEmpty,
      (),
      SoftphoneError.TlsConfigurationInvalid(
        "cipher suites cannot be chosen on Node"
      )
    ) *> Either.cond(
      !identity.exists(_.storeType == KeyStoreType.Jks),
      (),
      SoftphoneError.TlsConfigurationInvalid(
        "a JKS key store cannot be read on Node: use PKCS12"
      )
    )

  /** The parameters of a connection we open to the host a message is for. */
  def clientParameters(
      config: TlsConfig,
      name: Option[Host]
  ): TLSParameters =
    TLSParameters(
      // A literal IP address is not a name SNI can carry (RFC 6066).
      servername = name.flatMap {
        case host: Hostname => Some(host.toString)
        case host: IDN      => Some(host.hostname.toString)
        case _: IpAddress   => None
      },
      // Node checks the name in the certificate unless it is told not to; the
      // chain is checked either way.
      checkServerIdentity =
        Option.when(config.hostnameVerification == HostnameVerification.Skip)(
          new TLSParameters.CheckServerIdentity {
            def apply(
                @unused servername: String,
                @unused cert: Chunk[Byte]
            ) = Right(())
          }
        )
    )

  /** The parameters of a connection we accept. */
  def serverParameters(config: TlsConfig): TLSParameters =
    TLSParameters(
      requestCert =
        Some(config.clientAuthentication != ClientAuthentication.Off),
      rejectUnauthorized =
        Some(config.clientAuthentication == ClientAuthentication.Require)
    )

  /** Node has finished the handshake when the connection is built. */
  def handshaken[F[_]: Async](@unused tls: TLSSocket[F]): F[Unit] =
    Async[F].unit

  /** Nothing: the certificate of the other end is not shown by the TLS socket
    * of fs2 on this platform.
    */
  def identitiesOf[F[_]: Async](@unused tls: TLSSocket[F]): F[List[Host]] =
    Async[F].pure(Nil)
}
