package softphone4s.transport

import cats.effect.{Async, Sync}
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Hostname, IDN, IpAddress}
import fs2.io.net.Network
import fs2.io.net.tls.{TLSContext, TLSParameters, TLSSocket}
import softphone4s.model.SoftphoneError

import java.nio.file.Files
import java.security.cert.X509Certificate
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{
  KeyManagerFactory,
  SNIHostName,
  SSLContext,
  SSLPeerUnverifiedException,
  TrustManagerFactory
}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/** What `TlsTransport` needs of the TLS of the platform: here the JVM's,
  * through `javax.net.ssl`.
  */
private[transport] object TlsPlatform {

  /** The context that trusts `config.trustStore` and shows `identity`. Fails
    * with [[softphone4s.model.SoftphoneError.TlsConfigurationInvalid]] if a key
    * store cannot be read.
    */
  def context[F[_]: Async: Network](
      config: TlsConfig,
      identity: Option[KeyStoreConfig]
  ): F[TLSContext[F]] =
    Sync[F]
      .blocking(sslContext(config, identity))
      .adaptError { case NonFatal(error) =>
        SoftphoneError.TlsConfigurationInvalid(String.valueOf(error.getMessage))
      }
      .map(Network[F].tlsContext.fromSSLContext)

  /** The parameters of a connection we open to the host a message is for. */
  def clientParameters(
      config: TlsConfig,
      name: Option[Host]
  ): TLSParameters =
    TLSParameters(
      protocols = Some(config.minVersion.protocols),
      cipherSuites = config.cipherSuites,
      // A literal IP address is not a name SNI can carry (RFC 6066).
      serverNames = name.flatMap {
        case host: Hostname => Some(List(new SNIHostName(host.toString)))
        case host: IDN    => Some(List(new SNIHostName(host.hostname.toString)))
        case _: IpAddress => None
      },
      endpointIdentificationAlgorithm =
        Option.when(config.hostnameVerification == HostnameVerification.Verify)(
          "HTTPS"
        )
    )

  /** The parameters of a connection we accept. */
  def serverParameters(config: TlsConfig): TLSParameters =
    TLSParameters(
      protocols = Some(config.minVersion.protocols),
      cipherSuites = config.cipherSuites,
      needClientAuth =
        config.clientAuthentication == ClientAuthentication.Require,
      wantClientAuth =
        config.clientAuthentication == ClientAuthentication.Request
    )

  /** Completes when the handshake has. Nothing here moves it along: the reading
    * of the connection does, and a failure surfaces there.
    */
  def handshaken[F[_]: Async](tls: TLSSocket[F]): F[Unit] = {
    val pause         = 5.millis
    def poll: F[Unit] =
      tls.session.flatMap(session =>
        if session.isValid then Async[F].unit
        else Async[F].sleep(pause) >> poll
      )
    poll
  }

  /** The hosts the certificate the other end showed is valid for: its DNS and
    * IP subject alternative names (RFC 6125). A name with a wildcard is left
    * out, so that a host is only ever taken for one the certificate names
    * exactly. Nothing if no certificate was shown. The TLS layer has already
    * checked the chain, when it asked for one.
    */
  def identitiesOf[F[_]: Async](tls: TLSSocket[F]): F[List[Host]] =
    tls.session.flatMap(session =>
      Sync[F]
        .delay(session.getPeerCertificates.toList)
        .map(_.collectFirst { case cert: X509Certificate => namesIn(cert) })
        .map(_.getOrElse(Nil))
        .recover { case _: SSLPeerUnverifiedException => Nil }
    )

  private val DnsName = 2
  private val IpName  = 7

  private def namesIn(cert: X509Certificate): List[Host] =
    Option(cert.getSubjectAlternativeNames).toList
      .flatMap(_.asScala.toList)
      .flatMap(entry =>
        entry.asScala.toList match {
          case (kind: Integer) :: (value: String) :: _
              if (kind == DnsName || kind == IpName) && !value.contains('*') =>
            Host.fromString(value)
          case _ => None
        }
      )

  /** The trust is `config.trustStore`; the key we show is `identity`. */
  private def sslContext(
      config: TlsConfig,
      identity: Option[KeyStoreConfig]
  ): SSLContext = {
    val trust = config.trustStore.map { store =>
      val factory =
        TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      factory.init(keyStore(store))
      factory.getTrustManagers
    }
    val keys = identity.map { store =>
      val factory =
        KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
      factory.init(keyStore(store), store.password.reveal.toCharArray)
      factory.getKeyManagers
    }
    val context = SSLContext.getInstance("TLS")
    // The Java API takes `null` for the JVM's own key and trust managers, which
    // is the only way to ask for them; the source of randomness is made here
    // and not left to default.
    context.init(keys.orNull, trust.orNull, new SecureRandom())
    context
  }

  private def keyStore(config: KeyStoreConfig): KeyStore = {
    val store = KeyStore.getInstance(config.storeType.javaName)
    Using.resource(Files.newInputStream(config.path.toNioPath))(in =>
      store.load(in, config.password.reveal.toCharArray)
    )
    store
  }
}
