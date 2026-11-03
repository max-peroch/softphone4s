package softphone4s.transport

import cats.effect.{Async, Resource, Sync}
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Hostname, IDN, IpAddress, Port, SocketAddress}
import fs2.io.net.Network
import fs2.io.net.tls.{TLSContext, TLSParameters, TLSSocket}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.model.{SipMessage, SipRequest, SipResponse, SoftphoneError}

import java.security.SecureRandom
import java.nio.file.Files
import java.security.KeyStore
import java.security.cert.X509Certificate
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

/** SIP over TLS for a client: a [[StreamTransport]] whose sockets are wrapped
  * in TLS. The server is identified by the host the message is addressed to: it
  * is sent as the SNI name and, unless verification is switched off, the
  * certificate has to be valid for it.
  */
object TlsTransport {

  /** Fails with [[softphone4s.model.SoftphoneError.TlsConfigurationInvalid]] if
    * a key store cannot be read.
    */
  def resource[F[_]: Async: Network](
      bindAddress: IpAddress,
      listenOn: Option[Port],
      config: TlsConfig,
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F]] =
    for {
      context  <- Resource.eval(tlsContext[F](config, config.clientCertificate))
      listener <- listenOn.traverse(port =>
        listening(bindAddress, port, config, logger)
      )
      transport <- StreamTransport.resource[F](
        bindAddress,
        config.connection,
        connect = (to, name) =>
          for {
            raw <- Network[F].connect(to)
            tls <- context
              .clientBuilder(raw)
              .withParameters(parameters(config, name))
              .build
          } yield StreamTransport.Connected(
            tls,
            handshaken(tls),
            identitiesOf(tls)
          ),
        serverNameOf = serverNameOf,
        listener,
        logger
      )
    } yield transport

  /** Listens for TLS at `port`, showing `config.serverCertificate`. */
  private def listening[F[_]: Async: Network](
      bindAddress: IpAddress,
      port: Port,
      config: TlsConfig,
      logger: StructuredLogger[F]
  ): Resource[F, StreamTransport.Listener[F]] =
    for {
      identity <- Resource.eval(
        Async[F].fromOption(
          config.serverCertificate,
          SoftphoneError.TlsConfigurationInvalid(
            "listening for TLS needs a server certificate"
          )
        )
      )
      context  <- Resource.eval(tlsContext[F](config, Some(identity)))
      listener <- StreamListener
        .bind[F](SocketAddress(bindAddress, port), logger)(socket =>
          context
            .serverBuilder(socket)
            .withParameters(serverParameters(config))
            .build
            .map(tls =>
              StreamTransport.Connected(
                tls,
                handshaken(tls),
                identitiesOf(tls)
              )
            )
        )
    } yield listener

  /** The hosts the certificate the other end showed is valid for: its DNS and
    * IP subject alternative names (RFC 6125). A name with a wildcard is left
    * out, so that a host is only ever taken for one the certificate names
    * exactly. Nothing if no certificate was shown. The TLS layer has already
    * checked the chain, when it asked for one.
    */
  private def identitiesOf[F[_]: Async](tls: TLSSocket[F]): F[List[Host]] =
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

  private[transport] def serverParameters(config: TlsConfig): TLSParameters =
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
  private def handshaken[F[_]: Async](tls: TLSSocket[F]): F[Unit] = {
    val pause         = 5.millis
    def poll: F[Unit] =
      tls.session.flatMap(session =>
        if session.isValid then Async[F].unit
        else Async[F].sleep(pause) >> poll
      )
    poll
  }

  /** The host a message is going to: the first Route hop, else the Request-URI.
    * A response names none; it goes back on the connection its request came on.
    */
  private[transport] def serverNameOf(
      message: SipMessage
  ): Option[com.comcast.ip4s.Host] =
    message match {
      case request: SipRequest =>
        Some(
          request.headers.routes.headOption
            .map(_.uri.host)
            .getOrElse(request.requestUri.host)
        )
      case _: SipResponse => None
    }

  private[transport] def parameters(
      config: TlsConfig,
      name: Option[com.comcast.ip4s.Host]
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

  private def tlsContext[F[_]: Async: Network](
      config: TlsConfig,
      identity: Option[KeyStoreConfig]
  ): F[TLSContext[F]] =
    Sync[F]
      .blocking(sslContext(config, identity))
      .adaptError { case NonFatal(error) =>
        SoftphoneError.TlsConfigurationInvalid(String.valueOf(error.getMessage))
      }
      .map(Network[F].tlsContext.fromSSLContext)

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
    Using.resource(Files.newInputStream(config.path))(in =>
      store.load(in, config.password.reveal.toCharArray)
    )
    store
  }
}
