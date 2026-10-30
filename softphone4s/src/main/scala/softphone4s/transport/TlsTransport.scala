package softphone4s.transport

import cats.effect.{Async, Resource, Sync}
import cats.syntax.all.*
import com.comcast.ip4s.{Hostname, IDN, IpAddress}
import fs2.io.net.Network
import fs2.io.net.tls.{TLSContext, TLSParameters, TLSSocket}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.model.{SipMessage, SipRequest, SipResponse, SoftphoneError}

import java.nio.file.Files
import java.security.KeyStore
import javax.net.ssl.{
  KeyManagerFactory,
  SNIHostName,
  SSLContext,
  TrustManagerFactory
}
import scala.concurrent.duration.*
import scala.util.Using
import scala.util.control.NonFatal

/** SIP over TLS for a client: a [[StreamTransport]] whose sockets are wrapped
  * in TLS. The server is identified by the host the message is addressed to: it
  * is sent as the SNI name and, unless verification is switched off, the
  * certificate has to be valid for it.
  */
object TlsTransport {

  /** Fails with [[SoftphoneError.TlsConfigurationInvalid]] if a key store
    * cannot be read.
    */
  def resource[F[_]: Async: Network](
      bindAddress: IpAddress,
      config: TlsConfig,
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F]] =
    for {
      context   <- Resource.eval(tlsContext[F](config))
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
          } yield StreamTransport.Connected(tls, handshaken(tls)),
        serverNameOf = serverNameOf,
        logger
      )
    } yield transport

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
      config: TlsConfig
  ): F[TLSContext[F]] =
    Sync[F]
      .blocking(sslContext(config))
      .adaptError { case NonFatal(error) =>
        SoftphoneError.TlsConfigurationInvalid(String.valueOf(error.getMessage))
      }
      .map(Network[F].tlsContext.fromSSLContext)

  private def sslContext(config: TlsConfig): SSLContext = {
    val trust = config.trustStore.map { store =>
      val factory =
        TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      factory.init(keyStore(store))
      factory.getTrustManagers
    }
    val keys = config.clientCertificate.map { store =>
      val factory =
        KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
      factory.init(keyStore(store), store.password.toCharArray)
      factory.getKeyManagers
    }
    val context = SSLContext.getInstance("TLS")
    // `null` asks for the JVM's own key and trust managers.
    context.init(keys.orNull, trust.orNull, null)
    context
  }

  private def keyStore(config: KeyStoreConfig): KeyStore = {
    val store = KeyStore.getInstance(config.storeType)
    Using.resource(Files.newInputStream(config.path))(in =>
      store.load(in, config.password.toCharArray)
    )
    store
  }
}
