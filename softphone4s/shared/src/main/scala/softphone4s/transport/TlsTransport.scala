package softphone4s.transport

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{Host, IpAddress, Port, SocketAddress}
import fs2.io.net.Network
import fs2.io.net.tls.TLSParameters
import org.typelevel.log4cats.StructuredLogger
import softphone4s.model.{SipMessage, SipRequest, SipResponse, SoftphoneError}

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
      context <- Resource.eval(
        TlsPlatform.context[F](config, config.clientCertificate)
      )
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
              .withParameters(TlsPlatform.clientParameters(config, name))
              .build
          } yield StreamTransport.Connected(
            tls,
            TlsPlatform.handshaken(tls),
            TlsPlatform.identitiesOf(tls)
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
      context  <- Resource.eval(TlsPlatform.context[F](config, Some(identity)))
      listener <- StreamListener
        .bind[F](SocketAddress(bindAddress, port), logger)(socket =>
          context
            .serverBuilder(socket)
            .withParameters(TlsPlatform.serverParameters(config))
            .build
            .map(tls =>
              StreamTransport.Connected(
                tls,
                TlsPlatform.handshaken(tls),
                TlsPlatform.identitiesOf(tls)
              )
            )
        )
    } yield listener

  /** The host a message is going to: the first Route hop, else the Request-URI.
    * A response names none; it goes back on the connection its request came on.
    */
  private[transport] def serverNameOf(
      message: SipMessage
  ): Option[Host] =
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
      name: Option[Host]
  ): TLSParameters = TlsPlatform.clientParameters(config, name)

  private[transport] def serverParameters(config: TlsConfig): TLSParameters =
    TlsPlatform.serverParameters(config)
}
