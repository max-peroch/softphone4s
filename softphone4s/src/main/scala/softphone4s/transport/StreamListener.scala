package softphone4s.transport

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{GenSocketAddress, IpAddress, SocketAddress}
import fs2.Stream
import fs2.io.net.{Network, Socket}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.model.SoftphoneError
import softphone4s.transport.StreamTransport.{Accepted, Connected, Listener}

/** A listening socket for [[StreamTransport]]. */
object StreamListener {

  /** Listens at `address`. `wrap` turns each socket that arrives into the
    * connection to serve: itself for TCP, with a handshake started for TLS. A
    * connection that cannot be set up is dropped, and does not stop the others
    * from being served.
    */
  def bind[F[_]: Async: Network](
      address: SocketAddress[IpAddress],
      logger: StructuredLogger[F]
  )(
      wrap: Socket[F] => Resource[F, Connected[F]]
  ): Resource[F, Listener[F]] =
    Network[F].bind(address).evalMap { server =>
      ipOf[F](server.address).map { bound =>
        Listener[F](
          bound,
          (limit, handler) =>
            server.accept
              .map { socket =>
                Stream.eval(
                  wrap(socket)
                    .use(connected =>
                      ipOf[F](socket.peerAddress)
                        .flatMap(remote => handler(Accepted(connected, remote)))
                    )
                    .handleErrorWith(error =>
                      logger.debug(
                        Map("error" -> String.valueOf(error.getMessage))
                      )("An incoming connection could not be served")
                    )
                )
              }
              .parJoin(limit)
              .compile
              .drain
        )
      }
    }

  private def ipOf[F[_]: Async](
      address: GenSocketAddress
  ): F[SocketAddress[IpAddress]] =
    address match {
      case a: SocketAddress[?] =>
        a.host match {
          case ip: IpAddress => Async[F].pure(SocketAddress(ip, a.port))
          case _ => Async[F].raiseError(SoftphoneError.TransportFailed)
        }
      case _ => Async[F].raiseError(SoftphoneError.TransportFailed)
    }
}
