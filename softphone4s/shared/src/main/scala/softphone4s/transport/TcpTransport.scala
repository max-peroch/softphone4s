package softphone4s.transport

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, Port, SocketAddress}
import fs2.io.net.Network
import org.typelevel.log4cats.StructuredLogger

/** SIP over TCP: a [[StreamTransport]] over plain sockets. It listens on
  * `listenOn` if given, otherwise it only makes connections.
  */
object TcpTransport {

  def resource[F[_]: Async: Network](
      bindAddress: IpAddress,
      listenOn: Option[Port],
      config: StreamConfig,
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F]] =
    for {
      listener <- listenOn.traverse(port =>
        StreamListener.bind[F](SocketAddress(bindAddress, port), logger)(
          socket =>
            Resource.pure(
              StreamTransport.Connected(
                socket,
                Async[F].unit,
                Async[F].pure(Nil)
              )
            )
        )
      )
      transport <- StreamTransport.resource[F](
        bindAddress,
        config,
        connect = (to, _) =>
          Network[F]
            .connect(to)
            .map(
              StreamTransport.Connected(_, Async[F].unit, Async[F].pure(Nil))
            ),
        serverNameOf = _ => None,
        listener,
        logger
      )
    } yield transport
}
