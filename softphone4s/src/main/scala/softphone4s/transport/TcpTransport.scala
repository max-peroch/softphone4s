package softphone4s.transport

import cats.effect.{Async, Resource}
import com.comcast.ip4s.IpAddress
import fs2.io.net.Network
import org.typelevel.log4cats.StructuredLogger

/** SIP over TCP for a client: a [[StreamTransport]] over plain sockets. */
object TcpTransport {

  def resource[F[_]: Async: Network](
      bindAddress: IpAddress,
      config: StreamConfig,
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F]] =
    StreamTransport.resource[F](
      bindAddress,
      config,
      connect = (to, _) =>
        Network[F].connect(to).map(StreamTransport.Connected(_, Async[F].unit)),
      serverNameOf = _ => None,
      logger
    )
}
