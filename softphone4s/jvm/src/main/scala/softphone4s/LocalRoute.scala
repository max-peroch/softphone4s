package softphone4s

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, Ipv4Address, SocketAddress}

/** The address the OS would send from to reach a remote host. */
private[softphone4s] object LocalRoute {

  /** Connecting a UDP socket only picks the route; nothing is sent. (The first
    * network interface would not do: on macOS it can be Docker Desktop's
    * bridge, 192.168.65.1.) The SDP advertises IN IP4, so an address that is
    * not IPv4 is none.
    */
  def towards[F[_]: Async](
      remote: SocketAddress[IpAddress]
  ): F[Option[IpAddress]] =
    Resource
      .fromAutoCloseable(Async[F].blocking(new java.net.DatagramSocket()))
      .use(socket =>
        Async[F].blocking {
          socket.connect(remote.toInetSocketAddress)
          socket.getLocalAddress
        }
      )
      .map(address =>
        Option(address)
          .filterNot(_.isAnyLocalAddress)
          .map(IpAddress.fromInetAddress)
          .collect { case v4: Ipv4Address => v4: IpAddress }
      )
}
