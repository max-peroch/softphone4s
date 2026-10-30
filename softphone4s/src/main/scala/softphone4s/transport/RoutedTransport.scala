package softphone4s.transport

import cats.effect.Async
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import softphone4s.model.{
  SipMessage,
  SipRequest,
  SipResponse,
  SoftphoneError,
  TransportProtocol
}

/** Several transports used as one. A message goes out on the transport its top
  * Via names: for a request that is the one we chose, for a response the one
  * the request arrived on (RFC 3261 §18.2.2). What any of them receives is
  * delivered together.
  */
object RoutedTransport {

  def apply[F[_]: Async](
      udp: Transport[F],
      tcp: Transport[F],
      tls: Transport[F]
  ): Transport[F] =
    new Transport[F] {
      def localAddress: SocketAddress[IpAddress] = udp.localAddress

      def messages: Stream[F, Inbound] =
        udp.messages.merge(tcp.messages).merge(tls.messages)

      def send(to: SocketAddress[IpAddress], message: SipMessage): F[Unit] =
        protocolOf(message) match {
          case TransportProtocol.Udp => udp.send(to, message)
          case TransportProtocol.Tcp => tcp.send(to, message)
          case TransportProtocol.Tls => tls.send(to, message)
          case other                 =>
            Async[F].raiseError(
              SoftphoneError.TransportUnavailable(other.token)
            )
        }
    }

  private def protocolOf(message: SipMessage): TransportProtocol =
    message match {
      case request: SipRequest   => request.headers.topVia.transport
      case response: SipResponse => response.headers.topVia.transport
    }
}
