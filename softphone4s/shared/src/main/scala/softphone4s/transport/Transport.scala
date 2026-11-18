package softphone4s.transport

import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import softphone4s.model.SipMessage

/** How SIP messages move between this stack and the network. UDP is the only
  * implementation so far ([[SipTransport.udp]]); TCP and TLS follow.
  */
trait Transport[F[_]] {

  /** The address bound locally, with the port the OS chose if 0 was asked. */
  def localAddress: SocketAddress[IpAddress]

  /** Every message received that decodes; the others are logged and dropped. */
  def messages: Stream[F, Inbound]

  def send(to: SocketAddress[IpAddress], message: SipMessage): F[Unit]
}
