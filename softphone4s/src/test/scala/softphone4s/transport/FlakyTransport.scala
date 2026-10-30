package softphone4s.transport

import cats.effect.IO
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import softphone4s.model.SipMessage

import java.io.IOException

/** A transport that cannot reach some addresses: sends to the ones in `failing`
  * fail at once, as a refused connection would, and sends to the ones in
  * `silent` vanish, as to a host that is down.
  */
object FlakyTransport {

  def apply(
      inner: Transport[IO],
      failing: Set[SocketAddress[IpAddress]] = Set.empty,
      silent: Set[SocketAddress[IpAddress]] = Set.empty
  ): Transport[IO] =
    new Transport[IO] {
      def localAddress: SocketAddress[IpAddress] = inner.localAddress
      def messages: Stream[IO, Inbound]          = inner.messages
      def send(to: SocketAddress[IpAddress], message: SipMessage): IO[Unit] =
        if failing.contains(to) then
          IO.raiseError(new IOException(s"cannot reach $to"))
        else if silent.contains(to) then IO.unit
        else inner.send(to, message)
    }
}
