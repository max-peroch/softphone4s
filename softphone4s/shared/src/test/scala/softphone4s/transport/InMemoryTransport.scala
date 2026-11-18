package softphone4s.transport

import cats.effect.Concurrent
import cats.effect.std.Queue
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import softphone4s.model.SipMessage

/** Two connected transports with no network, for deterministic tests. */
object InMemoryTransport {

  val AddressA: SocketAddress[IpAddress] =
    SocketAddress(ip"127.0.0.1", port"5060")
  val AddressB: SocketAddress[IpAddress] =
    SocketAddress(ip"127.0.0.1", port"5061")

  def pair[F[_]: Concurrent]: F[(Transport[F], Transport[F])] =
    (Queue.unbounded[F, Inbound], Queue.unbounded[F, Inbound]).mapN {
      (toA, toB) =>
        (
          endpoint(AddressA, inbox = toA, peer = toB),
          endpoint(AddressB, inbox = toB, peer = toA)
        )
    }

  private def endpoint[F[_]: Concurrent](
      self: SocketAddress[IpAddress],
      inbox: Queue[F, Inbound],
      peer: Queue[F, Inbound]
  ): Transport[F] =
    new Transport[F] {
      def localAddress: SocketAddress[IpAddress] = self
      def messages: Stream[F, Inbound]           =
        Stream.fromQueueUnterminated(inbox, limit = 1)
      def send(to: SocketAddress[IpAddress], message: SipMessage): F[Unit] =
        peer.offer(Inbound(self, message))
    }
}
