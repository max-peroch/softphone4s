package softphone4s.stun

import cats.effect.Async
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.io.net.{Datagram, Network}
import softphone4s.config.Attempts

import scala.concurrent.duration.FiniteDuration

/** Asking a STUN server how a NAT maps one of our UDP ports, from a socket of
  * its own bound to that port, for the media port of a call: the mapping is the
  * port's, not the SIP socket's, and a NAT that rewrites ports gives the two
  * different ones. The socket is closed when the answer is in, and the port is
  * free for the media to bind; the NAT keeps the mapping for as long as it
  * keeps those of a UDP flow, which is longer than a call takes to set up.
  */
object StunProbe {

  /** The address `server` saw a Binding request come from `local`. The request
    * is sent, and again after each wait (`retransmit`, doubled each time), up
    * to `attempts` times in all. An answer to another request, or from another
    * server, is not an answer.
    */
  def mapping[F[_]: Async: Network](
      local: SocketAddress[IpAddress],
      server: SocketAddress[IpAddress],
      retransmit: FiniteDuration,
      attempts: Attempts
  ): F[Either[Stun.Error, SocketAddress[IpAddress]]] =
    Network[F].bindDatagramSocket(address = local).use { socket =>
      Stun.newTransactionId[F].flatMap { id =>
        // What comes back from the server that answers this request. Whatever
        // else arrives on the port, such as a late answer to a request of the
        // port's last use, is passed over and the wait goes on.
        val answer = socket.reads
          .collect {
            case Datagram(from, bytes)
                if from == server && Stun.isStun(bytes) =>
              Stun.decode(bytes)
          }
          .collect {
            case Right(response) if response.id == id => response.result
          }
          .head
          .compile
          .lastOrError
        def ask(
            waits: List[FiniteDuration]
        ): F[Either[Stun.Error, SocketAddress[IpAddress]]] =
          waits match {
            case Nil           => Async[F].pure(Left(Stun.Error.NoAnswer))
            case wait :: later =>
              // Built afresh each time it is run: the socket wraps the bytes in
              // a buffer when called, and a buffer can be sent once.
              (Async[F].defer(
                socket.write(Datagram(server, Stun.bindingRequest(id)))
              ) >> answer.map(_.some).timeoutTo(wait, none.pure[F])).flatMap {
                case Some(result) => result.pure[F]
                case None         => ask(later)
              }
          }
        ask(
          Iterator.iterate(retransmit)(_ * 2).take(attempts.value).toList
        )
      }
    }
}
