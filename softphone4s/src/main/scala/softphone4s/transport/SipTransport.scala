package softphone4s.transport

import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.{Chunk, Stream}
import scala.concurrent.duration.FiniteDuration
import fs2.io.net.{Datagram, Network}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.codec.{ParseError, SipCodec}
import softphone4s.model.SipMessage
import softphone4s.stun.{Stun, StunClient}

object SipTransport {

  /** A UDP socket bound to `bindAddress`; released with the resource. It also
    * speaks STUN on the same socket, as long as `messages` is being read.
    */
  def udp[F[_]: Async: Network](
      bindAddress: SocketAddress[IpAddress],
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F] & StunClient[F]] =
    Resource
      .eval(Ref.of[F, Map[Stun.TransactionId, Pending[F]]](Map.empty))
      .flatMap { pending =>
        Network[F]
          .bindDatagramSocket(address = bindAddress)
          .evalTap(socket =>
            logger.debug(Map(LogFields.Address -> socket.address.toString))(
              "SIP transport bound"
            )
          )
          .map { socket =>
            new Transport[F] with StunClient[F] {
              def localAddress: SocketAddress[IpAddress] =
                socket.address match {
                  case a: SocketAddress[?] =>
                    a.host match {
                      case ip: IpAddress => SocketAddress(ip, a.port)
                      case _             => bindAddress
                    }
                  case _ => bindAddress
                }

              def messages: Stream[F, Inbound] =
                socket.reads.evalMapFilter { datagram =>
                  if Stun.isStun(datagram.bytes) then
                    answer(pending, datagram.remote, datagram.bytes).as(None)
                  else
                    SipCodec.decode(datagram.bytes) match {
                      case Right(msg) =>
                        Async[F].pure(Some(Inbound(datagram.remote, msg)))
                      // A bare CRLF is a keep-alive (RFC 5626), not an error.
                      case Left(ParseError.Empty) => Async[F].pure(None)
                      case Left(err)              =>
                        logger
                          .warn(
                            Map(
                              LogFields.Error -> err.message,
                              "from"          -> datagram.remote.toString
                            )
                          )(
                            "Failed to decode SIP message"
                          )
                          .as(None)
                    }
                }

              def send(to: SocketAddress[IpAddress], message: SipMessage)
                  : F[Unit] =
                socket.write(Datagram(to, SipCodec.encode(message)))

              def bindingRequest(
                  server: SocketAddress[IpAddress],
                  retransmit: FiniteDuration,
                  attempts: Int
              ): F[Outcome] =
                Stun.newTransactionId[F].flatMap { id =>
                  // Waiting for the answer is registered for as long as it takes,
                  // and not a moment longer, however it ends.
                  Resource
                    .make(
                      Deferred[F, Outcome].flatTap(answer =>
                        pending.update(_.updated(id, Pending(server, answer)))
                      )
                    )(_ => pending.update(_ - id))
                    .use(answer =>
                      asking(
                        // Built afresh each time it is run: the socket wraps the bytes
                        // in a buffer when called, and a buffer can be sent once.
                        Async[F].defer(
                          socket.write(
                            Datagram(server, Stun.bindingRequest(id))
                          )
                        ),
                        answer,
                        Iterator
                          .iterate(retransmit)(_ * 2)
                          .take(math.max(attempts, 1))
                          .toList
                      )
                    )
                }
            }
          }
      }

  /** What a STUN server said: the address it saw us at, or why not. */
  private type Outcome = Either[Stun.Error, SocketAddress[IpAddress]]

  /** Sends the request, and again after each wait, until it is answered or the
    * waits are used up.
    */
  private def asking[F[_]: Async](
      send: F[Unit],
      answer: Deferred[F, Outcome],
      waits: List[FiniteDuration]
  ): F[Outcome] =
    waits match {
      case Nil           => Async[F].pure(Left(Stun.Error.NoAnswer))
      case wait :: later =>
        (send >> answer.get.map(_.some).timeoutTo(wait, none[Outcome].pure[F]))
          .flatMap {
            case Some(result) => result.pure[F]
            case None         => asking(send, answer, later)
          }
    }

  /** A request waiting for its answer, and whom it was asked of. */
  private final case class Pending[F[_]](
      server: SocketAddress[IpAddress],
      answer: Deferred[F, Outcome]
  )

  /** Hands a STUN answer to the request it belongs to, if it came from whom the
    * request went to.
    */
  private def answer[F[_]: Async](
      pending: Ref[F, Map[Stun.TransactionId, Pending[F]]],
      from: SocketAddress[IpAddress],
      bytes: Chunk[Byte]
  ): F[Unit] =
    Stun.decode(bytes) match {
      case Right(response) =>
        pending.get.flatMap(
          _.get(response.id)
            .filter(_.server == from)
            .traverse_(_.answer.complete(response.result).void)
        )
      case Left(_) => Async[F].unit
    }
}
