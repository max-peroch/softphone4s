package softphone4s.transport

import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.{Chunk, Stream}
import scala.concurrent.duration.FiniteDuration
import fs2.io.net.{Datagram, Network}
import org.typelevel.log4cats.StructuredLogger
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
            logger.debug(Map("address" -> socket.address.toString))(
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
                              "error" -> err.message,
                              "from"  -> datagram.remote.toString
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
              ): F[Either[Stun.Error, SocketAddress[IpAddress]]] =
                for {
                  random <- Async[F].delay(new Array[Byte](12))
                  _      <- Async[F].delay(secure.nextBytes(random))
                  id = Stun.TransactionId(Chunk.array(random))
                  answer <- Deferred[F, Either[Stun.Error, SocketAddress[
                    IpAddress
                  ]]]
                  _ <- pending.update(_.updated(id, Pending(server, answer)))
                  request = Stun.bindingRequest(id)
                  waits   = Iterator
                    .iterate(retransmit)(_ * 2)
                    .take(math.max(attempts, 1))
                    .toList
                  result <- waits
                    .foldLeft(
                      Async[F].pure(
                        Option
                          .empty[Either[Stun.Error, SocketAddress[IpAddress]]]
                      )
                    ) { (previous, wait) =>
                      previous.flatMap {
                        case done @ Some(_) => Async[F].pure(done)
                        case None           =>
                          socket.write(Datagram(server, request)) >>
                            answer.get
                              .timeoutTo(wait, Async[F].pure(null))
                              .map(Option(_))
                      }
                    }
                    .guarantee(pending.update(_ - id))
                } yield result.getOrElse(
                  Left(Stun.Error.Malformed("no answer"))
                )
            }
          }
      }

  private val secure = new java.security.SecureRandom

  /** A request waiting for its answer, and whom it was asked of. */
  private final case class Pending[F[_]](
      server: SocketAddress[IpAddress],
      answer: Deferred[F, Either[Stun.Error, SocketAddress[IpAddress]]]
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
