package softphone4s.transport

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.{Datagram, Network}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.codec.{ParseError, SipCodec}
import softphone4s.model.SipMessage

object SipTransport {

  /** A UDP socket bound to `bindAddress`; released with the resource. */
  def udp[F[_]: Async: Network](
      bindAddress: SocketAddress[IpAddress],
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F]] =
    Network[F]
      .bindDatagramSocket(address = bindAddress)
      .evalTap(socket =>
        logger.debug(Map("address" -> socket.address.toString))(
          "SIP transport bound"
        )
      )
      .map { socket =>
        new Transport[F] {
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

          def send(to: SocketAddress[IpAddress], message: SipMessage): F[Unit] =
            socket.write(Datagram(to, SipCodec.encode(message)))
        }
      }
}
