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

  def apply[F[_]: Async: Network](
      localPort: Port,
      bindAddress: IpAddress,
      logger: StructuredLogger[F]
  ): Resource[
    F,
    (Stream[F, SipMessage], (SocketAddress[IpAddress], SipMessage) => F[Unit])
  ] = {
    Network[F]
      .bindDatagramSocket(address = SocketAddress(bindAddress, localPort))
      .evalTap(socket =>
        logger.debug(Map("address" -> socket.address.toString))(
          "SIP transport bound"
        )
      )
      .map { socket =>
        val receive: Stream[F, SipMessage] =
          socket.reads
            .evalMap { datagram =>
              SipCodec.decode(datagram.bytes) match {
                case Right(msg) => Async[F].pure(Some(msg))
                // A bare CRLF is a keep-alive (RFC 5626), not an error.
                case Left(ParseError.Empty) => Async[F].pure(None)
                case Left(err)              =>
                  logger
                    .warn(Map("error" -> err.message))(
                      "Failed to decode SIP message"
                    )
                    .as(None)
              }
            }
            .collect { case Some(msg) => msg }

        val send: (SocketAddress[IpAddress], SipMessage) => F[Unit] = {
          (addr, msg) =>
            val bytes = SipCodec.encode(msg)
            socket.write(Datagram(addr, bytes))
        }

        (receive, send)
      }
  }
}
