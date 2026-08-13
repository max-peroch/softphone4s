package softphone4s.transport

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.{Datagram, Network}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.codec.SipCodec
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
              val raw = new String(datagram.bytes.toArray, "UTF-8")
              SipCodec.decode(raw) match {
                case Right(msg) => Async[F].pure(Some(msg))
                case Left(err)  =>
                  logger
                    .warn(Map("error" -> err))("Failed to decode SIP message")
                    .as(None)
              }
            }
            .collect { case Some(msg) => msg }

        val send: (SocketAddress[IpAddress], SipMessage) => F[Unit] = {
          (addr, msg) =>
            val bytes = fs2.Chunk.array(SipCodec.encode(msg).getBytes("UTF-8"))
            socket.write(Datagram(addr, bytes))
        }

        (receive, send)
      }
  }
}
