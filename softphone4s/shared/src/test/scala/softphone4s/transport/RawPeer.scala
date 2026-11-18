package softphone4s.transport

import cats.effect.std.Queue
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.Socket
import softphone4s.codec.SipCodec
import softphone4s.model.SipMessage

import scala.concurrent.duration.*

/** The far end of a stream connection in a test: it sends messages down a
  * socket and queues whatever comes back.
  */
final class RawPeer private (
    socket: Socket[IO],
    inbox: Queue[IO, SipMessage],
    val local: SocketAddress[IpAddress]
) {

  def send(message: SipMessage): IO[Unit] =
    socket.write(SipCodec.encode(message))

  /** Bytes as they are, for what is not a message. */
  def sendRaw(text: String): IO[Unit] =
    socket.write(
      fs2.Chunk.array(
        text.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
      )
    )

  /** The next message that arrives. */
  def next: IO[SipMessage] = inbox.take.timeout(5.seconds)

  /** Whether anything arrives within `wait`. */
  def quiet(wait: FiniteDuration): IO[Boolean] =
    inbox.take.timeout(wait).attempt.map(_.isLeft)
}

object RawPeer {

  def apply(
      socket: Socket[IO]
  ): Resource[IO, RawPeer] =
    for {
      inbox <- Resource.eval(Queue.unbounded[IO, SipMessage])
      local <- Resource.eval(addressOf(socket.address))
      _     <- socket.reads
        .through(StreamFraming.frames[IO]())
        .evalMap {
          case Right(bytes) =>
            SipCodec.decode(bytes).toOption.traverse_(inbox.offer)
          case Left(_) => IO.unit
        }
        .handleErrorWith(_ => Stream.empty)
        .compile
        .drain
        .background
    } yield new RawPeer(socket, inbox, local)

  def addressOf(address: GenSocketAddress): IO[SocketAddress[IpAddress]] =
    address match {
      case a: SocketAddress[?] =>
        a.host match {
          case ip: IpAddress => IO.pure(SocketAddress(ip, a.port))
          case _ => IO.raiseError(new RuntimeException("not an IP address"))
        }
      case _ => IO.raiseError(new RuntimeException("not an IP address"))
    }
}
