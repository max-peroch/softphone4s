package softphone4s.transport

import cats.effect.std.{Queue, Semaphore, Supervisor}
import cats.effect.syntax.all.*
import cats.effect.{Async, Deferred, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import fs2.{Chunk, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.codec.SipCodec
import softphone4s.model.SipMessage

import scala.concurrent.duration.*

/** SIP over TCP for a client (RFC 3261 §18): one connection per destination,
  * made when the first message for it is sent, reused for the messages and the
  * responses that follow, and closed after `idleTimeout`. A message that cannot
  * be written on a connection that was already there is tried once more on a
  * new one. Everything received on a connection is delivered, tagged with the
  * destination it was opened to.
  */
object TcpTransport {

  /** An open connection, as the senders see it. */
  private final class Connection[F[_]](val write: Chunk[Byte] => F[Unit])

  /** A connection being made, or made: `ready` says which, `stop` ends it. */
  private final case class Slot[F[_]](
      ready: Deferred[F, Either[Throwable, Connection[F]]],
      stop: Deferred[F, Unit]
  )

  def resource[F[_]: Async: Network](
      bindAddress: IpAddress,
      config: TcpConfig,
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      inbox      <- Resource.eval(Queue.unbounded[F, Inbound])
      pool       <- Resource.eval(
        Ref.of[F, Map[SocketAddress[IpAddress], Slot[F]]](Map.empty)
      )
    } yield new Transport[F] {

      def localAddress: SocketAddress[IpAddress] =
        SocketAddress(bindAddress, port"0")

      def messages: Stream[F, Inbound] = Stream.fromQueueUnterminated(inbox)

      def send(to: SocketAddress[IpAddress], message: SipMessage): F[Unit] = {
        val bytes = SipCodec.encode(message)
        write(to, bytes).handleErrorWith { error =>
          logger.debug(
            Map(
              "to"    -> to.toString,
              "error" -> String.valueOf(error.getMessage)
            )
          )("Send failed; trying a new connection") >>
            evict(to) >> write(to, bytes)
        }
      }

      private def write(
          to: SocketAddress[IpAddress],
          bytes: Chunk[Byte]
      ): F[Unit] =
        connection(to).flatMap(_.write(bytes))

      /** Drops the connection to `to`, so the next send makes a new one. */
      private def evict(to: SocketAddress[IpAddress]): F[Unit] =
        pool
          .modify(m => (m - to, m.get(to)))
          .flatMap(_.traverse_(_.stop.complete(()).void))

      /** The connection to `to`, made if there is none. */
      private def connection(to: SocketAddress[IpAddress]): F[Connection[F]] =
        for {
          fresh <- (
            Deferred[F, Either[Throwable, Connection[F]]],
            Deferred[F, Unit]
          ).mapN(Slot(_, _))
          (slot, isNew) <- pool.modify { m =>
            m.get(to) match {
              case Some(existing) => (m, (existing, false))
              case None           => (m.updated(to, fresh), (fresh, true))
            }
          }
          _          <- supervisor.supervise(run(to, slot)).whenA(isNew)
          connection <- slot.ready.get
            .timeoutTo(
              config.connectTimeout,
              evict(to).as(
                Left(
                  new java.util.concurrent.TimeoutException(
                    s"connecting to $to"
                  )
                )
              )
            )
            .rethrow
        } yield connection

      /** Makes the connection to `to` and serves it until it ends. */
      private def run(to: SocketAddress[IpAddress], slot: Slot[F]): F[Unit] =
        Network[F]
          .connect(to)
          .use { socket =>
            for {
              lock    <- Semaphore[F](1)
              touched <- Ref.of[F, FiniteDuration](Duration.Zero)
              touch = Async[F].monotonic.flatMap(touched.set)
              _ <- touch
              connection = new Connection[F](bytes =>
                lock.permit.surround(socket.write(bytes)) <* touch
              )
              _ <- slot.ready.complete(Right(connection))
              read = socket.reads.chunks
                .evalTap(_ => touch)
                .unchunks
                .through(StreamFraming.frames[F](config.framing))
                .evalMap(deliver(to, _))
                .takeWhile(identity)
                .compile
                .drain
              _ <- read.race(slot.stop.get).race(idle(touched))
            } yield ()
          }
          .handleErrorWith { error =>
            slot.ready.complete(Left(error)).void >>
              logger.debug(
                Map(
                  "to"    -> to.toString,
                  "error" -> String.valueOf(error.getMessage)
                )
              )("TCP connection ended")
          }
          .guarantee(
            pool.update(m => if m.get(to).contains(slot) then m - to else m)
          )

      /** Completes when nothing has crossed the connection for `idleTimeout`.
        */
      private def idle(touched: Ref[F, FiniteDuration]): F[Unit] = {
        val check         = (config.idleTimeout / 2).max(1.millis)
        def loop: F[Unit] =
          (Async[F]
            .sleep(check) >> (Async[F].monotonic, touched.get).mapN(_ - _))
            .flatMap(quiet =>
              if quiet >= config.idleTimeout then Async[F].unit else loop
            )
        loop
      }

      /** Passes a frame up. False when the stream is not worth reading on. */
      private def deliver(
          from: SocketAddress[IpAddress],
          frame: Either[FramingError, Chunk[Byte]]
      ): F[Boolean] =
        frame match {
          case Left(error) =>
            logger
              .warn(Map("from" -> from.toString, "error" -> error.message))(
                "Closing a TCP connection that cannot be framed"
              )
              .as(false)
          case Right(bytes) =>
            SipCodec.decode(bytes) match {
              case Right(message) =>
                inbox.offer(Inbound(from, message)).as(true)
              case Left(error) =>
                logger
                  .warn(Map("from" -> from.toString, "error" -> error.message))(
                    "Failed to decode SIP message"
                  )
                  .as(true)
            }
        }
    }
}
