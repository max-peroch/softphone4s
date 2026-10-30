package softphone4s.transport

import cats.effect.std.{Queue, Semaphore, Supervisor}
import cats.effect.implicits.*
import cats.effect.{Async, Deferred, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Socket
import fs2.{Chunk, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.codec.SipCodec
import softphone4s.model.SipMessage

import scala.concurrent.duration.*

/** SIP over a stream transport, TCP or TLS, for a client (RFC 3261 §18): one
  * connection per destination, made when the first message for it is sent,
  * reused for the messages and the responses that follow, and closed after
  * `idleTimeout`. A message that cannot be written on a connection that was
  * already there is tried once more on a new one. Everything received on a
  * connection is delivered, tagged with the destination it was opened to.
  *
  * What differs between TCP and TLS is how a socket is made (`connect`) and
  * what a connection must be told about the host it goes to (`serverNameOf`,
  * which for TLS is the name to check the certificate against).
  */
object StreamTransport {

  /** A socket, and how to wait until the connection on it is usable. For TCP
    * that is at once; for TLS it is when the handshake has finished. The wait
    * runs while the socket is being read, which is what drives a handshake
    * along.
    */
  final case class Connected[F[_]](socket: Socket[F], established: F[Unit])

  /** How to open a socket to an address, given the name of the host behind it,
    * if the message being sent names one.
    */
  type Connector[F[_]] =
    (SocketAddress[IpAddress], Option[Host]) => Resource[F, Connected[F]]

  /** An open connection, as the senders see it. */
  private final class Connection[F[_]](val write: Chunk[Byte] => F[Unit])

  /** What identifies a connection: where it goes and the name it was checked
    * against.
    */
  private final case class Key(
      address: SocketAddress[IpAddress],
      name: Option[Host]
  )

  /** A connection being made, or made: `ready` says which, `stop` ends it. */
  private final case class Slot[F[_]](
      ready: Deferred[F, Either[Throwable, Connection[F]]],
      stop: Deferred[F, Unit]
  )

  def resource[F[_]: Async](
      bindAddress: IpAddress,
      config: StreamConfig,
      connect: Connector[F],
      serverNameOf: SipMessage => Option[Host],
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      inbox      <- Resource.eval(Queue.unbounded[F, Inbound])
      pool       <- Resource.eval(Ref.of[F, Map[Key, Slot[F]]](Map.empty))
    } yield new Transport[F] {

      def localAddress: SocketAddress[IpAddress] =
        SocketAddress(bindAddress, port"0")

      def messages: Stream[F, Inbound] = Stream.fromQueueUnterminated(inbox)

      def send(to: SocketAddress[IpAddress], message: SipMessage): F[Unit] = {
        val bytes = SipCodec.encode(message)
        val name  = serverNameOf(message)
        write(to, name, bytes).handleErrorWith { error =>
          logger.debug(
            Map(
              "to"    -> to.toString,
              "error" -> String.valueOf(error.getMessage)
            )
          )("Send failed; trying a new connection") >>
            evict(Key(to, name)) >> write(to, name, bytes)
        }
      }

      private def write(
          to: SocketAddress[IpAddress],
          name: Option[Host],
          bytes: Chunk[Byte]
      ): F[Unit] =
        connection(to, name).flatMap(_.write(bytes))

      /** Drops the connection `key`, so the next send makes a new one. */
      private def evict(key: Key): F[Unit] =
        pool
          .modify(m => (m - key, m.get(key)))
          .flatMap(_.traverse_(_.stop.complete(()).void))

      /** The connection to `to`, made if there is none. A message that names no
        * host (a response) goes on whichever connection to `to` is there.
        */
      private def connection(
          to: SocketAddress[IpAddress],
          name: Option[Host]
      ): F[Connection[F]] =
        for {
          fresh <- (
            Deferred[F, Either[Throwable, Connection[F]]],
            Deferred[F, Unit]
          ).mapN(Slot(_, _))
          (key, slot, isNew) <- pool.modify { m =>
            val wanted   = Key(to, name)
            val existing = m.get(wanted).map(wanted -> _).orElse {
              if name.isEmpty then m.find { case (k, _) => k.address == to }
              else None
            }
            existing match {
              case Some((k, s)) => (m, (k, s, false))
              case None => (m.updated(wanted, fresh), (wanted, fresh, true))
            }
          }
          _          <- supervisor.supervise(run(key, slot)).whenA(isNew)
          connection <- slot.ready.get
            .timeoutTo(
              config.connectTimeout,
              evict(key).as(
                Left(
                  new java.util.concurrent.TimeoutException(
                    s"connecting to $to"
                  )
                )
              )
            )
            .rethrow
        } yield connection

      /** Makes the connection `key` and serves it until it ends. */
      private def run(key: Key, slot: Slot[F]): F[Unit] = {
        val to = key.address
        connect(to, key.name)
          .use { case Connected(socket, established) =>
            for {
              lock    <- Semaphore[F](1)
              touched <- Ref.of[F, FiniteDuration](Duration.Zero)
              touch = Async[F].monotonic.flatMap(touched.set)
              _ <- touch
              connection = new Connection[F](bytes =>
                lock.permit.surround(socket.write(bytes)) <* touch
              )
              // Ready once established; a failure while getting there ends the
              // reading below, and is reported through `slot` by the handler.
              ready = established >> slot.ready.complete(Right(connection)).void
              read  = socket.reads.chunks
                .evalTap(_ => touch)
                .unchunks
                .through(StreamFraming.frames[F](config.framing))
                .evalMap(deliver(to, _))
                .takeWhile(identity)
                .compile
                .drain
              _ <- (
                read.race(slot.stop.get).race(idle(touched)),
                ready
              ).parTupled
            } yield ()
          }
          .handleErrorWith { error =>
            slot.ready.complete(Left(error)).void >>
              logger.debug(
                Map(
                  "to"    -> to.toString,
                  "error" -> String.valueOf(error.getMessage)
                )
              )("Connection ended")
          }
          .guarantee(
            pool.update(m => if m.get(key).contains(slot) then m - key else m)
          )
      }

      /** Completes when nothing has crossed the connection for `idleTimeout`.
        */
      private def idle(touched: Ref[F, FiniteDuration]): F[Unit] = {
        val check         = (config.idleTimeout / 2).max(1.millis)
        def loop: F[Unit] =
          (Async[F].sleep(check) >>
            (Async[F].monotonic, touched.get).mapN(_ - _))
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
                "Closing a connection that cannot be framed"
              )
              .as(false)
          case Right(bytes) =>
            SipCodec.decode(bytes) match {
              case Right(message) =>
                inbox.offer(Inbound(from, message)).as(true)
              case Left(error) =>
                logger
                  .warn(
                    Map("from" -> from.toString, "error" -> error.message)
                  )("Failed to decode SIP message")
                  .as(true)
            }
        }
    }
}
