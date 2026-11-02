package softphone4s.transport

import cats.effect.implicits.*
import cats.effect.std.{Queue, Semaphore, Supervisor}
import cats.effect.{Async, Deferred, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Socket
import fs2.{Chunk, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.codec.SipCodec
import softphone4s.model.{SipMessage, SipRequest, SoftphoneError}

import scala.concurrent.duration.*

/** SIP over a stream transport, TCP or TLS (RFC 3261 §18): one connection per
  * destination, made when the first message for it is sent, reused for the
  * messages and the responses that follow, and closed after `idleTimeout`. A
  * message that cannot be written on a connection that was already there is
  * tried once more on a new one. Everything received on a connection is
  * delivered, tagged with the address it came from.
  *
  * It can also accept connections. One that arrives is served like one we
  * opened, and is found by the address it came from, so the responses to its
  * requests go back on it. A request whose Via carries `alias` (RFC 5923) also
  * makes the connection the one to use for requests to that sent-by address,
  * but only when it names the IP address the connection came from. Requests
  * that name a host (over TLS) reuse it only for hosts its peer's certificate
  * vouches for.
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
    *
    * `identities` are the hosts the other end has proved to be, once the
    * connection is established: the names in the certificate it showed, which
    * the TLS layer has checked against what we trust. None for TCP, and for a
    * TLS peer that showed no certificate.
    */
  final case class Connected[F[_]](
      socket: Socket[F],
      established: F[Unit],
      identities: F[List[Host]]
  )

  /** How to open a socket to an address, given the name of the host behind it,
    * if the message being sent names one.
    */
  type Connector[F[_]] =
    (SocketAddress[IpAddress], Option[Host]) => Resource[F, Connected[F]]

  /** A connection someone made to us, and who it came from. */
  final case class Accepted[F[_]](
      connected: Connected[F],
      remote: SocketAddress[IpAddress]
  )

  /** Where we listen, and how to serve what arrives there: `serve` runs a
    * handler on each connection, up to the given number at a time, for as long
    * as the connection lasts. It is a function, not a stream of connections,
    * because a socket that was accepted is closed when its handler is done.
    */
  final case class Listener[F[_]](
      address: SocketAddress[IpAddress],
      serve: (Int, Accepted[F] => F[Unit]) => F[Unit]
  )

  private val Ping: Chunk[Byte] =
    Chunk.array(
      "\r\n\r\n".getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
    )

  private val Pong: Chunk[Byte] =
    Chunk.array("\r\n".getBytes(java.nio.charset.StandardCharsets.ISO_8859_1))

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

  private def newSlot[F[_]: Async]: F[Slot[F]] =
    (
      Deferred[F, Either[Throwable, Connection[F]]],
      Deferred[F, Unit]
    ).mapN(Slot(_, _))

  /** The transport proper: the pool of connections and what is done with them.
    */
  private final class Connections[F[_]: Async](
      bindAddress: IpAddress,
      config: StreamConfig,
      connect: Connector[F],
      serverNameOf: SipMessage => Option[Host],
      listen: Option[Listener[F]],
      supervisor: Supervisor[F],
      inbox: Queue[F, Inbound],
      pool: Ref[F, Map[Key, Slot[F]]],
      logger: StructuredLogger[F]
  ) extends Transport[F] {

    def localAddress: SocketAddress[IpAddress] =
      listen.fold(SocketAddress(bindAddress, port"0"))(_.address)

    def messages: Stream[F, Inbound] = Stream.fromQueueUnterminated(inbox)

    def send(to: SocketAddress[IpAddress], message: SipMessage): F[Unit] = {
      val bytes = SipCodec.encode(message)
      val name  = serverNameOf(message)
      write(to, name, bytes).handleErrorWith { error =>
        logger.debug(
          Map(
            "to"            -> to.toString,
            LogFields.Error -> String.valueOf(error.getMessage)
          )
        )("Send failed; trying a new connection") >>
          evict(to, name) >> write(to, name, bytes)
      }
    }

    private def write(
        to: SocketAddress[IpAddress],
        name: Option[Host],
        bytes: Chunk[Byte]
    ): F[Unit] =
      connection(to, name).flatMap(_.write(bytes))

    /** Drops the connection that serves `to`, so the next send makes a new one.
      */
    private def evict(
        to: SocketAddress[IpAddress],
        name: Option[Host]
    ): F[Unit] =
      pool
        .modify { m =>
          val slot = lookup(m, Key(to, name))
          (m.filterNot { case (_, s) => slot.contains(s) }, slot)
        }
        .flatMap(_.traverse_(_.stop.complete(()).void))

    /** The connection for `wanted` among `m`: exactly that, else, for a message
      * that names no host, any to its address. A message that names a host
      * needs a connection checked for it.
      */
    private def lookup(m: Map[Key, Slot[F]], wanted: Key): Option[Slot[F]] =
      m.get(wanted).orElse {
        if wanted.name.isEmpty then
          m.collectFirst {
            case (k, slot) if k.address == wanted.address => slot
          }
        else None
      }

    /** The connection to `to`, made if there is none. */
    private def connection(
        to: SocketAddress[IpAddress],
        name: Option[Host]
    ): F[Connection[F]] =
      for {
        fresh         <- newSlot[F]
        (slot, isNew) <- pool.modify { m =>
          lookup(m, Key(to, name)) match {
            case Some(existing) => (m, (existing, false))
            case None => (m.updated(Key(to, name), fresh), (fresh, true))
          }
        }
        _ <- supervisor.supervise(run(Key(to, name), slot)).whenA(isNew)
        connection <- slot.ready.get
          .timeoutTo(
            config.connectTimeout,
            evict(to, name).as(
              Left(SoftphoneError.ConnectTimedOut(to.toString))
            )
          )
          .rethrow
      } yield connection

    /** Makes the connection `key` and serves it until it ends. */
    private def run(key: Key, slot: Slot[F]): F[Unit] =
      connect(key.address, key.name)
        .use(serve(key.address, slot, _))
        .handleErrorWith { error =>
          slot.ready.complete(Left(error)).void >>
            logger.debug(
              Map(
                "to"            -> key.address.toString,
                LogFields.Error -> String.valueOf(error.getMessage)
              )
            )("Connection ended")
        }
        .guarantee(forget(slot))

    /** Serves a connection someone made to us. */
    def accept(accepted: Accepted[F]): F[Unit] =
      newSlot[F].flatMap { slot =>
        (pool.update(_.updated(Key(accepted.remote, None), slot)) >>
          serve(accepted.remote, slot, accepted.connected))
          .handleErrorWith { error =>
            slot.ready.complete(Left(error)).void >>
              logger.debug(
                Map(LogFields.Error -> String.valueOf(error.getMessage))
              )("An incoming connection ended")
          }
          .guarantee(forget(slot))
      }

    /** Takes `slot` out of the pool, under every address it was found by. */
    private def forget(slot: Slot[F]): F[Unit] =
      pool.update(_.filterNot { case (_, s) => s == slot })

    /** Reads the connection, and says it is ready once it is established. */
    private def serve(
        remote: SocketAddress[IpAddress],
        slot: Slot[F],
        connected: Connected[F]
    ): F[Unit] =
      for {
        (lock, touched, received) <- (
          Semaphore[F](1),
          Ref.of[F, FiniteDuration](Duration.Zero),
          Ref.of[F, FiniteDuration](Duration.Zero)
        ).tupled
        touch = Async[F].monotonic.flatMap(touched.set)
        _ <- touch
        // Keep-alive traffic is written without counting as activity: a ping we
        // send must not keep a connection that nobody answers alive.
        plainWrite = (bytes: Chunk[Byte]) =>
          lock.permit.surround(connected.socket.write(bytes))
        connection = new Connection[F](bytes => plainWrite(bytes) <* touch)
        // Ready once established; a failure while getting there ends the
        // reading below, and is reported through `slot` by the handler.
        ready = connected.established >>
          slot.ready.complete(Right(connection)).void
        read = connected.socket.reads.chunks
          .evalTap(_ =>
            Async[F].monotonic.flatMap(now =>
              touched.set(now) >> received.set(now)
            )
          )
          .unchunks
          .through(StreamFraming.events[F](config.framing))
          .evalMap {
            // A ping is answered with a pong (RFC 5626 §4.4.1).
            case Right(Frame.Ping)           => plainWrite(Pong).as(true)
            case Right(Frame.Message(bytes)) =>
              deliver(remote, slot, connected.identities, Right(bytes))
            case Left(error) =>
              deliver(remote, slot, connected.identities, Left(error))
          }
          .takeWhile(identity)
          .compile
          .drain
        _ <- (
          read
            .race(slot.stop.get)
            .race(idle(touched))
            .race(keepAlive(plainWrite, touched, received)),
          ready
        ).parTupled
      } yield ()

    /** Pings the connection whenever it has been quiet for the interval, and
      * completes, which ends the connection, if nothing at all comes back
      * within the timeout. Never completes when keep-alives are not configured.
      */
    private def keepAlive(
        write: Chunk[Byte] => F[Unit],
        touched: Ref[F, FiniteDuration],
        received: Ref[F, FiniteDuration]
    ): F[Unit] =
      config.keepAlive.fold(Async[F].never[Unit]) { ka =>
        def loop: F[Unit] =
          Async[F].sleep(ka.interval) >>
            (Async[F].monotonic, touched.get).mapN(_ - _).flatMap { quiet =>
              if quiet < ka.interval then loop
              else
                for {
                  sentAt <- Async[F].monotonic
                  _      <- write(Ping)
                  _      <- Async[F].sleep(ka.timeout)
                  heard  <- received.get
                  _      <-
                    if heard >= sentAt then loop
                    else
                      logger.debug(Map("after" -> ka.timeout.toString))(
                        "No answer to a keep-alive; closing the connection"
                      )
                } yield ()
            }
        loop
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
        slot: Slot[F],
        identities: F[List[Host]],
        frame: Either[FramingError, Chunk[Byte]]
    ): F[Boolean] =
      frame match {
        case Left(error) =>
          logger
            .warn(
              Map("from" -> from.toString, LogFields.Error -> error.message)
            )(
              "Closing a connection that cannot be framed"
            )
            .as(false)
        case Right(bytes) =>
          SipCodec.decode(bytes) match {
            case Right(message) =>
              aliasOf(from, message).traverse_(address =>
                identities.flatMap(names =>
                  pool.update(m =>
                    (None :: names.map(Some(_))).foldLeft(m)((acc, name) =>
                      if acc.contains(Key(address, name)) then acc
                      else acc.updated(Key(address, name), slot)
                    )
                  )
                )
              ) >> inbox.offer(Inbound(from, message)).as(true)
            case Left(error) =>
              logger
                .warn(
                  Map("from" -> from.toString, LogFields.Error -> error.message)
                )("Failed to decode SIP message")
                .as(true)
          }
      }
  }

  def resource[F[_]: Async](
      bindAddress: IpAddress,
      config: StreamConfig,
      connect: Connector[F],
      serverNameOf: SipMessage => Option[Host],
      listen: Option[Listener[F]],
      logger: StructuredLogger[F]
  ): Resource[F, Transport[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      inbox      <- Resource.eval(Queue.unbounded[F, Inbound])
      pool       <- Resource.eval(Ref.of[F, Map[Key, Slot[F]]](Map.empty))
      connections = new Connections[F](
        bindAddress,
        config,
        connect,
        serverNameOf,
        listen,
        supervisor,
        inbox,
        pool,
        logger
      )
      _ <- listen.traverse_ { listener =>
        Resource.eval(
          supervisor.supervise(
            listener.serve(config.maxIncoming.value, connections.accept)
          )
        )
      }
    } yield connections: Transport[F]

  /** Where a request says its connection can be reached (RFC 5923): the sent-by
    * of a Via with `alias`, if that is on the IP address the request came from.
    * Anything else could point our traffic for someone else at this connection.
    */
  private def aliasOf(
      from: SocketAddress[IpAddress],
      message: SipMessage
  ): Option[SocketAddress[IpAddress]] =
    message match {
      case request: SipRequest if request.headers.topVia.hasAlias =>
        request.headers.topVia.sentBy.filter(_.host == from.host)
      case _ => None
    }
}
