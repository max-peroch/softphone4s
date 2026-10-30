package softphone4s.transaction

import cats.effect.std.{Queue, Supervisor}
import cats.effect.{Async, Concurrent, Deferred, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import org.typelevel.log4cats.StructuredLogger
import softphone4s.model.*
import softphone4s.runtime.Timers
import softphone4s.transport.{Inbound, Transport}

/** What the owner fiber of the transaction layer reacts to. */
private[transaction] sealed trait Command[F[_]]

private[transaction] object Command {
  final case class Receive[F[_]](inbound: Inbound) extends Command[F]

  final case class StartClient[F[_]](
      request: SipRequest,
      to: SocketAddress[IpAddress],
      reply: Deferred[F, Either[TransactionError, ClientTransaction[F]]]
  ) extends Command[F]

  final case class Respond[F[_]](key: ServerKey, response: SipResponse)
      extends Command[F]

  final case class Fired[F[_]](timer: TimerKey) extends Command[F]

  final case class SendFailed[F[_]](transaction: TransactionId)
      extends Command[F]
}

private[transaction] final case class ClientEntry[F[_]](
    machine: ClientMachine,
    to: SocketAddress[IpAddress],
    notices: Queue[F, Option[ClientNotice]]
)

private[transaction] final case class ServerEntry[F[_]](
    machine: ServerMachine,
    from: SocketAddress[IpAddress],
    notices: Queue[F, Option[ServerNotice]]
)

/** Every running transaction. It is only ever read and replaced by the owner
  * fiber.
  */
private[transaction] final case class Table[F[_]](
    clients: Map[ClientKey, ClientEntry[F]],
    servers: Map[ServerKey, ServerEntry[F]]
)

/** The owner fiber's logic: one [[Command]] in, a new [[Table]] out, with the
  * effects of the state machines' actions in between.
  */
private[transaction] final class TransactionEngine[F[_]: Concurrent](
    transport: Transport[F],
    reliability: Reliability,
    config: TransactionConfig,
    timers: Timers[F, TimerKey],
    commands: Queue[F, Command[F]],
    incomingQueue: Queue[F, ServerTransaction[F]],
    unmatchedQueue: Queue[F, Inbound],
    logger: StructuredLogger[F]
) {

  def step(table: Table[F], command: Command[F]): F[Table[F]] =
    command match {
      case Command.Receive(inbound)                => receive(table, inbound)
      case Command.StartClient(request, to, reply) =>
        startClient(table, request, to, reply)
      case Command.Respond(key, response)     => respond(table, key, response)
      case Command.Fired(TimerKey(id, timer)) =>
        id match {
          case TransactionId.Client(key) =>
            clientEvent(table, key, ClientEvent.TimerFired(timer))
          case TransactionId.Server(key) =>
            serverEvent(table, key, ServerEvent.TimerFired(timer))
        }
      case Command.SendFailed(id) =>
        id match {
          case TransactionId.Client(key) =>
            clientEvent(table, key, ClientEvent.TransportError)
          case TransactionId.Server(key) =>
            serverEvent(table, key, ServerEvent.TransportError)
        }
    }

  // ---- Inbound messages ------------------------------------------------------

  private def receive(table: Table[F], inbound: Inbound): F[Table[F]] =
    inbound.message match {
      case response: SipResponse =>
        ClientKey.forResponse(response) match {
          case Right(key) if table.clients.contains(key) =>
            clientEvent(table, key, ClientEvent.Response(response))
          case _ => unmatchedQueue.offer(inbound).as(table)
        }

      case request: SipRequest =>
        ServerKey.forRequest(request) match {
          case Left(error) =>
            logger.debug(Map("reason" -> error.message))(
              "Request has no usable branch"
            ) >>
              unmatchedQueue.offer(inbound).as(table)
          case Right(key) =>
            (table.servers.contains(key), request.method) match {
              case (true, SipMethod.Ack) =>
                serverEvent(table, key, ServerEvent.AckReceived)
              case (true, _) =>
                serverEvent(table, key, ServerEvent.RequestRetransmitted)
              // The ACK of a 2xx belongs to the dialog.
              case (false, SipMethod.Ack) =>
                unmatchedQueue.offer(inbound).as(table)
              case (false, _) => startServer(table, key, inbound.from, request)
            }
        }
    }

  // ---- Starting transactions ---------------------------------------------------

  private def startClient(
      table: Table[F],
      request: SipRequest,
      to: SocketAddress[IpAddress],
      reply: Deferred[F, Either[TransactionError, ClientTransaction[F]]]
  ): F[Table[F]] =
    if request.method == SipMethod.Ack then
      reply
        .complete(Left(TransactionError.NoTransactionFor(SipMethod.Ack)))
        .as(table)
    else
      ClientKey.forRequest(request) match {
        case Left(error) =>
          reply.complete(Left(TransactionError.InvalidKey(error))).as(table)
        case Right(key) if table.clients.contains(key) =>
          reply.complete(Left(TransactionError.Duplicate(key))).as(table)
        case Right(key) =>
          val (machine, actions) =
            ClientMachine.start(request, reliability, config)
          for {
            notices <- Queue.unbounded[F, Option[ClientNotice]]
            entry = ClientEntry[F](machine, to, notices)
            _ <- actions.traverse_(
              runClient(TransactionId.Client(key), entry, _)
            )
            _ <- reply.complete(
              Right(ClientTransaction(Stream.fromQueueNoneTerminated(notices)))
            )
          } yield table.copy(clients = table.clients.updated(key, entry))
      }

  private def startServer(
      table: Table[F],
      key: ServerKey,
      from: SocketAddress[IpAddress],
      request: SipRequest
  ): F[Table[F]] = {
    val (machine, actions) = ServerMachine.start(request, reliability, config)
    for {
      notices <- Queue.unbounded[F, Option[ServerNotice]]
      entry = ServerEntry[F](machine, from, notices)
      _ <- actions.traverse_(runServer(key, entry, _))
    } yield table.copy(servers = table.servers.updated(key, entry))
  }

  // ---- Driving the machines ------------------------------------------------------

  private def clientEvent(
      table: Table[F],
      key: ClientKey,
      event: ClientEvent
  ): F[Table[F]] =
    table.clients.get(key) match {
      case None        => table.pure[F]
      case Some(entry) =>
        val (next, actions) = entry.machine.step(event)
        val id              = TransactionId.Client(key)
        actions.traverse_(runClient(id, entry, _)) >> {
          if next.isTerminated then
            cancelTimers(id) >> entry.notices
              .offer(None)
              .as(
                table.copy(clients = table.clients - key)
              )
          else
            table
              .copy(clients =
                table.clients.updated(key, entry.copy(machine = next))
              )
              .pure[F]
        }
    }

  private def serverEvent(
      table: Table[F],
      key: ServerKey,
      event: ServerEvent
  ): F[Table[F]] =
    table.servers.get(key) match {
      case None        => table.pure[F]
      case Some(entry) =>
        val (next, actions) = entry.machine.step(event)
        val id              = TransactionId.Server(key)
        actions.traverse_(runServer(key, entry, _)) >> {
          if next.isTerminated then
            cancelTimers(id) >> entry.notices
              .offer(None)
              .as(
                table.copy(servers = table.servers - key)
              )
          else
            table
              .copy(servers =
                table.servers.updated(key, entry.copy(machine = next))
              )
              .pure[F]
        }
    }

  private def respond(
      table: Table[F],
      key: ServerKey,
      response: SipResponse
  ): F[Table[F]] =
    if table.servers.contains(key) then
      serverEvent(table, key, ServerEvent.Respond(response))
    else
      logger
        .debug(Map("status" -> response.statusCode.toString))(
          "Dropping a response to a transaction that has ended"
        )
        .as(table)

  // ---- Interpreting actions ------------------------------------------------------

  private def runClient(
      id: TransactionId,
      entry: ClientEntry[F],
      action: ClientAction
  ): F[Unit] =
    action match {
      case ClientAction.Transmit(request) => send(id, entry.to, request)
      case ClientAction.PassUp(response)  =>
        entry.notices.offer(Some(ClientNotice.Response(response)))
      case ClientAction.Timer(timerAction) =>
        timers.run(timerAction.mapId(TimerKey(id, _)))
      case ClientAction.TimedOut =>
        entry.notices.offer(Some(ClientNotice.TimedOut))
      case ClientAction.TransportFailed =>
        entry.notices.offer(Some(ClientNotice.TransportFailed))
    }

  private def runServer(
      key: ServerKey,
      entry: ServerEntry[F],
      action: ServerAction
  ): F[Unit] = {
    val id = TransactionId.Server(key)
    action match {
      case ServerAction.Transmit(response) => send(id, entry.from, response)
      case ServerAction.PassUp(request)    =>
        incomingQueue.offer(
          ServerTransaction(
            request,
            entry.from,
            response => commands.offer(Command.Respond(key, response)),
            Stream.fromQueueNoneTerminated(entry.notices)
          )
        )
      case ServerAction.Timer(timerAction) =>
        timers.run(timerAction.mapId(TimerKey(id, _)))
      case ServerAction.AckTimedOut =>
        entry.notices.offer(Some(ServerNotice.AckTimedOut))
      case ServerAction.TransportFailed =>
        entry.notices.offer(Some(ServerNotice.TransportFailed))
    }
  }

  /** A failed send is an event for the transaction, not an error here. */
  private def send(
      id: TransactionId,
      to: SocketAddress[IpAddress],
      message: SipMessage
  ): F[Unit] =
    transport.send(to, message).handleErrorWith { error =>
      logger.warn(Map("error" -> String.valueOf(error.getMessage)))(
        "Send failed"
      ) >>
        commands.offer(Command.SendFailed(id))
    }

  private def cancelTimers(id: TransactionId): F[Unit] =
    TransactionTimer.values.toList.traverse_(t =>
      timers.cancel(TimerKey(id, t))
    )
}

object TransactionManager {

  /** Starts the transaction layer over `transport`. `reliability` says whether
    * it retransmits for us. Everything it runs stops when the resource closes.
    */
  def resource[F[_]: Async](
      transport: Transport[F],
      reliability: Reliability,
      config: TransactionConfig,
      logger: StructuredLogger[F]
  ): Resource[F, TransactionLayer[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      timers     <- Timers.resource[F, TimerKey]
      commands   <- Resource.eval(Queue.unbounded[F, Command[F]])
      incomingQ  <- Resource.eval(Queue.unbounded[F, ServerTransaction[F]])
      unmatchedQ <- Resource.eval(Queue.unbounded[F, Inbound])
      engine = new TransactionEngine[F](
        transport,
        reliability,
        config,
        timers,
        commands,
        incomingQ,
        unmatchedQ,
        logger
      )
      _ <- Resource.eval(
        supervisor.supervise(
          transport.messages
            .evalMap(inbound => commands.offer(Command.Receive[F](inbound)))
            .compile
            .drain
        )
      )
      _ <- Resource.eval(
        supervisor.supervise(
          timers.fired
            .evalMap(timer => commands.offer(Command.Fired[F](timer)))
            .compile
            .drain
        )
      )
      _ <- Resource.eval(
        supervisor.supervise(
          Stream
            .fromQueueUnterminated(commands)
            .evalScan(Table[F](Map.empty, Map.empty)) { (table, command) =>
              engine.step(table, command).handleErrorWith { error =>
                logger
                  .error(Map("error" -> String.valueOf(error.getMessage)))(
                    "Transaction layer failed on a command"
                  )
                  .as(table)
              }
            }
            .compile
            .drain
        )
      )
    } yield new TransactionLayer[F] {

      def client(
          request: SipRequest,
          to: SocketAddress[IpAddress]
      ): F[Either[TransactionError, ClientTransaction[F]]] =
        Deferred[F, Either[TransactionError, ClientTransaction[F]]].flatMap {
          reply =>
            commands.offer(
              Command.StartClient[F](request, to, reply)
            ) >> reply.get
        }

      def incoming: Stream[F, ServerTransaction[F]] =
        Stream.fromQueueUnterminated(incomingQ)

      def unmatched: Stream[F, Inbound] =
        Stream.fromQueueUnterminated(unmatchedQ)

      def transmit(message: SipMessage, to: SocketAddress[IpAddress]): F[Unit] =
        transport.send(to, message)
    }
}
