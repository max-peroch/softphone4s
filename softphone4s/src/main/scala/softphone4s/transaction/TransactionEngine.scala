package softphone4s.transaction

import cats.effect.{Concurrent, Deferred}
import cats.effect.std.Queue
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.model.*
import softphone4s.runtime.Timers
import softphone4s.transport.{Inbound, Transport}

/** The owner fiber's logic: one [[Command]] in, a new [[Table]] out, with the
  * effects of the state machines' actions in between.
  */
private[transaction] final class TransactionEngine[F[_]: Concurrent](
    transport: Transport[F],
    config: TransactionConfig,
    timers: Timers[F, TimerKey],
    commands: Queue[F, Command[F]],
    incomingQueue: Queue[F, ServerTransaction[F]],
    unmatchedQueue: Queue[F, Inbound],
    observed: ObservedAddress => F[Unit],
    events: TransactionEvent => F[Unit],
    tracing: TransactionTracing[F],
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

  /** The transport a request travels on is named by its top Via. */
  private def reliabilityOf(request: SipRequest): Reliability =
    Reliability.of(request.headers.topVia.transport)

  // ---- Inbound messages ------------------------------------------------------

  private def receive(table: Table[F], inbound: Inbound): F[Table[F]] =
    inbound.message match {
      case response: SipResponse =>
        ClientKey.forResponse(response) match {
          case Right(key) if table.clients.contains(key) =>
            observe(table.clients(key).to, inbound, response) >>
              clientEvent(table, key, ClientEvent.Response(response))
          case _ => unmatchedQueue.offer(inbound).as(table)
        }

      case request: SipRequest =>
        ServerKey.forRequest(request) match {
          case Left(error) =>
            logger.debug(
              Map(
                LogFields.Reason -> error.message,
                LogFields.Method -> request.method.token,
                LogFields.CallId -> request.headers.callId.value
              )
            )(
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

  /** What the response says about how we look from outside. Believed only from
    * the server we sent the request to, and only for UDP: over a stream the
    * port is that of a connection, not of anything a peer can send to.
    */
  private def observe(
      sentTo: SocketAddress[IpAddress],
      inbound: Inbound,
      response: SipResponse
  ): F[Unit] = {
    val via = response.headers.topVia
    if inbound.from == sentTo && via.transport == TransportProtocol.Udp then
      via.observedAddress.traverse_(address =>
        observed(ObservedAddress(via.transport, address))
      )
    else Concurrent[F].unit
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
            ClientMachine.start(request, reliabilityOf(request), config)
          for {
            notices <- Queue.unbounded[F, Option[ClientNotice]]
            span    <- tracing.began(
              TransactionInfo(
                TransactionSide.Client,
                key.method,
                key.branch,
                request.headers.callId
              )
            )
            entry = ClientEntry[F](machine, to, notices, Some(span))
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
    val (machine, actions) =
      ServerMachine.start(request, reliabilityOf(request), config)
    for {
      notices <- Queue.unbounded[F, Option[ServerNotice]]
      span    <- tracing.began(
        TransactionInfo(
          TransactionSide.Server,
          key.method,
          key.branch,
          request.headers.callId
        )
      )
      entry = ServerEntry[F](machine, from, notices, Some(span))
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
        val outcome         = clientOutcome(event, actions, next.isTerminated)
        val after = if outcome.isDefined then entry.copy(span = None) else entry
        clientTrouble(key, event, actions).traverse_(events) >>
          actions.traverse_(runClient(id, entry, _)) >>
          (entry.span, outcome).tupled.traverse_((span, o) => span.end(o)) >> {
            if next.isTerminated then
              cancelTimers(id) >> entry.notices
                .offer(None)
                .as(
                  table.copy(clients = table.clients - key)
                )
            else
              table
                .copy(clients =
                  table.clients.updated(key, after.copy(machine = next))
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
        val outcome         = serverOutcome(event, actions, next.isTerminated)
        val after = if outcome.isDefined then entry.copy(span = None) else entry
        serverTrouble(key, event, actions).traverse_(events) >>
          actions.traverse_(runServer(key, entry, _)) >>
          (entry.span, outcome).tupled.traverse_((span, o) => span.end(o)) >> {
            if next.isTerminated then
              cancelTimers(id) >> entry.notices
                .offer(None)
                .as(
                  table.copy(servers = table.servers - key)
                )
            else
              table
                .copy(servers =
                  table.servers.updated(key, after.copy(machine = next))
                )
                .pure[F]
          }
    }

  /** How a client transaction's wait for an answer ended in this step, if it
    * did: with a final response, with no response, with a send that failed, or,
    * if the transaction is over and none of those was seen, with nothing.
    */
  private def clientOutcome(
      event: ClientEvent,
      actions: List[ClientAction],
      terminated: Boolean
  ): Option[TransactionOutcome] =
    if actions.contains(ClientAction.TimedOut) then
      Some(TransactionOutcome.TimedOut)
    else if actions.contains(ClientAction.TransportFailed) then
      Some(TransactionOutcome.SendFailed)
    else
      event match {
        case ClientEvent.Response(response) if response.statusCode >= 200 =>
          Some(TransactionOutcome.Answered(response.statusCode))
        case _ => Option.when(terminated)(TransactionOutcome.Abandoned)
      }

  /** As [[clientOutcome]]: a server transaction ends its wait by sending a
    * final response.
    */
  private def serverOutcome(
      event: ServerEvent,
      actions: List[ServerAction],
      terminated: Boolean
  ): Option[TransactionOutcome] =
    if actions.contains(ServerAction.AckTimedOut) then
      Some(TransactionOutcome.TimedOut)
    else
      event match {
        case ServerEvent.TransportError => Some(TransactionOutcome.SendFailed)
        case ServerEvent.Respond(response) if response.statusCode >= 200 =>
          Some(TransactionOutcome.Responded(response.statusCode))
        case _ => Option.when(terminated)(TransactionOutcome.Abandoned)
      }

  /** What a client transaction's step says of the network: a request sent again
    * because a timer fired (not the ACK of a failure, which a response causes),
    * or the end of the wait for an answer.
    */
  private def clientTrouble(
      key: ClientKey,
      event: ClientEvent,
      actions: List[ClientAction]
  ): List[TransactionEvent] = {
    val sentAgain = event match {
      case ClientEvent.TimerFired(_) =>
        actions.collect { case ClientAction.Transmit(_) =>
          TransactionEvent.Retransmitted(TransactionSide.Client, key.method)
        }
      case _ => Nil
    }
    sentAgain ++ actions.collect { case ClientAction.TimedOut =>
      TransactionEvent.TimedOut(TransactionSide.Client, key.method)
    }
  }

  /** As [[clientTrouble]]: a response sent again, because the request came
    * again or a timer fired, or no ACK for a failure.
    */
  private def serverTrouble(
      key: ServerKey,
      event: ServerEvent,
      actions: List[ServerAction]
  ): List[TransactionEvent] = {
    val sentAgain = event match {
      case ServerEvent.RequestRetransmitted | ServerEvent.TimerFired(_) =>
        actions.collect { case ServerAction.Transmit(_) =>
          TransactionEvent.Retransmitted(TransactionSide.Server, key.method)
        }
      case _ => Nil
    }
    sentAgain ++ actions.collect { case ServerAction.AckTimedOut =>
      TransactionEvent.TimedOut(TransactionSide.Server, key.method)
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
        .debug(
          Map(
            LogFields.Status -> response.statusCode.toString,
            LogFields.Branch -> key.branch.value,
            LogFields.Method -> key.method.token
          )
        )(
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
      case ServerAction.Transmit(response) =>
        send(id, entry.from, TransactionMessages.seenFrom(response, entry.from))
      case ServerAction.PassUp(request) =>
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
      logger.warn(
        Map(LogFields.Error -> String.valueOf(error.getMessage)) ++ describe(id)
      )(
        "Send failed"
      ) >>
        commands.offer(Command.SendFailed(id))
    }

  /** The fields that say which transaction a log line is about. */
  private def describe(id: TransactionId): Map[String, String] =
    id match {
      case TransactionId.Client(key) =>
        Map(
          LogFields.Branch -> key.branch.value,
          LogFields.Method -> key.method.token,
          LogFields.Side   -> "client"
        )
      case TransactionId.Server(key) =>
        Map(
          LogFields.Branch -> key.branch.value,
          LogFields.Method -> key.method.token,
          LogFields.Side   -> "server"
        )
    }

  private def cancelTimers(id: TransactionId): F[Unit] =
    TransactionTimer.values.toList.traverse_(t =>
      timers.cancel(TimerKey(id, t))
    )
}
