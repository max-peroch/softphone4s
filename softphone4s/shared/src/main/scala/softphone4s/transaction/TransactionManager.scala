package softphone4s.transaction

import cats.effect.std.{Queue, Supervisor}
import cats.effect.{Async, Deferred, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.model.*
import softphone4s.runtime.Timers
import softphone4s.transport.{Inbound, Transport}

object TransactionManager {

  /** Starts the transaction layer over `transport`. Each transaction
    * retransmits or not according to the transport its top Via names.
    * Everything it runs stops when the resource closes.
    */
  def resource[F[_]: Async](
      transport: Transport[F],
      config: TransactionConfig,
      logger: StructuredLogger[F]
  ): Resource[F, TransactionLayer[F]] =
    resourceObserving(
      transport,
      config,
      logger,
      _ => Async[F].unit,
      _ => Async[F].unit,
      TransactionTracing.noop[F]
    )

  /** Ends, as abandoned, the span of every transaction that is open as the
    * layer is released, so that a trace shows it, and does not lose it. A
    * transaction whose span has ended already has none.
    */
  private def endOpenSpans[F[_]: Async](
      table: Ref[F, Table[F]],
      logger: StructuredLogger[F]
  ): F[Unit] =
    table.get
      .flatMap { open =>
        (open.clients.values
          .flatMap(_.span) ++ open.servers.values.flatMap(_.span)).toList
          .traverse_(_.end(TransactionOutcome.Abandoned))
      }
      .handleErrorWith(error =>
        logger.warn(Map(LogFields.Error -> String.valueOf(error.getMessage)))(
          "A span open at shutdown could not be ended"
        )
      )

  /** As [[resource]], and `observed` is told what the servers we send requests
    * to say we look like from where they are (their `received` and `rport`),
    * and `events` of the retransmissions and timeouts the layer has, and
    * `tracing` of each transaction.
    */
  def resourceObserving[F[_]: Async](
      transport: Transport[F],
      config: TransactionConfig,
      logger: StructuredLogger[F],
      observed: ObservedAddress => F[Unit],
      events: TransactionEvent => F[Unit],
      tracing: TransactionTracing[F]
  ): Resource[F, TransactionLayer[F]] =
    for {
      // The table is held here, and not only by the loop that owns it, so that
      // what is open when the layer is released can be reached. The finalizer
      // is acquired before the supervisor, so it runs after the supervisor has
      // stopped the loop: nothing changes the table under it.
      table <- Resource.eval(
        Ref.of[F, Table[F]](Table[F](Map.empty, Map.empty))
      )
      _          <- Resource.onFinalize(endOpenSpans(table, logger))
      supervisor <- Supervisor[F](await = false)
      timers     <- Timers.resource[F, TimerKey]
      commands   <- Resource.eval(Queue.unbounded[F, Command[F]])
      incomingQ  <- Resource.eval(Queue.unbounded[F, ServerTransaction[F]])
      unmatchedQ <- Resource.eval(Queue.unbounded[F, Inbound])
      engine = new TransactionEngine[F](
        transport,
        config,
        timers,
        commands,
        incomingQ,
        unmatchedQ,
        observed,
        events,
        tracing,
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
            // This is the only fiber that changes the table.
            .evalMap { command =>
              table.get.flatMap { current =>
                engine
                  .step(current, command)
                  .handleErrorWith { error =>
                    logger
                      .error(
                        Map(LogFields.Error -> String.valueOf(error.getMessage))
                      )(
                        "Transaction layer failed on a command"
                      )
                      .as(current)
                  }
                  .flatMap(table.set)
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
        transport.send(
          to,
          message match {
            case response: SipResponse =>
              TransactionMessages.seenFrom(response, to)
            case request => request
          }
        )
    }
}
