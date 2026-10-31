package softphone4s.transaction

import cats.effect.std.{Queue, Supervisor}
import cats.effect.{Async, Deferred, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import org.typelevel.log4cats.StructuredLogger
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
    resourceObserving(transport, config, logger, _ => Async[F].unit)

  /** As [[resource]], and `observed` is told what the servers we send requests
    * to say we look like from where they are (their `received` and `rport`).
    */
  def resourceObserving[F[_]: Async](
      transport: Transport[F],
      config: TransactionConfig,
      logger: StructuredLogger[F],
      observed: ObservedAddress => F[Unit]
  ): Resource[F, TransactionLayer[F]] =
    for {
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
