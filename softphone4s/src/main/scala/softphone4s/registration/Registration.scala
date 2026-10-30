package softphone4s.registration

import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.effect.{Async, Deferred, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.concurrent.{Channel, Signal, SignallingRef}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.fsm.{TimerAction, TransactionFailure}
import softphone4s.fsm.SipMessages.*
import softphone4s.model.*
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transaction.{
  ClientNotice,
  ClientTransaction,
  TransactionLayer
}

import scala.concurrent.duration.*

/** A registration of the account at its registrar, kept alive for as long as
  * the resource that holds it.
  */
trait Registration[F[_]] {

  /** Where the registration stands now, and every change to it. */
  def state: Signal[F, RegistrationState]

  /** Completes once the account is registered (immediately if it already is).
    */
  def awaitRegistered: F[Unit]
}

object Registration {

  /** Registers `endpoint`'s account at `registrar` and keeps the binding fresh:
    * refresh before it lapses, retry with backoff after a failure. Releasing
    * the resource removes the binding (waiting up to `stopTimeout` for the
    * answer).
    *
    * @param registrarUri
    *   the Request-URI of the REGISTER, normally the account's domain.
    * @param requested
    *   the lifetime asked for.
    */
  def resource[F[_]: Async](
      layer: TransactionLayer[F],
      registrar: SocketAddress[IpAddress],
      registrarUri: SipUri,
      endpoint: LocalEndpoint,
      entropy: Entropy[F],
      requested: Expires,
      stopTimeout: FiniteDuration,
      logger: StructuredLogger[F]
  ): Resource[F, Registration[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      timers     <- Timers.resource[F, RegistrationTimer]
      agent      <- Resource.eval(
        Agent.create(
          layer,
          registrar,
          registrarUri,
          endpoint,
          entropy,
          requested,
          supervisor,
          timers,
          logger
        )
      )
      _ <- Resource.eval(supervisor.supervise(agent.run))
      _ <- Resource.make(agent.start)(_ => agent.stop(stopTimeout))
    } yield agent

  private final class Agent[F[_]: Async] private (
      layer: TransactionLayer[F],
      registrar: SocketAddress[IpAddress],
      registrarUri: SipUri,
      endpoint: LocalEndpoint,
      entropy: Entropy[F],
      requested: Expires,
      supervisor: Supervisor[F],
      timers: Timers[F, RegistrationTimer],
      signal: SignallingRef[F, RegistrationState],
      fsm: Ref[F, RegistrationFsmState],
      events: Channel[F, RegistrationEvent],
      nonceCounts: Ref[F, Map[Nonce, NonceCount]],
      stopped: Deferred[F, Unit],
      logger: StructuredLogger[F]
  ) extends Registration[F] {

    def state: Signal[F, RegistrationState] = signal

    def awaitRegistered: F[Unit] =
      signal.discrete
        .collectFirst { case RegistrationState.Registered(_) => () }
        .compile
        .drain

    /** Begins registering, with the identifiers that stay the same for the life
      * of the binding.
      */
    def start: F[Unit] =
      (entropy.callId(endpoint.ip), entropy.tag).flatMapN {
        (callId, localTag) =>
          events
            .send(
              RegistrationEvent.Start(
                RegistrationContext(
                  callId,
                  localTag,
                  CSeqNumber.One,
                  endpoint.contact,
                  requested
                )
              )
            )
            .void
      }

    /** Removes the binding and waits for the registrar's answer, for a while.
      */
    def stop(timeout: FiniteDuration): F[Unit] =
      events.send(RegistrationEvent.Stop) >>
        stopped.get.timeoutTo(timeout, Async[F].unit)

    def run: F[Unit] =
      events.stream
        .merge(timers.fired.map(RegistrationEvent.TimerFired(_)))
        .evalMap(process)
        .compile
        .drain

    private def process(event: RegistrationEvent): F[Unit] =
      fsm
        .modify(s => RegistrationFsm.transition(event).run(s).value)
        .flatMap(_.traverse_(interpret))

    private def interpret(action: RegistrationAction): F[Unit] =
      action match {
        case RegistrationAction.Notify(newState) =>
          signal.set(newState) >>
            stopped
              .complete(())
              .void
              .whenA(newState == RegistrationState.Unregistered)

        case RegistrationAction.Timer(TimerAction.Start(timer, after)) =>
          entropy.fraction.flatMap(f => timers.start(timer, jittered(after, f)))

        case RegistrationAction.Timer(TimerAction.Cancel(timer)) =>
          timers.cancel(timer)

        case RegistrationAction.SendRegister(
              context,
              lifetime,
              authorization
            ) =>
          for {
            branch <- entropy.branch
            auth   <- authorization.traverse { (kind, challenge) =>
              (entropy.cnonce, nextNonceCount(challenge.nonce)).mapN {
                (cnonce, count) =>
                  endpoint.authorizationFor(
                    kind,
                    SipMethod.Register,
                    registrarUri,
                    "",
                    challenge,
                    cnonce,
                    count
                  )
              }
            }
            request = endpoint.buildRegister(
              registrarUri,
              context.callId,
              context.localTag,
              context.cseq,
              branch,
              lifetime,
              auth
            )
            _ <- logger.debug(
              Map(
                "cseq"     -> context.cseq.value.toString,
                "lifetime" -> lifetime.value.toString,
                "auth"     -> auth.isDefined.toString
              )
            )("Sending REGISTER")
            _ <- layer.client(request, registrar).flatMap {
              case Right(transaction) =>
                supervisor.supervise(forward(transaction)).void
              case Left(error) =>
                logger.warn(Map("error" -> error.message))(
                  "Cannot start a REGISTER"
                ) >>
                  events
                    .send(
                      RegistrationEvent
                        .TransactionFailed(TransactionFailure.TransportFailed)
                    )
                    .void
            }
          } yield ()
      }

    private def forward(transaction: ClientTransaction[F]): F[Unit] =
      transaction.notices
        .evalMap {
          case ClientNotice.Response(response) =>
            events.send(RegistrationEvent.Response(response))
          case ClientNotice.TimedOut =>
            events.send(
              RegistrationEvent.TransactionFailed(TransactionFailure.TimedOut)
            )
          case ClientNotice.TransportFailed =>
            events.send(
              RegistrationEvent.TransactionFailed(
                TransactionFailure.TransportFailed
              )
            )
        }
        .compile
        .drain

    private def nextNonceCount(nonce: Nonce): F[NonceCount] =
      nonceCounts.modify { m =>
        val next = m.get(nonce).fold(NonceCount.One)(_.next)
        (m.updated(nonce, next), next)
      }
  }

  private object Agent {

    def create[F[_]: Async](
        layer: TransactionLayer[F],
        registrar: SocketAddress[IpAddress],
        registrarUri: SipUri,
        endpoint: LocalEndpoint,
        entropy: Entropy[F],
        requested: Expires,
        supervisor: Supervisor[F],
        timers: Timers[F, RegistrationTimer],
        logger: StructuredLogger[F]
    ): F[Agent[F]] =
      for {
        signal <- SignallingRef.of[F, RegistrationState](
          RegistrationState.Unregistered
        )
        fsm    <- Ref.of[F, RegistrationFsmState](RegistrationFsmState.Idle)
        events <- Channel.unbounded[F, RegistrationEvent]
        nonceCounts <- Ref.of[F, Map[Nonce, NonceCount]](Map.empty)
        stopped     <- Deferred[F, Unit]
      } yield new Agent(
        layer,
        registrar,
        registrarUri,
        endpoint,
        entropy,
        requested,
        supervisor,
        timers,
        signal,
        fsm,
        events,
        nonceCounts,
        stopped,
        logger
      )
  }

  /** `base` stretched by up to 10%, so that many registrations do not refresh
    * or retry in step.
    */
  private[registration] def jittered(
      base: FiniteDuration,
      fraction: Double
  ): FiniteDuration =
    (base.toMillis * (1.0 + 0.1 * fraction)).toLong.millis
}
