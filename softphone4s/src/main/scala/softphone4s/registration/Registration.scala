package softphone4s.registration

import cats.data.NonEmptyList
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

  /** Registers the account at the registrar and keeps the binding fresh:
    * refresh before it lapses, retry with backoff after a failure. Releasing
    * the resource removes the binding (waiting up to `stopTimeout` for the
    * answer).
    *
    * @param uplinks
    *   the ways to reach the registrar, best first. The first is used; each
    *   time an attempt fails the next one is, and after the last the first
    *   again, so a registrar with several servers is found when one is down.
    * @param registrarUri
    *   the Request-URI of the REGISTER, normally the account's domain.
    * @param requested
    *   the lifetime asked for.
    */
  def resource[F[_]: Async](
      layer: TransactionLayer[F],
      uplinks: NonEmptyList[Uplink],
      publicAddress: Signal[F, Option[SocketAddress[IpAddress]]],
      registrarUri: SipUri,
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
          uplinks,
          publicAddress,
          registrarUri,
          entropy,
          requested,
          supervisor,
          timers,
          logger
        )
      )
      _ <- Resource.eval(supervisor.supervise(agent.run))
      _ <- Resource.eval(supervisor.supervise(agent.followAddress))
      _ <- Resource.make(agent.start)(_ => agent.stop(stopTimeout))
    } yield agent

  /** The Contact last registered, and over which of the ways to the registrar.
    */
  private[registration] final case class Bound(uplink: Int, contact: SipUri)

  private final class Agent[F[_]: Async] private (
      layer: TransactionLayer[F],
      uplinks: NonEmptyList[Uplink],
      publicAddress: Signal[F, Option[SocketAddress[IpAddress]]],
      selected: Ref[F, Int],
      sentContact: Ref[F, Option[SipUri]],
      bound: Ref[F, Option[Registration.Bound]],
      registrarUri: SipUri,
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

    /** The way to the registrar that is in use now. */
    private def current: F[Uplink] =
      selected.get.map(i => uplinks.toList(i % uplinks.size))

    /** After a failure: the next way, for the retry. */
    private def moveOn: F[Unit] =
      selected.update(i => (i + 1) % uplinks.size).whenA(uplinks.size > 1)

    def awaitRegistered: F[Unit] =
      signal.discrete
        .collectFirst { case RegistrationState.Registered(_) => () }
        .compile
        .drain

    /** Begins registering, with the identifiers that stay the same for the life
      * of the binding.
      */
    def start: F[Unit] =
      (entropy.callId(uplinks.head.endpoint.ip), entropy.tag).flatMapN {
        (callId, localTag) =>
          events
            .send(
              RegistrationEvent.Start(
                RegistrationContext(
                  callId,
                  localTag,
                  CSeqNumber.One,
                  uplinks.head.endpoint.contact,
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

    /** The endpoint of `uplink` as the outside sees it, when a server has told
      * us (a UDP registration behind a NAT); otherwise as it is.
      */
    private def asSeen(endpoint: LocalEndpoint): F[LocalEndpoint] =
      publicAddress.get.map(
        _.flatMap(endpoint.mappedTo(_).toOption).getOrElse(endpoint)
      )

    /** Tells the state machine whenever the address we are seen at changes, so
      * that the binding is moved there at once.
      */
    def followAddress: F[Unit] =
      publicAddress.discrete.changes
        .evalMap(seen =>
          (current, seen.pure[F]).flatMapN { (uplink, address) =>
            address
              .flatMap(uplink.endpoint.mappedTo(_).toOption)
              .traverse_(mapped =>
                events.send(RegistrationEvent.AddressChanged(mapped.contact))
              )
          }
        )
        .compile
        .drain

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
          moveOn.whenA(isFailure(newState)) >>
            signal.set(newState) >>
            (retireOldBinding >> reconcileContact).whenA(
              isRegistered(newState)
            ) >>
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
            uplink   <- current
            endpoint <- asSeen(uplink.endpoint)
            branch   <- entropy.branch
            auth     <- authorization.traverse(authorize(endpoint, _, _))
            _        <- sentContact.set(Some(endpoint.contact))
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
            _ <- layer.client(request, uplink.peer).flatMap {
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

    private def authorize(
        endpoint: LocalEndpoint,
        kind: AuthKind,
        challenge: DigestChallenge
    ): F[ExtensionHeader] =
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

    /** Registered at a Contact other than the one registered before, on the
      * same way to the registrar: the old binding would otherwise stay until it
      * expires, and calls could be sent to where we no longer are.
      */
    private def retireOldBinding: F[Unit] =
      (selected.get, sentContact.get).flatMapN { (index, sent) =>
        sent.traverse_ { now =>
          bound
            .getAndSet(Some(Registration.Bound(index, now)))
            .flatMap(
              _.collect {
                case Registration.Bound(i, old) if i == index && old != now =>
                  old
              }
                .traverse_(old => supervisor.supervise(removeBinding(old)).void)
            )
        }
      }

    /** Removes the binding at `old` (RFC 3261 §10.2.2), as a registration of
      * its own: a new Call-ID, so that its CSeq does not have to follow ours.
      * Best effort: it is answered once if challenged, and otherwise only
      * logged, since the binding expires on its own.
      */
    private def removeBinding(old: SipUri): F[Unit] =
      for {
        uplink   <- current
        callId   <- entropy.callId(uplink.endpoint.ip)
        localTag <- entropy.tag
        _        <- removal(uplink, old, callId, localTag, CSeqNumber.One, None)
      } yield ()

    private def removal(
        uplink: Uplink,
        old: SipUri,
        callId: CallId,
        localTag: Tag,
        cseq: CSeqNumber,
        authorization: Option[(AuthKind, DigestChallenge)]
    ): F[Unit] = {
      val endpoint = uplink.endpoint.advertising(old)
      for {
        branch <- entropy.branch
        auth   <- authorization.traverse(authorize(endpoint, _, _))
        request = endpoint.buildRegister(
          registrarUri,
          callId,
          localTag,
          cseq,
          branch,
          Expires.Zero,
          auth
        )
        started <- layer.client(request, uplink.peer)
        answer  <- started.toOption.flatTraverse(
          _.notices
            .collectFirst {
              case ClientNotice.Response(r) if r.statusCode >= 200 => r
            }
            .compile
            .last
        )
        _ <- answer match {
          case Some(Challenges.Challenged(kind, challenges)) =>
            Challenges
              .once(challenges, authorization.fold(0)(_ => 1))
              .traverse_(challenge =>
                removal(
                  uplink,
                  old,
                  callId,
                  localTag,
                  cseq.next,
                  Some((kind, challenge))
                )
              )
          case other =>
            logger.debug(
              Map(
                "contact" -> old.render,
                "status"  -> other.fold("none")(_.statusCode.toString)
              )
            )("Removed the old binding")
        }
      } yield ()
    }

    private def isRegistered(state: RegistrationState): Boolean = state match {
      case RegistrationState.Registered(_) => true
      case _                               => false
    }

    /** Registered, but at a Contact that is not the one we would send now: the
      * outside address was learned from this very answer, or while it was in
      * flight. Say so, and the state machine registers the new one.
      */
    private def reconcileContact: F[Unit] =
      (current.flatMap(u => asSeen(u.endpoint)), sentContact.get).flatMapN {
        (wanted, sent) =>
          events
            .send(RegistrationEvent.AddressChanged(wanted.contact))
            .whenA(!sent.contains(wanted.contact))
      }

    private def isFailure(state: RegistrationState): Boolean = state match {
      case RegistrationState.Failed(_, _) => true
      case _                              => false
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
        uplinks: NonEmptyList[Uplink],
        publicAddress: Signal[F, Option[SocketAddress[IpAddress]]],
        registrarUri: SipUri,
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
        selected    <- Ref.of[F, Int](0)
        sentContact <- Ref.of[F, Option[SipUri]](None)
        bound       <- Ref.of[F, Option[Registration.Bound]](None)
        fsm    <- Ref.of[F, RegistrationFsmState](RegistrationFsmState.Idle)
        events <- Channel.unbounded[F, RegistrationEvent]
        nonceCounts <- Ref.of[F, Map[Nonce, NonceCount]](Map.empty)
        stopped     <- Deferred[F, Unit]
      } yield new Agent(
        layer,
        uplinks,
        publicAddress,
        selected,
        sentContact,
        bound,
        registrarUri,
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
