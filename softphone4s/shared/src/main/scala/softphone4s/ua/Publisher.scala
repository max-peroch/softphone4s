package softphone4s.ua

import cats.data.NonEmptyList
import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.effect.{Async, Deferred, Fiber, Ref, Resource}
import cats.syntax.all.*
import fs2.concurrent.{Channel, Signal, SignallingRef}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.Publication
import softphone4s.fsm.SipMessages.*
import softphone4s.fsm.TransactionFailure
import softphone4s.model.*
import softphone4s.publication.*
import softphone4s.registration.RegistrationFsm
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.subscription.EventHeader
import softphone4s.transaction.{
  ClientNotice,
  ClientTransaction,
  TransactionLayer
}

import scala.concurrent.duration.*

/** Runs a publication: the pure [[PublicationFsm]], the PUBLISHes it asks for,
  * its timers, and a new run after a failure that allows one.
  */
private[softphone4s] object Publisher {

  /** Publishes `document` as the state of `presentity` for `event`, and keeps
    * it published as long as the resource is held; releasing it removes the
    * state (waiting up to `stopTimeout` for the answer).
    *
    * @param ways
    *   the ways to reach the notifier, best first; the next is used after a
    *   failure.
    */
  def resource[F[_]: Async](
      layer: TransactionLayer[F],
      ways: NonEmptyList[Uplink],
      presentity: SipUri,
      event: EventHeader,
      document: PublicationBody,
      requested: Expires,
      entropy: Entropy[F],
      stopTimeout: FiniteDuration,
      phoneLogger: StructuredLogger[F]
  ): Resource[F, Publication[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      logger = phoneLogger.addContext(
        Map(
          LogFields.Account    -> ways.head.endpoint.accountUri.render,
          LogFields.Event      -> event.render,
          LogFields.Presentity -> presentity.render
        )
      )
      timers <- Timers.resource[F, PublicationTimer]
      agent  <- Resource.eval(
        Agent.create(
          layer,
          Aim(ways, presentity, event, requested),
          document,
          entropy,
          supervisor,
          timers,
          logger
        )
      )
      _ <- Resource.eval(supervisor.supervise(agent.run))
      _ <- Resource.make(agent.begin)(_ => agent.release(stopTimeout))
    } yield agent

  /** What is published, and where, which stays the same for its life. */
  private final case class Aim(
      ways: NonEmptyList[Uplink],
      presentity: SipUri,
      event: EventHeader,
      requested: Expires
  )

  /** What changes as it runs. */
  private final case class Cells[F[_]](
      fsm: Ref[F, PublicationState],
      document: Ref[F, PublicationBody],
      selected: Ref[F, Int],
      attempt: Ref[F, Int],
      retrying: Ref[F, Option[Fiber[F, Throwable, Unit]]],
      nonceCounts: Ref[F, Map[Nonce, NonceCount]]
  )

  /** What the program sees of it, and waits on. */
  private final case class Outputs[F[_]](
      signal: SignallingRef[F, PublicationStatus],
      published: Deferred[F, Either[PublicationEnding, Unit]],
      releasing: Deferred[F, Unit],
      stopped: Deferred[F, Unit]
  )

  private final class Agent[F[_]: Async] private (
      layer: TransactionLayer[F],
      aim: Aim,
      cells: Cells[F],
      outputs: Outputs[F],
      entropy: Entropy[F],
      supervisor: Supervisor[F],
      timers: Timers[F, PublicationTimer],
      events: Channel[F, PublicationEvent],
      logger: StructuredLogger[F]
  ) extends Publication[F] {
    import aim.*
    import cells.*
    import outputs.*

    def update(next: PublicationBody): F[Unit] =
      // Kept, so that a run begun after a failure publishes the latest.
      document.set(next) >> events.send(PublicationEvent.Update(next)).void

    def status: Signal[F, PublicationStatus] = signal

    def awaitPublished: F[Either[PublicationEnding, Unit]] = published.get

    private def current: F[Uplink] =
      selected.get.map(i => ways.toList(i % ways.size))

    private def moveOn: F[Unit] =
      selected.update(i => (i + 1) % ways.size).whenA(ways.size > 1)

    // ---- Beginning and ending ------------------------------------------------

    def begin: F[Unit] =
      releasing.tryGet.flatMap(released => start.unlessA(released.isDefined))

    /** A run: new identifiers, and the latest document. */
    private def start: F[Unit] =
      for {
        uplink <- current
        callId <- entropy.callId(uplink.endpoint.ip)
        tag    <- entropy.tag
        latest <- document.get
        _      <- fsm.set(PublicationState.Idle)
        _      <- events.send(
          PublicationEvent.Start(
            PublicationContext(
              callId,
              tag,
              CSeqNumber.One,
              event,
              requested,
              latest
            )
          )
        )
      } yield ()

    /** Removes the state: asks the notifier, and waits for its answer for a
      * while.
      */
    def release(timeout: FiniteDuration): F[Unit] =
      releasing.complete(()) >>
        retrying.getAndSet(None).flatMap(_.traverse_(_.cancel)) >>
        fsm.get.flatMap {
          case PublicationState.Idle | PublicationState.Ended(_) =>
            // Between runs, or over: the status must not go on saying that a try
            // is awaited.
            signal.update {
              case over @ PublicationStatus.Ended(_) => over
              case _ => PublicationStatus.Ended(PublicationEnding.Removed)
            } >> finish(PublicationEnding.Removed)
          case _ =>
            events.send(PublicationEvent.Stop) >>
              stopped.get.timeoutTo(timeout, Async[F].unit)
        }

    /** The publication is over for good. Once, however many times it is asked.
      */
    private def finish(ending: PublicationEnding): F[Unit] =
      stopped
        .complete(())
        .flatMap(first => published.complete(Left(ending)).void.whenA(first))

    // ---- The event loop -------------------------------------------------------

    def run: F[Unit] =
      events.stream
        .merge(timers.fired.map(PublicationEvent.TimerFired(_)))
        .evalMap(process)
        .compile
        .drain

    private def process(input: PublicationEvent): F[Unit] =
      input match {
        // Nothing begins once the publication is released, whatever was on its
        // way when it was.
        case PublicationEvent.Start(context) =>
          // The document is read now, in the loop that also takes the updates: one
          // given before this has been kept, and one after it arrives as an event
          // to a publication that has begun.
          (releasing.tryGet, document.get).flatMapN { (released, latest) =>
            step(PublicationEvent.Start(context.copy(document = latest)))
              .unlessA(released.isDefined)
          }
        case _ => step(input)
      }

    private def step(input: PublicationEvent): F[Unit] =
      fsm
        .modify(s => PublicationFsm.transition(input).run(s).value)
        .flatMap(_.traverse_(interpret))

    // ---- What the machine asks for ----------------------------------------------

    private def interpret(action: PublicationAction): F[Unit] =
      action match {
        case PublicationAction.SendPublish(
              context,
              expires,
              tag,
              body,
              authorization
            ) =>
          send(context, expires, tag, body, authorization)

        case PublicationAction.Report(PublicationStatus.Ended(ending)) =>
          ended(ending)

        case PublicationAction.Report(status) =>
          signal.set(status) >> (status match {
            case PublicationStatus.Published(_) =>
              attempt.set(0) >> published.complete(Right(())).void
            case _ => Async[F].unit
          })

        case PublicationAction.Timer(timerAction) =>
          timers.run(timerAction)
      }

    private def send(
        context: PublicationContext,
        expires: Expires,
        tag: Option[EntityTag],
        body: Option[PublicationBody],
        authorization: Option[(AuthKind, DigestChallenge)]
    ): F[Unit] =
      for {
        uplink <- current
        branch <- entropy.branch
        header <- authorization.traverse((kind, challenge) =>
          authorize(
            uplink.endpoint,
            kind,
            body.fold("")(_.text),
            challenge
          )
        )
        request = uplink.endpoint.buildPublish(
          presentity,
          context.callId,
          context.localTag,
          context.cseq,
          branch,
          context.event,
          expires,
          tag,
          body,
          header
        )
        started <- layer.client(request, uplink.peer)
        _       <- started match {
          case Right(transaction) =>
            supervisor.supervise(forward(transaction)).void
          case Left(_) =>
            events
              .send(
                PublicationEvent.TransactionFailed(
                  TransactionFailure.TransportFailed
                )
              )
              .void
        }
      } yield ()

    private def authorize(
        endpoint: LocalEndpoint,
        kind: AuthKind,
        body: String,
        challenge: DigestChallenge
    ): F[ExtensionHeader] =
      (entropy.cnonce, nextNonceCount(challenge.nonce)).mapN {
        (cnonce, count) =>
          endpoint.authorizationFor(
            kind,
            SipMethod.Publish,
            presentity,
            body,
            challenge,
            cnonce,
            count
          )
      }

    private def nextNonceCount(nonce: Nonce): F[NonceCount] =
      nonceCounts.modify { m =>
        val next = m.get(nonce).fold(NonceCount.One)(_.next)
        (m.updated(nonce, next), next)
      }

    private def forward(transaction: ClientTransaction[F]): F[Unit] =
      transaction.notices
        .evalMap {
          case ClientNotice.Response(response) =>
            events.send(PublicationEvent.Response(response))
          case ClientNotice.TimedOut =>
            events.send(
              PublicationEvent.TransactionFailed(TransactionFailure.TimedOut)
            )
          case ClientNotice.TransportFailed =>
            events.send(
              PublicationEvent.TransactionFailed(
                TransactionFailure.TransportFailed
              )
            )
        }
        .compile
        .drain

    // ---- A run has ended ---------------------------------------------------------

    /** What follows the end of a run: the end of the publication if it was
      * asked for or cannot be had again, else a wait and a new run.
      */
    private def ended(ending: PublicationEnding): F[Unit] =
      releasing.tryGet.flatMap {
        case Some(_) =>
          signal.set(PublicationStatus.Ended(ending)) >> finish(ending)
        case None =>
          ending match {
            case PublicationEnding.Failed(_) =>
              (attempt.updateAndGet(_ + 1) <* moveOn).flatMap { failures =>
                val wait = RegistrationFsm.backoff(failures.max(1))
                logger.debug(
                  Map(
                    LogFields.Presentity -> presentity.render,
                    "ending"             -> ending.toString,
                    "wait"               -> wait.toString
                  )
                )("A publication failed, trying again") >>
                  signal.set(PublicationStatus.Retrying(ending, wait)) >>
                  supervisor
                    .supervise(Async[F].sleep(wait) >> begin)
                    .flatMap(fiber => retrying.set(Some(fiber)))
              }
            case _ =>
              signal.set(PublicationStatus.Ended(ending)) >> finish(ending)
          }
      }
  }

  private object Agent {

    def create[F[_]: Async](
        layer: TransactionLayer[F],
        aim: Aim,
        document: PublicationBody,
        entropy: Entropy[F],
        supervisor: Supervisor[F],
        timers: Timers[F, PublicationTimer],
        logger: StructuredLogger[F]
    ): F[Agent[F]] =
      for {
        cells <- (
          Ref.of[F, PublicationState](PublicationState.Idle),
          Ref.of[F, PublicationBody](document),
          Ref.of[F, Int](0),
          Ref.of[F, Int](0),
          Ref.of[F, Option[Fiber[F, Throwable, Unit]]](None),
          Ref.of[F, Map[Nonce, NonceCount]](Map.empty)
        ).mapN(Cells[F].apply)
        outputs <- (
          SignallingRef.of[F, PublicationStatus](PublicationStatus.Publishing),
          Deferred[F, Either[PublicationEnding, Unit]],
          Deferred[F, Unit],
          Deferred[F, Unit]
        ).mapN(Outputs[F].apply)
        events <- Channel.unbounded[F, PublicationEvent]
      } yield new Agent(
        layer,
        aim,
        cells,
        outputs,
        entropy,
        supervisor,
        timers,
        events,
        logger
      )
  }
}
