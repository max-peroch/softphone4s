package softphone4s.ua

import cats.data.NonEmptyList
import cats.effect.std.{Queue, Supervisor}
import cats.effect.syntax.all.*
import cats.effect.{Async, Deferred, Fiber, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import fs2.concurrent.{Channel, Signal, SignallingRef}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.Subscription
import softphone4s.fsm.SipMessages.*
import softphone4s.fsm.{TransactionFailure}
import softphone4s.model.*
import softphone4s.registration.RegistrationFsm
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.subscription.*
import softphone4s.transaction.{
  ClientNotice,
  ClientTransaction,
  ServerTransaction,
  TransactionLayer,
  TransactionMessages
}

import scala.concurrent.duration.*

/** Runs a subscription: the pure [[SubscriptionFsm]] for one dialog after
  * another, the SUBSCRIBEs it asks for, the NOTIFYs routed to it, its timers,
  * and the beginning of a new dialog when one ends in a way that allows it
  * ([[Ending.resubscribeAfter]]).
  */
private[softphone4s] object Subscriber {

  /** Subscribes to `event` of `resource` and keeps the subscription as long as
    * the resource is held; releasing it ends the subscription (waiting up to
    * `stopTimeout` for the answer).
    *
    * @param ways
    *   the ways to reach the notifier, best first; the next is used after a
    *   failure.
    * @param routes
    *   where requests of calls and subscriptions are sent by Call-ID: the
    *   subscription lists its dialog here, so that its NOTIFYs reach it.
    * @param nextHop
    *   where a request within the dialog goes, given the transport and the URI
    *   of its next hop.
    */
  def resource[F[_]: Async](
      layer: TransactionLayer[F],
      ways: NonEmptyList[Uplink],
      routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
      target: SipUri,
      event: EventHeader,
      accept: List[MediaType],
      requested: Expires,
      nextHop: (TransportProtocol, SipUri) => F[SocketAddress[IpAddress]],
      entropy: Entropy[F],
      stopTimeout: FiniteDuration,
      logger: StructuredLogger[F]
  ): Resource[F, Subscription[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      timers     <- Timers.resource[F, SubscriptionTimer]
      agent      <- Resource.eval(
        Agent.create(
          layer,
          ways,
          routes,
          target,
          event,
          accept,
          requested,
          nextHop,
          entropy,
          supervisor,
          timers,
          logger
        )
      )
      _ <- Resource.eval(supervisor.supervise(agent.run))
      _ <- Resource.eval(supervisor.supervise(agent.watchRequests))
      _ <- Resource.make(agent.begin)(_ => agent.release(stopTimeout))
    } yield agent

  /** What the subscription is to, which stays the same for its life. */
  private final case class Aim[F[_]](
      ways: NonEmptyList[Uplink],
      target: SipUri,
      event: EventHeader,
      accept: List[MediaType],
      requested: Expires,
      nextHop: (TransportProtocol, SipUri) => F[SocketAddress[IpAddress]]
  )

  /** What changes as it runs. */
  private final case class Cells[F[_]](
      fsm: Ref[F, SubscriptionFsmState],
      dialog: Ref[F, Option[SipDialog]],
      listed: Ref[F, Option[CallId]],
      pending: Ref[F, Map[Int, ServerTransaction[F]]],
      selected: Ref[F, Int],
      attempt: Ref[F, Int],
      retrying: Ref[F, Option[Fiber[F, Throwable, Unit]]],
      nonceCounts: Ref[F, Map[Nonce, NonceCount]]
  )

  /** What the program sees of it, and waits on. */
  private final case class Outputs[F[_]](
      signal: SignallingRef[F, SubscriptionStatus],
      out: Queue[F, Option[Notification]],
      active: Deferred[F, Either[Ending, Unit]],
      releasing: Deferred[F, Unit],
      stopped: Deferred[F, Unit]
  )

  private final class Agent[F[_]: Async] private (
      layer: TransactionLayer[F],
      routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
      aim: Aim[F],
      cells: Cells[F],
      outputs: Outputs[F],
      entropy: Entropy[F],
      supervisor: Supervisor[F],
      timers: Timers[F, SubscriptionTimer],
      events: Channel[F, SubscriptionEvent],
      requestsIn: Channel[F, DialogInput[F]],
      logger: StructuredLogger[F]
  ) extends Subscription[F] {
    import aim.*
    import cells.*
    import outputs.*

    // The end is put back for the next reader, so that a stream begun after the
    // subscription is over ends at once and does not wait for ever.
    def notifications: Stream[F, Notification] =
      Stream
        .repeatEval(out.take)
        .evalTap(_.fold(out.offer(None))(_ => Async[F].unit))
        .unNoneTerminate

    def status: Signal[F, SubscriptionStatus] = signal

    def awaitActive: F[Either[Ending, Unit]] = active.get

    private def current: F[Uplink] =
      selected.get.map(i => ways.toList(i % ways.size))

    private def moveOn: F[Unit] =
      selected.update(i => (i + 1) % ways.size).whenA(ways.size > 1)

    // ---- Beginning and ending ------------------------------------------------

    /** Begins a dialog: new identifiers, listed so that its NOTIFYs come here.
      */
    def begin: F[Unit] =
      releasing.tryGet.flatMap(released => start.unlessA(released.isDefined))

    private def start: F[Unit] =
      for {
        uplink <- current
        callId <- entropy.callId(uplink.endpoint.ip)
        tag    <- entropy.tag
        _      <- dialog.set(None)
        _      <- pending.set(Map.empty)
        _      <- list(callId)
        _      <- fsm.set(SubscriptionFsmState.Idle)
        _      <- events.send(
          SubscriptionEvent.Start(
            SubscriptionContext(
              callId,
              tag,
              None,
              CSeqNumber.One,
              event,
              requested
            )
          )
        )
      } yield ()

    private def list(callId: CallId): F[Unit] =
      unlist >> routes.update(_ + (callId -> requestsIn)) >> listed.set(
        Some(callId)
      )

    private def unlist: F[Unit] =
      listed.getAndSet(None).flatMap(_.traverse_(id => routes.update(_ - id)))

    /** Ends the subscription: asks the notifier, and waits for its answer for a
      * while.
      */
    def release(timeout: FiniteDuration): F[Unit] =
      releasing.complete(()) >>
        retrying.getAndSet(None).flatMap(_.traverse_(_.cancel)) >>
        fsm.get.flatMap {
          case SubscriptionFsmState.Idle | SubscriptionFsmState.Ended(_) =>
            finish(None)
          case _ =>
            events.send(SubscriptionEvent.Stop) >>
              stopped.get.timeoutTo(timeout, Async[F].unit)
        } >> unlist

    /** The subscription is over for good. */
    private def finish(ending: Option[Ending]): F[Unit] =
      // Once, however many times it is asked: the first to complete `stopped`
      // does the rest.
      stopped
        .complete(())
        .flatMap(first =>
          (unlist >>
            out.offer(None) >>
            active
              .complete(Left(ending.getOrElse(Ending.Unsubscribed)))
              .void).whenA(first)
        )

    // ---- The event loop -------------------------------------------------------

    def run: F[Unit] =
      events.stream
        .merge(timers.fired.map(SubscriptionEvent.TimerFired(_)))
        .evalMap(process)
        .compile
        .drain

    /** NOTIFYs and other requests that come to the dialog. */
    def watchRequests: F[Unit] =
      requestsIn.stream
        .evalMap {
          case DialogInput.Request(transaction) => onRequest(transaction)
          case DialogInput.Unmatched(_)         => Async[F].unit
        }
        .compile
        .drain

    private def onRequest(transaction: ServerTransaction[F]): F[Unit] = {
      val request                           = transaction.request
      def refuse(code: Int, reason: String) =
        transaction.respond(
          TransactionMessages.responseTo(request, code, reason)
        )
      request.method match {
        case SipMethod.Notify =>
          (fsm.get.map(contextOf), dialog.get).flatMapN { (context, known) =>
            val ours =
              context.exists(c => request.headers.to.tag.contains(c.localTag))
            if !ours || known.exists(!_.matches(request)) then
              refuse(481, "Subscription Does Not Exist")
            else
              NotifyInfo.from(request) match {
                case Left(error) =>
                  logger.debug(Map("error" -> error.message))(
                    "Refusing a NOTIFY"
                  ) >>
                    refuse(400, "Bad Request")
                case Right(info) =>
                  pending.update(
                    _ + (request.headers.cseq.seq.value -> transaction)
                  ) >>
                    events
                      .send(SubscriptionEvent.NotifyReceived(request, info))
                      .void
              }
          }
        case _ => refuse(405, "Method Not Allowed")
      }
    }

    private def contextOf(
        state: SubscriptionFsmState
    ): Option[SubscriptionContext] =
      state match {
        case SubscriptionFsmState.Establishing(c, _)   => Some(c)
        case SubscriptionFsmState.AwaitingNotify(c, _) => Some(c)
        case SubscriptionFsmState.Subscribed(c, _, _)  => Some(c)
        case SubscriptionFsmState.Refreshing(c, _, _)  => Some(c)
        case SubscriptionFsmState.Unsubscribing(c)     => Some(c)
        case SubscriptionFsmState.Leaving(c)           => Some(c)
        case SubscriptionFsmState.Idle | SubscriptionFsmState.Ended(_) => None
      }

    private def process(input: SubscriptionEvent): F[Unit] =
      input match {
        // Nothing begins once the subscription is released, whatever was on its
        // way when it was.
        case SubscriptionEvent.Start(_) =>
          releasing.tryGet.flatMap(released =>
            step(input).unlessA(released.isDefined)
          )
        case _ => step(input)
      }

    private def step(input: SubscriptionEvent): F[Unit] =
      for {
        before  <- fsm.get
        _       <- noteDialog(before, input)
        actions <- fsm.modify(s =>
          SubscriptionFsm.transition(input).run(s).value
        )
        _ <- actions.traverse_(interpret)
      } yield ()

    /** The dialog forms when the 2xx or a NOTIFY first gives the notifier's
      * tag; later NOTIFYs may move the target.
      */
    private def noteDialog(
        before: SubscriptionFsmState,
        input: SubscriptionEvent
    ): F[Unit] =
      current.flatMap { uplink =>
        (before, input) match {
          case (
                SubscriptionFsmState.Establishing(context, _),
                SubscriptionEvent.Response(response)
              ) if response.statusCode >= 200 && response.statusCode < 300 =>
            dialog.update(
              _.orElse(
                Some(
                  SubscriptionDialogs.accepted(
                    context,
                    uplink.endpoint.accountUri,
                    response
                  )
                )
              )
            )
          case (
                SubscriptionFsmState.Establishing(context, _),
                SubscriptionEvent.NotifyReceived(request, _)
              ) =>
            dialog.update(
              _.orElse(SubscriptionDialogs.notified(context, request))
            )
          case (_, SubscriptionEvent.NotifyReceived(request, _)) =>
            dialog.update(_.map(SubscriptionDialogs.retargeted(_, request)))
          case _ => Async[F].unit
        }
      }

    // ---- What the machine asks for ----------------------------------------------

    private def interpret(action: SubscriptionAction): F[Unit] =
      action match {
        case SubscriptionAction.SendSubscribe(
              context,
              expires,
              authorization
            ) =>
          send(context, expires, authorization)

        case SubscriptionAction.Respond(request, status, reason) =>
          pending
            .modify(m =>
              (
                m - request.headers.cseq.seq.value,
                m.get(request.headers.cseq.seq.value)
              )
            )
            .flatMap(
              _.traverse_(
                _.respond(
                  TransactionMessages.responseTo(request, status, reason)
                )
              )
            )

        case SubscriptionAction.Deliver(notification) =>
          out.offer(Some(notification))

        case SubscriptionAction.Report(SubscriptionStatus.Ended(ending)) =>
          ended(ending)

        case SubscriptionAction.Report(status) =>
          signal.set(status) >> (status match {
            case SubscriptionStatus.Active(_) =>
              attempt.set(0) >> active.complete(Right(())).void
            case _ => Async[F].unit
          })

        case SubscriptionAction.Timer(timerAction) =>
          timers.run(timerAction)
      }

    private def send(
        context: SubscriptionContext,
        expires: Expires,
        authorization: Option[(AuthKind, DigestChallenge)]
    ): F[Unit] =
      for {
        uplink <- current
        known  <- dialog.get
        branch <- entropy.branch
        endpoint   = uplink.endpoint
        requestUri = known.fold(target)(_.route.requestUri)
        header <- authorization.traverse((kind, challenge) =>
          authorize(endpoint, kind, requestUri, challenge)
        )
        request = known.fold(
          endpoint.buildSubscribe(
            target,
            context.callId,
            context.localTag,
            context.cseq,
            branch,
            context.event,
            expires,
            accept,
            header
          )
        )(d =>
          endpoint.buildSubscribeIn(
            d,
            context.cseq,
            branch,
            context.event,
            expires,
            accept,
            header
          )
        )
        to <- known.fold(Async[F].pure(uplink.peer))(d =>
          nextHop(endpoint.transport, d.route.nextHop).handleError(_ =>
            uplink.peer
          )
        )
        started <- layer.client(request, to)
        _       <- started match {
          case Right(transaction) =>
            supervisor.supervise(forward(transaction)).void
          case Left(_) =>
            events
              .send(
                SubscriptionEvent.TransactionFailed(
                  TransactionFailure.TransportFailed
                )
              )
              .void
        }
      } yield ()

    private def authorize(
        endpoint: LocalEndpoint,
        kind: AuthKind,
        requestUri: SipUri,
        challenge: DigestChallenge
    ): F[ExtensionHeader] =
      (entropy.cnonce, nextNonceCount(challenge.nonce)).mapN {
        (cnonce, count) =>
          endpoint.authorizationFor(
            kind,
            SipMethod.Subscribe,
            requestUri,
            "",
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
            events.send(SubscriptionEvent.Response(response))
          case ClientNotice.TimedOut =>
            events.send(
              SubscriptionEvent.TransactionFailed(TransactionFailure.TimedOut)
            )
          case ClientNotice.TransportFailed =>
            events.send(
              SubscriptionEvent.TransactionFailed(
                TransactionFailure.TransportFailed
              )
            )
        }
        .compile
        .drain

    // ---- A dialog has ended --------------------------------------------------------

    /** What follows the end of a dialog: the end of the subscription if it was
      * asked for or cannot be had again, else a wait and a new dialog.
      */
    private def ended(ending: Ending): F[Unit] =
      unlist >>
        releasing.tryGet.flatMap {
          case Some(_) =>
            signal.set(SubscriptionStatus.Ended(ending)) >> finish(Some(ending))
          case None =>
            (ending match {
              case Ending.Failed(_) => attempt.updateAndGet(_ + 1) <* moveOn
              case _                => attempt.get
            }).flatMap { failures =>
              ending.resubscribeAfter(
                RegistrationFsm.backoff(failures.max(1))
              ) match {
                case None =>
                  signal.set(SubscriptionStatus.Ended(ending)) >> finish(
                    Some(ending)
                  )
                case Some(wait) =>
                  signal.set(SubscriptionStatus.Retrying(ending, wait)) >>
                    supervisor
                      .supervise(Async[F].sleep(wait) >> begin)
                      .flatMap(fiber => retrying.set(Some(fiber)))
              }
            }
        }
  }

  private object Agent {

    def create[F[_]: Async](
        layer: TransactionLayer[F],
        ways: NonEmptyList[Uplink],
        routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
        target: SipUri,
        event: EventHeader,
        accept: List[MediaType],
        requested: Expires,
        nextHop: (TransportProtocol, SipUri) => F[SocketAddress[IpAddress]],
        entropy: Entropy[F],
        supervisor: Supervisor[F],
        timers: Timers[F, SubscriptionTimer],
        logger: StructuredLogger[F]
    ): F[Agent[F]] =
      for {
        cells <- (
          Ref.of[F, SubscriptionFsmState](SubscriptionFsmState.Idle),
          Ref.of[F, Option[SipDialog]](None),
          Ref.of[F, Option[CallId]](None),
          Ref.of[F, Map[Int, ServerTransaction[F]]](Map.empty),
          Ref.of[F, Int](0),
          Ref.of[F, Int](0),
          Ref.of[F, Option[Fiber[F, Throwable, Unit]]](None),
          Ref.of[F, Map[Nonce, NonceCount]](Map.empty)
        ).mapN(Cells[F].apply)
        outputs <- (
          SignallingRef.of[F, SubscriptionStatus](
            SubscriptionStatus.Subscribing
          ),
          Queue.circularBuffer[F, Option[Notification]](64),
          Deferred[F, Either[Ending, Unit]],
          Deferred[F, Unit],
          Deferred[F, Unit]
        ).mapN(Outputs[F].apply)
        events     <- Channel.unbounded[F, SubscriptionEvent]
        requestsIn <- Channel.bounded[F, DialogInput[F]](128)
      } yield new Agent(
        layer,
        routes,
        Aim(ways, target, event, accept, requested, nextHop),
        cells,
        outputs,
        entropy,
        supervisor,
        timers,
        events,
        requestsIn,
        logger
      )
  }
}
