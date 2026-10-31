package softphone4s.ua

import cats.effect.*
import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.concurrent.Channel
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.{Call, IncomingCall}
import softphone4s.codec.SdpCodec
import softphone4s.fsm.*
import softphone4s.fsm.SipMessages.*
import softphone4s.model.*
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transaction.{
  ClientNotice,
  ClientTransaction,
  ServerNotice,
  ServerTransaction,
  TransactionConfig,
  TransactionLayer,
  TransactionMessages
}
import InboundAction.*

import scala.concurrent.duration.*

/** One incoming call. It answers the INVITE through its server transaction,
  * repeats the 200 OK until the ACK, and turns what happens into events for
  * [[InboundCallFsm]]. Once accepted it is also the [[Call]].
  */
private[softphone4s] final class InboundCall[F[_]: Async] private (
    invite: ServerTransaction[F],
    val account: AccountId,
    layer: TransactionLayer[F],
    resolve: (TransportProtocol, SipUri) => F[SocketAddress[IpAddress]],
    udpLimit: Int,
    endpoint: LocalEndpoint,
    supervisor: Supervisor[F],
    entropy: Entropy[F],
    media: MediaSession[F],
    timers: Timers[F, CallTimer],
    hangupTimeout: FiniteDuration,
    stateRef: Ref[F, InboundState],
    events: Channel[F, InboundEvent],
    serverTransactions: Ref[F, Map[(SipMethod, CSeqNumber), ServerTransaction[
      F
    ]]],
    lastOk: Ref[F, Option[SipResponse]],
    established: Deferred[F, Either[Throwable, Unit]],
    callEnded: Deferred[F, Throwable],
    cancelledByCaller: Deferred[F, Unit],
    finished: Deferred[F, Unit],
    dtmfChannel: Channel[F, DtmfDigit],
    acks: SessionAcks[F],
    logger: StructuredLogger[F]
) extends Call[F]
    with IncomingCall[F] {

  private val peer: SocketAddress[IpAddress] = invite.from

  // ---- IncomingCall ------------------------------------------------------------

  def caller: NameAddr = invite.request.headers.from
  def callee: NameAddr = invite.request.headers.to

  def ringing: F[Unit] = events.send(InboundEvent.UserRinging).void

  def accept: Resource[F, Call[F]] =
    Resource.make(events.send(InboundEvent.UserAccept).as(this: Call[F]))(_ =>
      events.send(InboundEvent.HangupRequested) >>
        finished.get.timeoutTo(hangupTimeout, Async[F].unit)
    )

  def reject(status: FailureStatus): F[Unit] =
    events.send(InboundEvent.UserReject(status)).void

  def cancelled: F[Unit] = cancelledByCaller.get

  // ---- Call --------------------------------------------------------------------

  private def endedInterrupt: F[Either[Throwable, Unit]] =
    callEnded.get.map(Left(_))

  def source: Stream[F, Byte] =
    Stream.eval(callEnded.tryGet).flatMap {
      case Some(err) => Stream.raiseError[F](err)
      case None      => media.source.interruptWhen(endedInterrupt)
    }
  def sink: Pipe[F, Byte, Unit]        = media.sink
  def awaitPickup: F[Unit]             = established.get.flatMap(_.liftTo[F])
  def dtmfEvents: Stream[F, DtmfDigit] =
    Stream.eval(callEnded.tryGet).flatMap {
      case Some(err) => Stream.raiseError[F](err)
      case None      => dtmfChannel.stream.interruptWhen(endedInterrupt)
    }
  def awaitHangup: F[Unit] = callEnded.get.void

  def sendDtmf(digit: DtmfDigit, duration: FiniteDuration): F[DtmfMethod] =
    for {
      checked <- Async[F].fromEither(Dtmf.checkedDuration(duration))
      ready   <- established.tryGet
      _       <- Async[F].raiseUnless(ready.exists(_.isRight))(
        SoftphoneError.CallNotEstablished
      )
      viaRtp <- media.sendDtmf(digit, checked)
      method <-
        if viaRtp then Async[F].pure(DtmfMethod.Rfc4733)
        else
          events
            .send(InboundEvent.DtmfRequested(digit, checked))
            .as(DtmfMethod.SipInfo)
    } yield method

  def changeSession(change: SessionChange): F[Unit] =
    events.send(InboundEvent.SessionChangeRequested(change)).void

  /** True while the call can still be answered or is under way. */
  private[softphone4s] def isOpen: F[Boolean] =
    stateRef.get.map(_ != InboundState.Closed)

  // ---- The event loop -----------------------------------------------------------

  /** Runs until the call is over, then lets `finished` go. */
  private[softphone4s] def run(
      dialogInput: Stream[F, DialogInput[F]]
  ): F[Unit] = {
    val fromDialog = dialogInput.evalMapFilter {
      case DialogInput.Request(transaction) =>
        serverTransactions
          .update(
            _.updated(
              (
                transaction.request.method,
                transaction.request.headers.cseq.seq
              ),
              transaction
            )
          )
          .as(Option(InboundEvent.RequestReceived(transaction.request)))
      case DialogInput.Unmatched(inbound) =>
        (inbound.message match {
          case request: SipRequest if request.method == SipMethod.Ack =>
            Some(request)
          case _ => None
        }).traverse(request => acks.acked(request).as(InboundEvent.AckReceived))
    }
    val fromInvite = invite.notices.collect {
      case ServerNotice.TransportFailed =>
        InboundEvent.TransactionFailed(TransactionFailure.TransportFailed)
    }
    fromDialog
      .merge(events.stream)
      .merge(fromInvite)
      .merge(timers.fired.map(InboundEvent.TimerFired(_)))
      .evalMap(process)
      .takeWhile(identity)
      .compile
      .drain
      .guarantee(finished.complete(()).void)
  }

  /** Runs one event through the machine; false once the call is over. */
  private def process(event: InboundEvent): F[Boolean] =
    media.restrict(endpoint.capabilities).flatMap { caps =>
      stateRef
        .modify(s => InboundCallFsm.transition(event, caps).run(s).value)
        .flatMap { actions =>
          actions
            .traverse_(interpret)
            .as(!actions.exists {
              case NotifyHangup(_) => true
              case _               => false
            })
        }
    }

  // ---- Actions -------------------------------------------------------------------

  private[ua] def interpret(action: InboundAction): F[Unit] =
    action match {
      case SendRinging(ctx) =>
        invite.respond(
          endpoint.buildInviteResponse(ctx.invite, 180, "Ringing", ctx.localTag)
        )

      case SendInviteFailure(request, localTag, status) =>
        invite.respond(
          endpoint.buildInviteResponse(
            request,
            status.value,
            status.reason,
            localTag
          )
        )

      case SendInviteOk(ctx) =>
        val ok = endpoint.buildInviteResponse(
          ctx.invite,
          200,
          "OK",
          ctx.localTag,
          Some(
            SdpCodec.buildAnswerTo(
              ctx.offer,
              endpoint.mediaIp,
              endpoint.rtpPort,
              endpoint.capabilities
            )
          )
        )
        lastOk.set(Some(ok)) >> invite.respond(ok)

      case ResendOk =>
        lastOk.get.flatMap(_.traverse_(layer.transmit(_, peer)))

      case Reply(request, status, reason) =>
        respondTo(
          request,
          TransactionMessages.responseTo(request, status, reason)
        )

      case SendByeOk(request) =>
        respondTo(request, endpoint.buildByeOk(request))
      case SendInfoOk(request) =>
        respondTo(request, endpoint.buildInfoOk(request))

      case SendReinvite(dialog, change) =>
        (
          entropy.branch,
          destinationFor(dialog),
          media.restrict(endpoint.capabilities)
        ).flatMapN { (branch, to, caps) =>
          logger.debug(Map("change" -> change.toString))(
            "Sending a session change"
          ) >>
            startInDialog(
              endpoint
                .withCapabilities(caps)
                .buildSessionRequest(dialog, branch, change),
              dialog,
              to
            )
        }

      case SendAck(dialog) =>
        (entropy.branch, destinationFor(dialog)).flatMapN { (branch, to) =>
          logger.debug("Sending ACK") >>
            transmitInDialog(
              endpoint.buildAck(dialog, branch, None),
              dialog,
              to
            )
        }

      case ApplyAnswer(response) => applyAnswer(response)

      case ScheduleReinviteRetry(role) =>
        entropy.fraction.flatMap(f =>
          timers.start(CallTimer.ReinviteRetry, Reinvite.glareDelay(role, f))
        )

      case SendOptionsOk(request) =>
        logger.debug("Answering OPTIONS") >>
          respondTo(request, endpoint.buildOptionsOk(request))

      case SendSessionOk(request, offer) =>
        media.restrict(endpoint.capabilities).flatMap { caps =>
          val ok = endpoint.buildSessionOk(
            request,
            offer.fold("")(
              SdpCodec
                .buildAnswerTo(_, endpoint.mediaIp, endpoint.rtpPort, caps)
            )
          )
          logger.debug("Answering a session change") >>
            answerSession(request, ok, AckContent.Empty) >>
            offer.traverse_(o => media.retarget(o.ip, o.port, o.direction))
        }

      case SendOfferOk(request) =>
        media.restrict(endpoint.capabilities).flatMap { caps =>
          logger.debug("Answering a re-INVITE with an offer") >>
            answerSession(
              request,
              endpoint.buildSessionOk(
                request,
                SdpCodec.buildOffer(endpoint.mediaIp, endpoint.rtpPort, caps)
              ),
              AckContent.Answer
            )
        }

      case SendBye(dialog) =>
        (entropy.branch, destinationFor(dialog)).flatMapN { (branch, to) =>
          logger.debug("Sending BYE") >> startInDialog(
            endpoint.buildBye(dialog, branch),
            dialog,
            to
          )
        }

      case SendInfo(dialog, digit, duration) =>
        (entropy.branch, destinationFor(dialog)).flatMapN { (branch, to) =>
          logger.debug(Map("digit" -> digit.symbol.toString))(
            "Sending a digit in INFO"
          ) >> startInDialog(
            endpoint.buildInfo(dialog, branch, digit, duration),
            dialog,
            to
          )
        }

      case StartMedia(offer) =>
        SdpCodec.planFor(offer, endpoint.capabilities) match {
          case Right(plan) =>
            media.start(plan, supervisor, dtmfChannel.send(_).void).void
          case Left(reject) =>
            logger.error(Map("reason" -> reject.message))(
              "The offer we accepted cannot be answered; no media"
            )
        }

      case StopMedia => media.stop()

      case NotifyEstablished => established.complete(Right(())).void

      case NotifyHangup(reason) =>
        val err: Throwable = reason match {
          case TerminationReason.RemoteBye        => SoftphoneError.RemoteHangup
          case TerminationReason.TimedOut         => SoftphoneError.CallTimedOut
          case TerminationReason.TransportFailure =>
            SoftphoneError.TransportFailed
          case TerminationReason.Rejected(c, r) =>
            SoftphoneError.CallRejected(c, r)
          case TerminationReason.LocalHangup |
              TerminationReason.CancelledByCaller |
              TerminationReason.RejectedLocally =>
            SoftphoneError.CallCancelled
        }
        val signalStreams = reason match {
          case TerminationReason.LocalHangup => Async[F].unit
          case _                             => callEnded.complete(err).void
        }
        logger.debug(Map("reason" -> reason.toString))("Incoming call ended") >>
          cancelledByCaller
            .complete(())
            .void
            .whenA(reason == TerminationReason.CancelledByCaller) >>
          signalStreams >>
          dtmfChannel.close.void >>
          established.complete(Left(err)).void

      case NotifyDtmf(digit) => dtmfChannel.send(digit).void

      case Timer(timerAction) => timers.run(timerAction)
    }

  /** Answers `request` through its server transaction, or directly if it has
    * none.
    */
  private def respondTo(request: SipRequest, response: SipResponse): F[Unit] =
    respondFrom(request, response).void

  /** As [[respondTo]], and says where the response went. */
  private def respondFrom(
      request: SipRequest,
      response: SipResponse
  ): F[SocketAddress[IpAddress]] = {
    val key = (request.method, request.headers.cseq.seq)
    serverTransactions.modify(m => (m - key, m.get(key))).flatMap {
      case Some(transaction) =>
        transaction.respond(response).as(transaction.from)
      case None => layer.transmit(response, peer).as(peer)
    }
  }

  /** Answers a re-INVITE or UPDATE. The 2xx to a re-INVITE is repeated until
    * its ACK, which carries our answer if the 2xx carried our offer.
    */
  private def answerSession(
      request: SipRequest,
      ok: SipResponse,
      ack: AckContent
  ): F[Unit] =
    if request.method == SipMethod.Invite then
      acks.track(request, ack)(
        respondFrom(request, ok).map(ok -> _)
      )
    else respondTo(request, ok)

  /** Where a request within the dialog goes: the next hop of its route (RFC
    * 3261 §12.2.1.1), or the address the INVITE came from if that cannot be
    * resolved.
    */
  private def destinationFor(dialog: SipDialog): F[SocketAddress[IpAddress]] =
    resolve(endpoint.transport, dialog.route.nextHop).handleErrorWith { error =>
      logger
        .warn(Map("error" -> String.valueOf(error.getMessage)))(
          "Cannot resolve the next hop; using the INVITE's source"
        )
        .as(peer)
    }

  /** Points media where the answer to our re-INVITE says, or stops it if the
    * remote party will not receive.
    */
  private def applyAnswer(response: SipResponse): F[Unit] =
    media
      .restrict(endpoint.capabilities)
      .flatMap(caps =>
        SdpCodec
          .parseOffer(response.body, caps)
          .traverse_(o => media.retarget(o.ip, o.port, o.direction))
      )

  /** Sends a request within `dialog` to `to`. One that would go over UDP but is
    * too big for a datagram goes over TCP instead (RFC 3261 §18.1.1): its Via
    * and Contact are made TCP's, it is sent to where the dialog's next hop
    * serves TCP, and if that connection cannot be made it is sent again over
    * UDP as a new transaction.
    */
  private def startInDialog(
      request: SipRequest,
      dialog: SipDialog,
      to: SocketAddress[IpAddress]
  ): F[Unit] =
    endpoint.via(endpoint.ip, TransportProtocol.Tcp) match {
      case Right(tcp) if StreamSwitch.tooBigForUdp(request, udpLimit) =>
        for {
          tcpTo <- resolve(TransportProtocol.Tcp, dialog.route.nextHop)
            .handleError(_ => to)
          _ <- logger.info(Map("method" -> request.method.toString))(
            "Request too big for UDP; sending it over TCP"
          )
          _ <- startClient(
            StreamSwitch.over(request, tcp),
            tcpTo,
            failover = entropy.branch.flatMap(branch =>
              startClient(StreamSwitch.reissued(request, branch), to).as(true)
            )
          )
        } yield ()
      case _ => startClient(request, to)
    }

  /** Sends a request that is not part of a transaction, the ACK of a 2xx,
    * within `dialog`. Too big for UDP, it goes over TCP, and over UDP after all
    * if that cannot be written.
    */
  private def transmitInDialog(
      request: SipRequest,
      dialog: SipDialog,
      to: SocketAddress[IpAddress]
  ): F[Unit] =
    endpoint.via(endpoint.ip, TransportProtocol.Tcp) match {
      case Right(tcp) if StreamSwitch.tooBigForUdp(request, udpLimit) =>
        resolve(TransportProtocol.Tcp, dialog.route.nextHop)
          .handleError(_ => to)
          .flatMap(tcpTo =>
            layer
              .transmit(StreamSwitch.over(request, tcp), tcpTo)
              .handleErrorWith(_ => layer.transmit(request, to))
          )
      case _ => layer.transmit(request, to)
    }

  private def startClient(
      request: SipRequest,
      to: SocketAddress[IpAddress],
      failover: F[Boolean] = Async[F].pure(false)
  ): F[Unit] =
    layer.client(request, to).flatMap {
      case Right(transaction) =>
        supervisor.supervise(forward(transaction, failover)).void
      case Left(error) =>
        logger.warn(Map("error" -> error.message))(
          "Cannot start a transaction"
        ) >>
          events
            .send(
              InboundEvent.TransactionFailed(TransactionFailure.TransportFailed)
            )
            .void
    }

  /** Passes on what a client transaction reports. When it ends without an
    * answer, `failover` may find another way to try; if it does, that is the
    * end of this transaction and nothing is reported.
    */
  private def forward(
      transaction: ClientTransaction[F],
      failover: F[Boolean]
  ): F[Unit] =
    transaction.notices
      .evalMap {
        case ClientNotice.Response(response) =>
          events.send(InboundEvent.ResponseReceived(response)).void
        case ClientNotice.TimedOut =>
          failover.ifM(
            Async[F].unit,
            events
              .send(InboundEvent.TransactionFailed(TransactionFailure.TimedOut))
              .void
          )
        case ClientNotice.TransportFailed =>
          failover.ifM(
            Async[F].unit,
            events
              .send(
                InboundEvent.TransactionFailed(
                  TransactionFailure.TransportFailed
                )
              )
              .void
          )
      }
      .compile
      .drain
}

private[softphone4s] object InboundCall {

  /** Takes the INVITE of `invite` and starts the call: the machine's first
    * actions run at once, so a call we cannot take is already answered with its
    * failure when this returns (check `isOpen`).
    */
  def create[F[_]: Async](
      invite: ServerTransaction[F],
      account: AccountId,
      layer: TransactionLayer[F],
      resolve: (TransportProtocol, SipUri) => F[SocketAddress[IpAddress]],
      udpLimit: Int,
      endpoint: LocalEndpoint,
      supervisor: Supervisor[F],
      entropy: Entropy[F],
      media: MediaSession[F],
      timers: Timers[F, CallTimer],
      transactionConfig: TransactionConfig,
      answerTimeout: FiniteDuration,
      hangupTimeout: FiniteDuration,
      logger: StructuredLogger[F]
  ): F[InboundCall[F]] =
    for {
      localTag <- entropy.tag
      (state, firstActions) = InboundCallFsm.start(
        invite.request,
        localTag,
        transactionConfig,
        answerTimeout,
        endpoint.capabilities
      )
      stateRef           <- Ref[F].of(state)
      events             <- Channel.unbounded[F, InboundEvent]
      serverTransactions <- Ref[F].of(
        Map.empty[(SipMethod, CSeqNumber), ServerTransaction[F]]
      )
      lastOk            <- Ref[F].of(Option.empty[SipResponse])
      established       <- Deferred[F, Either[Throwable, Unit]]
      callEnded         <- Deferred[F, Throwable]
      cancelledByCaller <- Deferred[F, Unit]
      finished          <- Deferred[F, Unit]
      dtmfChannel       <- Channel.bounded[F, DtmfDigit](64)
      acks              <- SessionAcks.create(
        supervisor,
        transactionConfig,
        (response, to) => layer.transmit(response, to),
        offer => media.retarget(offer.ip, offer.port, offer.direction),
        media.restrict(endpoint.capabilities),
        logger
      )
      call = new InboundCall(
        invite,
        account,
        layer,
        resolve,
        udpLimit,
        endpoint,
        supervisor,
        entropy,
        media,
        timers,
        hangupTimeout,
        stateRef,
        events,
        serverTransactions,
        lastOk,
        established,
        callEnded,
        cancelledByCaller,
        finished,
        dtmfChannel,
        acks,
        logger
      )
      _ <- firstActions.traverse_(call.interpret)
    } yield call
}
