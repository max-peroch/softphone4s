package softphone4s.ua

import cats.effect.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.concurrent.Channel
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.metrics.CallDirection
import softphone4s.logging.LogFields
import softphone4s.Call
import softphone4s.codec.{SdpCodec, SipCodec}
import softphone4s.sdp.Capabilities
import softphone4s.fsm.*
import softphone4s.fsm.SipMessages.*
import softphone4s.media.JitterStats
import softphone4s.model.*
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transaction.{
  ServerTransaction,
  TransactionConfig,
  TransactionLayer,
  TransactionMessages
}
import SipAction.*
import TerminationReason.*

import scala.concurrent.duration.*

/** One outbound call: the INVITE, BYE and CANCEL go out as client transactions,
  * in-dialog BYE and INFO arrive as server transactions, and everything that
  * happens becomes an event for [[CallFsm]].
  */
private[softphone4s] class OutboundCall[F[_]: Async] private (
    answered: Deferred[F, Either[Throwable, Unit]],
    callEnded: Deferred[F, Throwable],
    endReason: Deferred[F, TerminationReason],
    dtmfChannel: Channel[F, DtmfDigit],
    hangupSignal: Deferred[F, Unit],
    stateRef: Ref[F, CallState],
    layer: TransactionLayer[F],
    uplink: Ref[F, Uplink],
    alternatives: Ref[F, List[Uplink]],
    oversize: Ref[F, Option[Uplink]],
    udpLimit: Int,
    resolve: (TransportProtocol, SipUri) => F[SocketAddress[IpAddress]],
    events: Channel[F, SipEvent],
    serverTransactions: Ref[F, Map[(SipMethod, CSeqNumber), ServerTransaction[
      F
    ]]],
    cancelled: Ref[F, Boolean],
    supervisor: Supervisor[F],
    inviteBranch: Ref[F, Branch],
    nonceCounts: Ref[F, Map[Nonce, NonceCount]],
    entropy: Entropy[F],
    media: MediaSession[F],
    timers: Timers[F, CallTimer],
    acks: SessionAcks[F],
    logger: StructuredLogger[F]
) extends Call[F] {

  private def newBranch(): F[Branch] = entropy.branch

  // Sending and answering within the dialog, which an incoming call does the
  // same way.
  private val wire = new DialogWire[F](
    layer,
    resolve,
    udpLimit,
    supervisor,
    logger,
    entropy.branch,
    uplink.get,
    serverTransactions,
    acks,
    {
      case DialogWire.Notice.Response(response) =>
        events.send(SipEvent.ResponseReceived(response)).void
      case DialogWire.Notice.Failed(failure) =>
        events.send(SipEvent.TransactionFailed(failure)).void
    },
    "the server we dialled"
  )
  import wire.*

  /** What a session change is made from: the codecs of the call's endpoint, or
    * just the one in use once media is running. They are the same over every
    * uplink.
    */
  private def inUse: F[Capabilities] =
    uplink.get.flatMap(u => media.restrict(u.endpoint.capabilities))

  private def hangupInterrupt: F[Either[Throwable, Unit]] =
    callEnded.get.map(Left(_))

  def source: Stream[F, Byte] =
    Stream.eval(callEnded.tryGet).flatMap {
      case Some(err) => Stream.raiseError[F](err)
      case None      => media.source.interruptWhen(hangupInterrupt)
    }
  def sink: Pipe[F, Byte, Unit]            = media.sink
  def awaitPickup: F[Unit]                 = answered.get.flatMap(_.liftTo[F])
  private[softphone4s] def hangup: F[Unit] = hangupSignal.complete(()).void
  def dtmfEvents: Stream[F, DtmfDigit]     =
    Stream.eval(callEnded.tryGet).flatMap {
      case Some(err) => Stream.raiseError[F](err)
      case None      => dtmfChannel.stream.interruptWhen(hangupInterrupt)
    }
  def awaitHangup: F[Unit] = callEnded.get.void

  /** Why the call ended, whichever side ended it: for its metrics. */
  private[softphone4s] def endedBecause: F[TerminationReason] = endReason.get

  /** What the jitter buffer has done over the call, for its metrics. */
  private[softphone4s] def jitterStats: F[JitterStats] = media.jitterStats

  def sendDtmf(digit: DtmfDigit, duration: FiniteDuration): F[DtmfMethod] =
    for {
      checked  <- Async[F].fromEither(Dtmf.checkedDuration(duration))
      pickedUp <- answered.tryGet
      _        <- Async[F].raiseUnless(pickedUp.exists(_.isRight))(
        SoftphoneError.CallNotEstablished
      )
      viaRtp <- media.sendDtmf(digit, checked)
      method <-
        if viaRtp then Async[F].pure(DtmfMethod.Rfc4733)
        else
          events
            .send(SipEvent.DtmfRequested(digit, checked))
            .as(DtmfMethod.SipInfo)
    } yield method

  // ---- The event loop ----------------------------------------------------------

  private[softphone4s] def runEventLoop(
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
          .as(Option(SipEvent.RequestReceived(transaction.request)))
      case DialogInput.Unmatched(inbound) =>
        inbound.message match {
          case response: SipResponse =>
            Async[F].pure(Some(SipEvent.ResponseReceived(response)))
          case request: SipRequest if request.method == SipMethod.Ack =>
            acks.acked(request).as(None)
          case _: SipRequest => Async[F].pure(None)
        }
    }
    fromDialog
      .merge(events.stream)
      .merge(Stream.eval(hangupSignal.get).as(SipEvent.HangupRequested))
      .merge(timers.fired.map(SipEvent.TimerFired(_)))
      .evalMap(processEvent)
      .takeWhile(identity)
      .compile
      .drain
  }

  /** Runs one event through the FSM. Returns false when the call is over and
    * the loop should stop. After a CANCEL the loop keeps going until the final
    * response to the INVITE arrives, since a 200 can cross the CANCEL and then
    * needs an ACK and a BYE (RFC 3261 §9.1).
    */
  private def processEvent(event: SipEvent): F[Boolean] =
    logIncoming(event) >>
      inUse.flatMap { caps =>
        stateRef
          .modify(state => CallFsm.transition(event, caps).run(state).value)
          .flatMap { actions =>
            for {
              _ <- actions.traverse_(interpretAction)
              _ <- cancelled.set(true).whenA(actions.exists(isCancel))
              awaitingFinal <- cancelled.get
            } yield {
              val ended = actions.exists {
                case NotifyHangup(_) => true; case _ => false
              }
              if awaitingFinal then !endsCancelledCall(event) else !ended
            }
          }
      }

  private def isCancel(action: SipAction): Boolean = action match {
    case SendCancel(_) => true
    case _             => false
  }

  /** After a CANCEL, the INVITE's final response (or the transaction failing)
    * is the last thing to wait for.
    */
  private def endsCancelledCall(event: SipEvent): Boolean = event match {
    case SipEvent.ResponseReceived(r) =>
      r.headers.cseq.method == SipMethod.Invite && r.statusCode >= 200
    case SipEvent.TransactionFailed(_) => true
    case _                             => false
  }

  private def logIncoming(event: SipEvent): F[Unit] = event match {
    case SipEvent.RequestReceived(req) =>
      logger.debug(
        Map(
          LogFields.Method -> req.method.token
        )
      )("Received SIP request")
    case SipEvent.ResponseReceived(res) =>
      logger.debug(
        Map(
          LogFields.Cseq   -> res.headers.cseq.method.toString,
          LogFields.Status -> res.statusCode.toString,
          LogFields.Reason -> res.reason
        )
      )("Received SIP response")
    case SipEvent.TransactionFailed(failure) =>
      logger.debug(Map("failure" -> failure.toString))("Transaction failed")
    case SipEvent.HangupRequested | SipEvent.TimerFired(_) |
        SipEvent.SessionChangeRequested(_) | SipEvent.DtmfRequested(_, _) =>
      Async[F].unit
  }

  // ---- Transactions ---------------------------------------------------------------

  /** Points media where the answer to our re-INVITE says, or stops it if the
    * remote party will not receive.
    */
  private def applyAnswer(response: SipResponse): F[Unit] =
    inUse
      .flatMap(caps =>
        SdpCodec
          .parseOffer(response.body, caps)
          .traverse_(o => media.retarget(o.ip, o.port, o.direction))
      )

  def changeSession(change: SessionChange): F[Unit] =
    events.send(SipEvent.SessionChangeRequested(change)).void

  /** Sends the call's first INVITE over the uplink the call has now. If it gets
    * no answer, the same INVITE goes out over the next uplink, with a new
    * branch and that uplink's Via and Contact.
    */
  private[ua] def sendInitialInvite(
      callee: SipUri,
      callId: CallId,
      localTag: Tag,
      extraHeaders: List[ExtensionHeader]
  ): F[Unit] =
    (newBranch(), uplink.get).flatMapN { (branch, current) =>
      def build(over: Uplink): SipRequest =
        over.endpoint.buildInvite(
          callee,
          callId,
          localTag,
          CSeqNumber.One,
          branch,
          extraHeaders = extraHeaders
        )
      val request = build(current)
      for {
        sendOver <- streamIfTooBig(current, request)
        _        <- inviteBranch.set(branch)
        _        <- startClient(
          if sendOver == current then request else build(sendOver),
          sendOver.peer,
          failover = nextUplink(
            sendInitialInvite(callee, callId, localTag, extraHeaders)
          )
        )
      } yield ()
    }

  /** The uplink to send `request` over: `current`, unless that is UDP and the
    * request is too big for a datagram, in which case the stream way to the
    * same server, if there is one (RFC 3261 §18.1.1). The UDP way then becomes
    * the next to try, so a connection that cannot be made falls back to UDP.
    * Only once: after that fall-back the request goes over UDP whatever its
    * size.
    */
  private def streamIfTooBig(current: Uplink, request: SipRequest): F[Uplink] =
    if current.endpoint.transport != TransportProtocol.Udp ||
      SipCodec.encode(request).size <= udpLimit
    then Async[F].pure(current)
    else
      oversize.getAndSet(None).flatMap {
        case None         => Async[F].pure(current)
        case Some(stream) =>
          alternatives.update(current :: _) >>
            uplink.set(stream) >>
            logger
              .info(
                Map(
                  "bytes" -> SipCodec.encode(request).size.toString,
                  "limit" -> udpLimit.toString
                )
              )("INVITE too big for UDP; sending it over TCP")
              .as(stream)
      }

  /** Moves the call to its next uplink and runs `retry` over it. False, doing
    * nothing, when there is none, or the call is no longer just inviting (it
    * was answered, or is being hung up).
    */
  private def nextUplink(retry: F[Unit]): F[Boolean] =
    (stateRef.get, cancelled.get).flatMapN { (state, hangingUp) =>
      val inviting = state match {
        case CallState.Inviting(_) => true
        case _                     => false
      }
      if !inviting || hangingUp then Async[F].pure(false)
      else
        alternatives
          .modify {
            case next :: rest => (rest, Some(next))
            case Nil          => (Nil, None)
          }
          .flatMap {
            case None       => Async[F].pure(false)
            case Some(next) =>
              uplink.set(next) >>
                logger.info(
                  Map(
                    "to"                -> next.peer.toString,
                    LogFields.Transport -> next.endpoint.transport.token
                  )
                )("No answer; trying the next server") >>
                retry.as(true)
          }
    }

  // ---- Actions -------------------------------------------------------------------

  private def interpretAction(action: SipAction): F[Unit] =
    uplink.get.flatMap(current =>
      interpret(current.endpoint, current.peer, action)
    )

  /** Carries out `action` over the uplink the call has at the moment. */
  private def interpret(
      endpoint: LocalEndpoint,
      peer: SocketAddress[IpAddress],
      action: SipAction
  ): F[Unit] =
    action match {
      case SendInvite(callee, callId, localTag, cseq, authHeader) =>
        newBranch().flatMap { branch =>
          inviteBranch.set(branch) >>
            logger.debug(
              Map("callee" -> callee.render)
            )("Sending INVITE") >>
            startClient(
              endpoint.buildInvite(
                callee,
                callId,
                localTag,
                cseq,
                branch,
                authHeader
              ),
              peer
            )
        }

      case RetryInviteWithAuth(ctx, kind, challenge) =>
        for {
          cnonce     <- entropy.cnonce
          nonceCount <- nonceCounts.modify { m =>
            val next = m.get(challenge.nonce).fold(NonceCount.One)(_.next)
            (m.updated(challenge.nonce, next), next)
          }
          authHeader = Some(
            endpoint.authorizationFor(
              kind,
              SipMethod.Invite,
              ctx.callee,
              endpoint.offerBody,
              challenge,
              cnonce,
              nonceCount
            )
          )
          branch <- newBranch()
          _      <- inviteBranch.set(branch)
          _      <- logger.debug(
            Map(
              LogFields.Cseq -> ctx.cseq.value.toString,
              "nonceCount"   -> nonceCount.render
            )
          )("Retrying INVITE with authentication")
          _ <- startClient(
            endpoint.buildInvite(
              ctx.callee,
              ctx.callId,
              ctx.localTag,
              ctx.cseq,
              branch,
              authHeader
            ),
            peer
          )
        } yield ()

      // The ACK of a 2xx is not part of a transaction (RFC 3261 §13.2.2.4).
      case SendAck(dialog, remoteSdpForAnswer) =>
        (newBranch(), destinationFor(dialog)).flatMapN { (branch, to) =>
          logger.debug("Sending ACK") >>
            transmitInDialog(
              endpoint.buildAck(
                dialog,
                branch,
                remoteSdpForAnswer.map(endpoint.buildSdpAnswer)
              ),
              dialog,
              to
            )
        }

      case SendCancel(ctx) =>
        inviteBranch.get.flatMap { branch =>
          logger.debug("Sending CANCEL") >> startClient(
            endpoint.buildCancel(ctx, branch),
            peer
          )
        }

      case SendBye(dialog) =>
        (newBranch(), destinationFor(dialog)).flatMapN { (branch, to) =>
          logger.debug("Sending BYE") >> startInDialog(
            endpoint.buildBye(dialog, branch),
            dialog,
            to
          )
        }

      case SendInfo(dialog, digit, duration) =>
        (newBranch(), destinationFor(dialog)).flatMapN { (branch, to) =>
          logger.debug(Map("digit" -> digit.symbol.toString))(
            "Sending a digit in INFO"
          ) >> startInDialog(
            endpoint.buildInfo(dialog, branch, digit, duration),
            dialog,
            to
          )
        }

      case SendReply(request, status) =>
        respondTo(
          request,
          TransactionMessages.responseTo(request, status.value, status.reason)
        )

      case SendByeOk(request) =>
        logger.debug("Acknowledging BYE") >>
          respondTo(request, endpoint.buildByeOk(request))

      case SendInfoOk(request) =>
        logger.debug("Acknowledging INFO") >>
          respondTo(request, endpoint.buildInfoOk(request))

      case SendReinvite(dialog, change) =>
        (
          newBranch(),
          destinationFor(dialog),
          media.restrict(endpoint.capabilities)
        )
          .flatMapN { (branch, to, caps) =>
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
                .buildAnswerTo(_, endpoint.mediaIp, endpoint.mediaPort, caps)
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
                SdpCodec.buildOffer(endpoint.mediaIp, endpoint.mediaPort, caps)
              ),
              AckContent.Answer
            )
        }

      case StartMedia(sdpBody) =>
        SdpCodec.planFromAnswer(
          sdpBody,
          endpoint.capabilities,
          MediaDirection.SendRecv
        ) match {
          case Right(plan) =>
            media.start(plan, supervisor, dtmfChannel.send(_).void).flatMap {
              case MediaStart.CodecChanged =>
                logger.warn(Map("codec" -> plan.codec.encoding))(
                  "The answer chose another codec than the early media's; keeping the early one"
                )
              case MediaStart.Started | MediaStart.Followed => Async[F].unit
            }
          case Left(reject) =>
            // An answer we cannot use: no media can flow, so the call goes.
            logger.warn(Map(LogFields.Reason -> reject.message))(
              "The answer cannot be used; hanging up"
            ) >> hangup
        }

      case StopMedia =>
        media.stop()

      case NotifyAnswered =>
        answered.complete(Right(())).void

      case NotifyHangup(reason) =>
        val err: Throwable = reason match {
          case Rejected(code, msg) => SoftphoneError.CallRejected(code, msg)
          case TimedOut            => SoftphoneError.CallTimedOut
          case TransportFailure    => SoftphoneError.TransportFailed
          case CancelledByCaller | RejectedLocally =>
            SoftphoneError.CallCancelled
          case LocalHangup => SoftphoneError.CallCancelled
          case RemoteBye   => SoftphoneError.RemoteHangup
        }
        val signalStreams = reason match {
          case LocalHangup => Async[F].unit
          case _           => callEnded.complete(err).void
        }
        logger.debug(Map(LogFields.Reason -> reason.toString))("Call ended") >>
          // Early media may have started it, and a call that never got to be
          // answered would otherwise leave it running.
          media.stop() >>
          signalStreams >>
          dtmfChannel.close.void >>
          answered.complete(Left(err)).void >>
          endReason.complete(reason).void

      case NotifyDtmf(digit) =>
        dtmfChannel.send(digit).void

      case Timer(action) =>
        timers.run(action)
    }
}

private[softphone4s] object OutboundCall {

  /** Starts the call by sending its INVITE over `first` through `layer`. If
    * that gets no answer, or the transport fails, the INVITE is sent again over
    * each of `alternatives` in turn (RFC 3263 §4.3). `inviteTimeout` is how
    * long to wait for the whole call to be answered, so it should cover every
    * target. An INVITE of more than `udpLimit` bytes that would go over UDP
    * goes over `oversize` instead, when there is one.
    */
  def create[F[_]: Async](
      callId: CallId,
      callee: SipUri,
      layer: TransactionLayer[F],
      first: Uplink,
      alternatives: List[Uplink],
      oversize: Option[Uplink],
      udpLimit: Int,
      resolve: (TransportProtocol, SipUri) => F[SocketAddress[IpAddress]],
      supervisor: Supervisor[F],
      entropy: Entropy[F],
      media: MediaSession[F],
      timers: Timers[F, CallTimer],
      inviteTimeout: FiniteDuration,
      phoneLogger: StructuredLogger[F],
      extraHeaders: List[ExtensionHeader] = Nil
  ): F[OutboundCall[F]] =
    for {
      localTag <- entropy.tag
      // Every line the call logs says which call it is.
      logger = phoneLogger.addContext(
        Map(
          LogFields.CallId    -> callId.value,
          LogFields.Direction -> LogFields.direction(CallDirection.Outgoing),
          LogFields.Account   -> first.endpoint.accountUri.render
        )
      )
      branch       <- entropy.branch
      inviteBranch <- Ref[F].of(branch)
      uplink       <- Ref[F].of(first)
      remaining    <- Ref[F].of(alternatives)
      streamWay    <- Ref[F].of(oversize)
      ctx = InviteCtx(
        callId = callId,
        localTag = localTag,
        cseq = CSeqNumber.One,
        callee = callee,
        localUri = first.endpoint.accountUri,
        offerInInvite = true,
        inviteTimeout = inviteTimeout
      )
      answered           <- Deferred[F, Either[Throwable, Unit]]
      callEnded          <- Deferred[F, Throwable]
      endReason          <- Deferred[F, TerminationReason]
      dtmfChannel        <- Channel.bounded[F, DtmfDigit](64)
      hangupSignal       <- Deferred[F, Unit]
      stateRef           <- Ref[F].of[CallState](CallState.Inviting(ctx))
      nonceCounts        <- Ref[F].of(Map.empty[Nonce, NonceCount])
      events             <- Channel.unbounded[F, SipEvent]
      serverTransactions <- Ref[F].of(
        Map.empty[(SipMethod, CSeqNumber), ServerTransaction[F]]
      )
      cancelled <- Ref[F].of(false)

      acks <- SessionAcks.create(
        supervisor,
        TransactionConfig(),
        (response, to) => layer.transmit(response, to),
        offer => media.retarget(offer.ip, offer.port, offer.direction),
        media.restrict(first.endpoint.capabilities),
        logger
      )
      call = new OutboundCall(
        answered,
        callEnded,
        endReason,
        dtmfChannel,
        hangupSignal,
        stateRef,
        layer,
        uplink,
        remaining,
        streamWay,
        udpLimit,
        resolve,
        events,
        serverTransactions,
        cancelled,
        supervisor,
        inviteBranch,
        nonceCounts,
        entropy,
        media,
        timers,
        acks,
        logger
      )
      _ <- timers.start(CallTimer.InviteTimeout, inviteTimeout)
      _ <- logger.debug(
        Map("callee" -> callee.render)
      )("Sending INVITE")
      _ <- call.sendInitialInvite(callee, callId, localTag, extraHeaders)
    } yield call
}
