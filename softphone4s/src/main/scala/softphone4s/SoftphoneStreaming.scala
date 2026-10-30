package softphone4s

import cats.effect.*
import cats.effect.std.{Queue, Supervisor}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.concurrent.Channel
import fs2.io.net.Network
import org.typelevel.log4cats.StructuredLogger
import softphone4s.config.SoftphoneConfig
import softphone4s.fsm.CallTimer
import softphone4s.model.*
import softphone4s.registration.Registration
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transaction.{
  Reliability,
  ServerTransaction,
  TransactionConfig,
  TransactionLayer,
  TransactionManager,
  TransactionMessages
}
import softphone4s.transport.{Inbound, RtpTransport, SipTransport}
import softphone4s.ua.{DialogInput, InboundCall, MediaSession, OutboundCall}

import javax.sound.sampled.AudioFormat

/** fs2/Cats Effect `Softphone` backed by a single shared UDP socket, routing
  * inbound SIP messages to the matching call by Call-ID. Construct via
  * [[SoftphoneStreaming.resource]].
  */
class SoftphoneStreaming[F[_]: Async: Network] private (
    config: SoftphoneConfig,
    layer: TransactionLayer[F],
    routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
    nextCallIndex: Ref[F, Int],
    entropy: Entropy[F],
    pcmFrameBytes: Int,
    supervisor: Supervisor[F],
    incomingCalls: Queue[F, IncomingCall[F]],
    logger: StructuredLogger[F]
) extends Softphone[F] {

  private def toExtensionHeaders(
      headers: Map[String, List[String]]
  ): Either[SoftphoneError, List[ExtensionHeader]] =
    headers.toList.flatTraverse { (name, values) =>
      FieldName(name)
        .leftMap(SoftphoneError.InvalidHeader(name, _))
        .map(n => values.map(ExtensionHeader(n, _)))
    }

  private def resolveHost(host: Host): F[IpAddress] =
    host.resolve[F](using Dns.forAsync[F], Async[F]).adaptError { case _ =>
      SoftphoneError.HostResolutionFailed(host.toString)
    }

  override def call(
      callee: Callee,
      extraHeaders: Map[String, List[String]] = Map.empty
  ): Resource[F, Call[F]] =
    for {
      extra <- Resource.eval(
        Async[F].fromEither(toExtensionHeaders(extraHeaders))
      )
      target <- Resource.eval(
        Async[F].fromEither(callee.toSipUri(config.realm))
      )
      calleeAddr <- Resource.eval(resolveCalleeAddress(target))
      localIp    <- Resource.eval(
        localIpFor(calleeAddr).flatTap(ip =>
          logger.info(Map("address" -> ip.toString))("Advertising local IP")
        )
      )
      endpoint <- Resource.eval(allocateEndpoint(localIp))
      callId   <- Resource.eval(entropy.callId(localIp))
      sipCh    <- Resource.make(
        Channel
          .bounded[F, DialogInput[F]](128)
          .flatTap(ch => routes.update(_ + (callId -> ch)))
      )(_ => routes.update(_ - callId))
      call <- buildCall(
        callId,
        target,
        sipCh.stream,
        calleeAddr,
        endpoint,
        extra
      )
    } yield call

  override def register: Resource[F, Registration[F]] =
    for {
      host <- Resource.eval(
        Async[F].fromOption(
          Host.fromString(config.realm),
          SoftphoneError.InvalidSipUri(config.realm, "not a valid host")
        )
      )
      registrarUri <- Resource.eval(
        Async[F].fromEither(
          SipUri
            .from(SipScheme.Sip, host)
            .leftMap(SoftphoneError.InvalidSipUri(config.realm, _))
        )
      )
      registrar <- Resource.eval(
        resolveHost(host).map(SocketAddress(_, config.serverPort))
      )
      localIp <- Resource.eval(localIpFor(registrar))
      // A REGISTER carries no media, so the RTP port is not used.
      endpoint <- Resource.eval(
        Async[F].fromEither(
          LocalEndpoint.from(
            config,
            localIp,
            config.localSipPort,
            config.baseRtpPort
          )
        )
      )
      registration <- Registration.resource(
        layer,
        registrar,
        registrarUri,
        endpoint,
        entropy,
        Expires.of(config.registerExpires),
        config.hangupTimeout,
        logger
      )
    } yield registration

  /** The address of a dialog's next hop: its host (resolved) and port, or the
    * default for its scheme.
    */
  private def resolveNextHop(uri: SipUri): F[SocketAddress[IpAddress]] = {
    val defaultPort = uri.scheme match {
      case SipScheme.Sip  => port"5060"
      case SipScheme.Sips => port"5061"
    }
    resolveHost(uri.host).map(SocketAddress(_, uri.port.getOrElse(defaultPort)))
  }

  private def resolveCalleeAddress(
      target: SipUri
  ): F[SocketAddress[IpAddress]] =
    resolveHost(target.host).map(ip =>
      SocketAddress(ip, target.port.getOrElse(config.serverPort))
    )

  /** The address the OS would send from to reach `remote`: what the SDP and
    * Contact must advertise for replies to come back. Connecting a UDP socket
    * only picks the route; nothing is sent. (The first network interface would
    * not do: on macOS it can be Docker Desktop's bridge, 192.168.65.1.) The SDP
    * advertises IN IP4, so anything but IPv4 falls back.
    */
  private def localIpFor(remote: SocketAddress[IpAddress]): F[IpAddress] =
    Resource
      .fromAutoCloseable(Async[F].blocking(new java.net.DatagramSocket()))
      .use(socket =>
        Async[F].blocking {
          socket.connect(remote.toInetSocketAddress)
          socket.getLocalAddress
        }
      )
      .map(address =>
        Option(address)
          .filterNot(_.isAnyLocalAddress)
          .map(IpAddress.fromInetAddress)
          .collect { case v4: Ipv4Address => v4: IpAddress }
          .getOrElse(config.localIpFallback)
      )
      .handleError(_ => config.localIpFallback)

  private def allocateEndpoint(localIp: IpAddress): F[LocalEndpoint] =
    nextCallIndex.getAndUpdate(_ + 1).flatMap { callIndex =>
      val portsPerCall = 2
      val rtpPortInt   = config.baseRtpPort.value + callIndex * portsPerCall
      for {
        rtpPort <- Async[F].fromOption(
          Port.fromInt(rtpPortInt),
          SoftphoneError.RtpPortExhausted(rtpPortInt)
        )
        endpoint <- Async[F].fromEither(
          LocalEndpoint.from(config, localIp, config.localSipPort, rtpPort)
        )
      } yield endpoint
    }

  private def buildCall(
      callId: CallId,
      callee: SipUri,
      dialogInput: Stream[F, DialogInput[F]],
      peer: SocketAddress[IpAddress],
      endpoint: LocalEndpoint,
      extraHeaders: List[ExtensionHeader]
  ): Resource[F, OutboundCall[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      timers     <- Timers.resource[F, CallTimer]
      media      <- Resource.eval(
        MediaSession.create(
          pcmFrameBytes,
          endpoint.bindAddress,
          endpoint.rtpPort
        )
      )
      call <- Resource.eval(
        OutboundCall.create(
          callId,
          callee,
          layer,
          peer,
          resolveNextHop,
          endpoint,
          supervisor,
          entropy,
          media,
          timers,
          config.inviteTimeout,
          logger,
          extraHeaders
        )
      )
      _ <- Resource.make(call.runEventLoop(dialogInput).start)(fiber =>
        call.hangup >> fiber.join.void
          .timeoutTo(config.hangupTimeout, fiber.cancel)
      )
    } yield call

  // ---- Incoming calls ----------------------------------------------------------

  override def incoming: Stream[F, IncomingCall[F]] =
    Stream.fromQueueUnterminated(incomingCalls)

  /** Hands each request to the call it belongs to. An INVITE with no To-tag and
    * a new Call-ID starts an incoming call; anything else for a call that is
    * gone, or never was, gets `481 Call/Transaction Does Not Exist` (RFC 3261
    * §12.2.2), and a new INVITE reusing a live Call-ID gets
    * `482 Loop Detected`.
    */
  private[softphone4s] def routeIncoming: F[Unit] =
    layer.incoming.evalMap(route).compile.drain

  private def isNewInvite(request: SipRequest): Boolean =
    request.method == SipMethod.Invite && request.headers.to.tag.isEmpty

  private def route(transaction: ServerTransaction[F]): F[Unit] = {
    val request = transaction.request
    routes.get.flatMap(_.get(request.headers.callId) match {
      case Some(_) if isNewInvite(request) =>
        transaction.respond(
          TransactionMessages.responseTo(request, 482, "Loop Detected")
        )
      case Some(channel) => channel.send(DialogInput.Request(transaction)).void
      case None if isNewInvite(request) => acceptInbound(transaction)
      case None                         =>
        logger.debug(Map("callId" -> request.headers.callId.value))(
          "Request for a call that does not exist"
        ) >>
          transaction.respond(
            TransactionMessages.responseTo(
              request,
              FailureStatus.CallDoesNotExist.value,
              FailureStatus.CallDoesNotExist.reason
            )
          )
    })
  }

  /** Hands messages no transaction claimed (the ACK of a 2xx, a retransmitted
    * 2xx) to their call.
    */
  private[softphone4s] def routeUnmatched: F[Unit] =
    layer.unmatched
      .evalMap { (inbound: Inbound) =>
        routes.get.flatMap(_.get(inbound.message.headers.callId) match {
          case Some(channel) =>
            channel.send(DialogInput.Unmatched(inbound)).void
          case None =>
            logger.debug(Map("callId" -> inbound.message.headers.callId.value))(
              "Dropping a message for a call that does not exist"
            )
        })
      }
      .compile
      .drain

  /** Registers the route first, so a CANCEL or ACK right behind the INVITE
    * finds its call, then runs the call on its own fiber for as long as it
    * lasts.
    */
  private def acceptInbound(invite: ServerTransaction[F]): F[Unit] = {
    val callId = invite.request.headers.callId
    Channel.bounded[F, DialogInput[F]](128).flatMap { channel =>
      routes.update(_ + (callId -> channel)) >>
        supervisor
          .supervise(
            inboundCall(invite, channel)
              .handleErrorWith { error =>
                logger.error(Map("error" -> String.valueOf(error.getMessage)))(
                  "Could not take the incoming call"
                ) >> invite.respond(
                  TransactionMessages.responseTo(
                    invite.request,
                    FailureStatus.ServerInternalError.value,
                    FailureStatus.ServerInternalError.reason
                  )
                )
              }
              .guarantee(routes.update(_ - callId))
          )
          .void
    }
  }

  /** Builds the call's resources, offers the call to the application if it can
    * be answered, and runs it to its end.
    */
  private def inboundCall(
      invite: ServerTransaction[F],
      channel: Channel[F, DialogInput[F]]
  ): F[Unit] =
    (for {
      callSupervisor <- Supervisor[F](await = false)
      timers         <- Timers.resource[F, CallTimer]
      localIp        <- Resource.eval(localIpFor(invite.from))
      endpoint       <- Resource.eval(allocateEndpoint(localIp))
      media          <- Resource.eval(
        MediaSession
          .create(pcmFrameBytes, endpoint.bindAddress, endpoint.rtpPort)
      )
      call <- Resource.eval(
        InboundCall.create(
          invite,
          layer,
          resolveNextHop,
          endpoint,
          callSupervisor,
          entropy,
          media,
          timers,
          TransactionConfig(),
          config.answerTimeout,
          config.hangupTimeout,
          logger
        )
      )
    } yield call).use { call =>
      call.isOpen.flatMap(incomingCalls.offer(call).whenA(_)) >>
        call.run(channel.stream)
    }
}

object SoftphoneStreaming {

  /** Binds the local SIP UDP socket and returns a ready-to-use `Softphone`. The
    * socket, the transaction layer and the routers that hand messages to calls
    * are released when the resource is.
    */
  def resource[F[_]: Async: Network](
      config: SoftphoneConfig,
      audioFormat: AudioFormat,
      logger: StructuredLogger[F]
  ): Resource[F, SoftphoneStreaming[F]] =
    for {
      transport <- SipTransport.udp[F](
        SocketAddress(config.bindAddress, config.localSipPort),
        logger
      )
      layer <- TransactionManager.resource[F](
        transport,
        Reliability.Unreliable,
        TransactionConfig(),
        logger
      )
      routes <- Resource.eval(
        Ref[F].of(Map.empty[CallId, Channel[F, DialogInput[F]]])
      )
      supervisor    <- Supervisor[F](await = false)
      incomingCalls <- Resource.eval(Queue.unbounded[F, IncomingCall[F]])
      callIndex     <- Resource.eval(Ref[F].of(0))
      entropy       <- Resource.eval(Entropy.secure[F])
      softphone = new SoftphoneStreaming(
        config,
        layer,
        routes,
        callIndex,
        entropy,
        pcmFrameBytes = audioFormat.getFrameSize * RtpTransport.PacketSamples,
        supervisor,
        incomingCalls,
        logger
      )
      _ <- Resource.eval(supervisor.supervise(softphone.routeIncoming))
      _ <- Resource.eval(supervisor.supervise(softphone.routeUnmatched))
    } yield softphone
}
