package softphone4s

import cats.data.NonEmptyList
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
  ServerTransaction,
  TransactionConfig,
  TransactionLayer,
  TransactionManager,
  TransactionMessages
}
import softphone4s.transport.{
  Inbound,
  RoutedTransport,
  RtpTransport,
  SipTransport,
  TcpTransport,
  TlsTransport
}
import softphone4s.ua.{DialogInput, InboundCall, MediaSession, OutboundCall}

import javax.sound.sampled.AudioFormat

/** fs2/Cats Effect `Softphone` backed by a single shared UDP socket, routing
  * inbound SIP messages to the matching call by Call-ID. Construct via
  * [[SoftphoneStreaming.resource]].
  */
class SoftphoneStreaming[F[_]: Async: Network] private (
    config: SoftphoneConfig,
    accountList: NonEmptyList[Account],
    layer: TransactionLayer[F],
    routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
    nextCallIndex: Ref[F, Int],
    entropy: Entropy[F],
    pcmFrameBytes: Int,
    supervisor: Supervisor[F],
    incomingCalls: Queue[F, IncomingCall[F]],
    logger: StructuredLogger[F]
) extends Softphone[F] {

  override val accounts: NonEmptyList[AccountId] = accountList.map(_.id)

  private def accountNamed(id: AccountId): F[Account] =
    Async[F].fromOption(
      accountList.find(_.id == id),
      SoftphoneError.UnknownAccount(id.render)
    )

  /** The account an incoming request is for: the one whose user is in the
    * Request-URI. With a single account every call is for it, whatever it
    * names.
    */
  private def accountFor(request: SipRequest): Option[Account] =
    if accountList.tail.isEmpty then Some(accountList.head)
    else
      request.requestUri.user.flatMap(user =>
        accountList.find(_.config.user == user.value)
      )

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

  override def callAs(
      id: AccountId,
      callee: Callee,
      extraHeaders: Map[String, List[String]] = Map.empty
  ): Resource[F, Call[F]] =
    for {
      account <- Resource.eval(accountNamed(id))
      extra   <- Resource.eval(
        Async[F].fromEither(toExtensionHeaders(extraHeaders))
      )
      target <- Resource.eval(
        Async[F].fromEither(callee.toSipUri(account.config.realm))
      )
      protocol   <- Resource.eval(transportFor(account, target))
      calleeAddr <- Resource.eval(resolveCalleeAddress(account, target))
      localIp    <- Resource.eval(
        localIpFor(calleeAddr).flatTap(ip =>
          logger.info(Map("address" -> ip.toString))("Advertising local IP")
        )
      )
      endpoint <- Resource.eval(allocateEndpoint(account, localIp, protocol))
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

  override def registerAs(id: AccountId): Resource[F, Registration[F]] =
    for {
      account <- Resource.eval(accountNamed(id))
      host    <- Resource.eval(
        Async[F].fromOption(
          Host.fromString(account.config.realm),
          SoftphoneError.InvalidSipUri(account.config.realm, "not a valid host")
        )
      )
      registrarUri <- Resource.eval(
        Async[F].fromEither(
          SipUri
            .from(SipScheme.Sip, host)
            .leftMap(SoftphoneError.InvalidSipUri(account.config.realm, _))
        )
      )
      registrar <- Resource.eval(
        resolveHost(host).map(SocketAddress(_, account.config.serverPort))
      )
      protocol <- Resource.eval(available(account.config.transport))
      localIp  <- Resource.eval(localIpFor(registrar))
      // A REGISTER carries no media, so the RTP port is not used.
      endpoint <- Resource.eval(
        Async[F].fromEither(
          LocalEndpoint.from(
            account.config,
            config.bindAddress,
            localIp,
            config.localSipPort,
            config.baseRtpPort,
            protocol
          )
        )
      )
      registration <- Registration.resource(
        layer,
        registrar,
        registrarUri,
        endpoint,
        entropy,
        Expires.of(account.config.registerExpires),
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
      account: Account,
      target: SipUri
  ): F[SocketAddress[IpAddress]] =
    resolveHost(target.host).map(ip =>
      SocketAddress(ip, target.port.getOrElse(account.config.serverPort))
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

  /** The transport a call to `target` as `account` uses: the one the URI names
    * (`;transport=`), else the account's.
    */
  private def transportFor(
      account: Account,
      target: SipUri
  ): F[TransportProtocol] =
    available(
      target.transport.getOrElse(
        // A sips: URI asks for TLS all the way (RFC 3261 §26.2.2).
        if target.scheme == SipScheme.Sips then TransportProtocol.Tls
        else account.config.transport
      )
    )

  /** `transport` itself, if we can speak it. */
  private def available(transport: TransportProtocol): F[TransportProtocol] =
    transport match {
      case TransportProtocol.Udp | TransportProtocol.Tcp |
          TransportProtocol.Tls =>
        transport.pure[F]
      case other =>
        Async[F].raiseError(SoftphoneError.TransportUnavailable(other.token))
    }

  private def allocateEndpoint(
      account: Account,
      localIp: IpAddress,
      transport: TransportProtocol
  ): F[LocalEndpoint] =
    nextCallIndex.getAndUpdate(_ + 1).flatMap { callIndex =>
      val portsPerCall = 2
      val rtpPortInt   = config.baseRtpPort.value + callIndex * portsPerCall
      for {
        rtpPort <- Async[F].fromOption(
          Port.fromInt(rtpPortInt),
          SoftphoneError.RtpPortExhausted(rtpPortInt)
        )
        endpoint <- Async[F].fromEither(
          LocalEndpoint.from(
            account.config,
            config.bindAddress,
            localIp,
            config.localSipPort,
            rtpPort,
            transport
          )
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
      case None if isNewInvite(request) =>
        accountFor(request).fold(
          transaction.respond(
            TransactionMessages.responseTo(
              request,
              FailureStatus.NotFound.value,
              FailureStatus.NotFound.reason
            )
          )
        )(acceptInbound(transaction, _))
      case None =>
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
  private def acceptInbound(
      invite: ServerTransaction[F],
      account: Account
  ): F[Unit] = {
    val callId = invite.request.headers.callId
    Channel.bounded[F, DialogInput[F]](128).flatMap { channel =>
      routes.update(_ + (callId -> channel)) >>
        supervisor
          .supervise(
            inboundCall(invite, account, channel)
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
      account: Account,
      channel: Channel[F, DialogInput[F]]
  ): F[Unit] =
    (for {
      callSupervisor <- Supervisor[F](await = false)
      timers         <- Timers.resource[F, CallTimer]
      localIp        <- Resource.eval(localIpFor(invite.from))
      endpoint       <- Resource.eval(
        allocateEndpoint(
          account,
          localIp,
          invite.request.headers.topVia.transport
        )
      )
      media <- Resource.eval(
        MediaSession
          .create(pcmFrameBytes, endpoint.bindAddress, endpoint.rtpPort)
      )
      call <- Resource.eval(
        InboundCall.create(
          invite,
          account.id,
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
      udp <- SipTransport.udp[F](
        SocketAddress(config.bindAddress, config.localSipPort),
        logger
      )
      tcp <- TcpTransport
        .resource[F](config.bindAddress, config.tcp, logger)
      tls <- TlsTransport.resource[F](config.bindAddress, config.tls, logger)
      transport = RoutedTransport(udp, tcp, tls)
      layer <- TransactionManager.resource[F](
        transport,
        TransactionConfig(),
        logger
      )
      routes <- Resource.eval(
        Ref[F].of(Map.empty[CallId, Channel[F, DialogInput[F]]])
      )
      accounts <- Resource.eval(
        Async[F].fromEither(Account.all(config.accounts))
      )
      supervisor    <- Supervisor[F](await = false)
      incomingCalls <- Resource.eval(Queue.unbounded[F, IncomingCall[F]])
      callIndex     <- Resource.eval(Ref[F].of(0))
      entropy       <- Resource.eval(Entropy.secure[F])
      softphone = new SoftphoneStreaming(
        config,
        accounts,
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
