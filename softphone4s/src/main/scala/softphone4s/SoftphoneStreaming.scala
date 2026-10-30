package softphone4s

import cats.effect.*
import cats.effect.std.Supervisor
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
import softphone4s.ua.{DialogInput, MediaSession, OutboundCall}

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
}

object SoftphoneStreaming {

  /** Binds the local SIP UDP socket and returns a ready-to-use `Softphone`. The
    * socket and its message router are released when the resource is.
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
      supervisor <- Supervisor[F](await = false)
      _          <- Resource.eval(
        supervisor.supervise(routeIncoming(layer, routes, logger))
      )
      _ <- Resource.eval(
        supervisor.supervise(routeUnmatched(layer, routes, logger))
      )
      callIndex <- Resource.eval(Ref[F].of(0))
      entropy   <- Resource.eval(Entropy.secure[F])
    } yield new SoftphoneStreaming(
      config,
      layer,
      routes,
      callIndex,
      entropy,
      pcmFrameBytes = audioFormat.getFrameSize * RtpTransport.PacketSamples,
      logger
    )

  /** Hands requests within a dialog to their call; a call that is gone, or
    * never was, gets `481 Call/Transaction Does Not Exist` (RFC 3261 §12.2.2).
    */
  private def routeIncoming[F[_]: Async](
      layer: TransactionLayer[F],
      routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
      logger: StructuredLogger[F]
  ): F[Unit] =
    layer.incoming
      .evalMap { (transaction: ServerTransaction[F]) =>
        routes.get.flatMap(_.get(transaction.request.headers.callId) match {
          case Some(channel) =>
            channel.send(DialogInput.Request(transaction)).void
          case None =>
            logger.debug(
              Map("callId" -> transaction.request.headers.callId.value)
            )("Request for a call that does not exist") >>
              transaction.respond(
                TransactionMessages.responseTo(
                  transaction.request,
                  481,
                  "Call/Transaction Does Not Exist"
                )
              )
        })
      }
      .compile
      .drain

  /** Hands messages no transaction claimed (a retransmitted 2xx) to their call.
    */
  private def routeUnmatched[F[_]: Async](
      layer: TransactionLayer[F],
      routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
      logger: StructuredLogger[F]
  ): F[Unit] =
    layer.unmatched
      .evalMap { (inbound: Inbound) =>
        routes.get.flatMap(_.get(inbound.message.headers.callId) match {
          case Some(channel) =>
            channel.send(DialogInput.Unmatched(inbound)).void
          case None =>
            logger.debug(
              Map("callId" -> inbound.message.headers.callId.value)
            )("Dropping a message for a call that does not exist")
        })
      }
      .compile
      .drain
}
