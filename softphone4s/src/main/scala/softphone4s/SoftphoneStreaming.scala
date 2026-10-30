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
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transport.{RtpTransport, SipTransport, Transport}
import softphone4s.ua.{MediaSession, OutboundCall}

import javax.sound.sampled.AudioFormat

/** fs2/Cats Effect `Softphone` backed by a single shared UDP socket, routing
  * inbound SIP messages to the matching call by Call-ID. Construct via
  * [[SoftphoneStreaming.resource]].
  */
class SoftphoneStreaming[F[_]: Async: Network] private (
    config: SoftphoneConfig,
    transport: Transport[F],
    routes: Ref[F, Map[CallId, Channel[F, SipMessage]]],
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
          .bounded[F, SipMessage](128)
          .flatTap(ch => routes.update(_ + (callId -> ch)))
      )(_ => routes.update(_ - callId))
      sipIn   = sipCh.stream
      sendSip = (msg: SipMessage) => transport.send(calleeAddr, msg)
      call <- buildCall(
        callId,
        target,
        sipIn,
        sendSip,
        endpoint,
        extra
      )
    } yield call

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
      sipIn: Stream[F, SipMessage],
      sendSip: SipMessage => F[Unit],
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
          sendSip,
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
      _ <- Resource.make(call.runEventLoop(sipIn).start)(fiber =>
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
      routes <- Resource.eval(
        Ref[F].of(Map.empty[CallId, Channel[F, SipMessage]])
      )
      _ <- Resource.make(
        transport.messages
          .map(_.message)
          .evalMap { msg =>
            routes.get.flatMap {
              _.get(msg.headers.callId) match {
                case Some(ch) => ch.send(msg).void
                case None     =>
                  logger.warn(Map("callId" -> msg.headers.callId.value))(
                    "Dropping SIP message: no active call for this Call-ID"
                  )
              }
            }
          }
          .compile
          .drain
          .start
      )(_.cancel)
      callIndex <- Resource.eval(Ref[F].of(0))
      entropy   <- Resource.eval(Entropy.secure[F])
    } yield new SoftphoneStreaming(
      config,
      transport,
      routes,
      callIndex,
      entropy,
      pcmFrameBytes = audioFormat.getFrameSize * RtpTransport.PacketSamples,
      logger
    )
}
