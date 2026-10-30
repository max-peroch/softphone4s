package softphone4s

import cats.effect.*
import cats.effect.std.{Random, SecureRandom, Supervisor}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.concurrent.Channel
import fs2.io.net.Network
import org.typelevel.log4cats.StructuredLogger
import softphone4s.config.SoftphoneConfig
import softphone4s.fsm.*
import softphone4s.model.*
import softphone4s.transport.{RtpTransport, SipTransport}
import softphone4s.ua.{MediaSession, OutboundCall}

import javax.sound.sampled.AudioFormat

/** fs2/Cats Effect `Softphone` backed by a single shared UDP socket, routing
  * inbound SIP messages to the matching call by Call-ID. Construct via
  * [[SoftphoneStreaming.resource]].
  */
class SoftphoneStreaming[F[_]: Async: Network] private (
    config: SoftphoneConfig,
    sendRaw: (SocketAddress[IpAddress], SipMessage) => F[Unit],
    routes: Ref[F, Map[String, Channel[F, SipMessage]]],
    nextCallIndex: Ref[F, Int],
    random: Random[F],
    pcmFrameBytes: Int,
    logger: StructuredLogger[F]
) extends Softphone[F] {

  private def newCallId(localIp: IpAddress): F[String] =
    random
      .nextBytes(10)
      .map(_.map("%02x".format(_)).mkString)
      .map(_ + "@" + localIp.toString)

  private def resolveHost(host: String): F[IpAddress] =
    IpAddress.fromString(host) match {
      case Some(ip) => Async[F].pure(ip)
      case None     =>
        Hostname.fromString(host) match {
          case Some(h) =>
            Dns.forAsync[F].resolve(h).adaptError { case _ =>
              SoftphoneError.HostResolutionFailed(host)
            }
          case None => Async[F].raiseError(SoftphoneError.InvalidHostname(host))
        }
    }

  override def call(
      callee: Callee,
      extraHeaders: Map[String, List[String]] = Map.empty
  ): Resource[F, Call[F]] =
    for {
      calleeAddr <- Resource.eval(resolveCalleeAddress(callee.value))
      localIp    <- Resource.eval(
        localIpFor(calleeAddr).flatTap(ip =>
          logger.info(Map("address" -> ip.toString))("Advertising local IP")
        )
      )
      builder <- Resource.eval(allocateBuilder(localIp))
      callId  <- Resource.eval(newCallId(localIp))
      sipCh   <- Resource.make(
        Channel
          .bounded[F, SipMessage](128)
          .flatTap(ch => routes.update(_ + (callId -> ch)))
      )(_ => routes.update(_ - callId))
      sipIn   = sipCh.stream
      sendSip = (msg: SipMessage) => sendRaw(calleeAddr, msg)
      call <- buildCall(
        callId,
        callee.value,
        sipIn,
        sendSip,
        builder,
        extraHeaders
      )
    } yield call

  private def resolveCalleeAddress(
      callee: String
  ): F[SocketAddress[IpAddress]] = {
    val host =
      if callee.contains("@") then callee.split("@").last
      else config.realm
    resolveHost(host).map(ip => SocketAddress(ip, config.serverPort))
  }

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

  private def allocateBuilder(localIp: IpAddress): F[SipMessageBuilder] =
    nextCallIndex.getAndUpdate(_ + 1).flatMap { callIndex =>
      val portsPerCall = 2
      val rtpPortInt   = config.baseRtpPort.value + callIndex * portsPerCall
      Port.fromInt(rtpPortInt) match {
        case None =>
          Async[F].raiseError(SoftphoneError.RtpPortExhausted(rtpPortInt))
        case Some(rtpPort) =>
          Async[F].pure(
            new SipMessageBuilder(config, localIp, config.localSipPort, rtpPort)
          )
      }
    }

  private def buildCall(
      callId: String,
      callee: String,
      sipIn: Stream[F, SipMessage],
      sendSip: SipMessage => F[Unit],
      builder: SipMessageBuilder,
      extraHeaders: Map[String, List[String]]
  ): Resource[F, OutboundCall[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      media      <- Resource.eval(
        MediaSession.create(
          pcmFrameBytes,
          builder.bindAddress,
          builder.localRtpPort
        )
      )
      call <- Resource.eval(
        OutboundCall.create(
          callId,
          callee,
          sendSip,
          builder,
          supervisor,
          random,
          media,
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
      sipTransport <- SipTransport[F](
        config.localSipPort,
        config.bindAddress,
        logger
      )
      (sipIn, sendRaw) = sipTransport
      routes <- Resource.eval(
        Ref[F].of(Map.empty[String, Channel[F, SipMessage]])
      )
      _ <- Resource.make(
        sipIn
          .evalMap { msg =>
            routes.get.flatMap {
              _.get(msg.headers.callId) match {
                case Some(ch) => ch.send(msg).void
                case None     =>
                  logger.warn(Map("callId" -> msg.headers.callId))(
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
      random    <- Resource.eval(SecureRandom.javaSecuritySecureRandom[F])
    } yield new SoftphoneStreaming(
      config,
      sendRaw,
      routes,
      callIndex,
      random,
      pcmFrameBytes = audioFormat.getFrameSize * RtpTransport.PacketSamples,
      logger
    )
}
