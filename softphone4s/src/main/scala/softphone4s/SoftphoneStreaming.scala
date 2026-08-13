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
    localIp: IpAddress,
    sendRaw: (SocketAddress[IpAddress], SipMessage) => F[Unit],
    routes: Ref[F, Map[String, Channel[F, SipMessage]]],
    nextCallIndex: Ref[F, Int],
    random: Random[F],
    pcmFrameBytes: Int,
    logger: StructuredLogger[F]
) extends Softphone[F] {

  private def newCallId(): F[String] =
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
      builder    <- Resource.eval(allocateBuilder)
      callId     <- Resource.eval(newCallId())
      sipCh      <- Resource.make(
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

  private def allocateBuilder: F[SipMessageBuilder] =
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
      localIp      <- Resource.eval(detectLocalIp(config.localIpFallback))
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
      localIp,
      sendRaw,
      routes,
      callIndex,
      random,
      pcmFrameBytes = audioFormat.getFrameSize * RtpTransport.PacketSamples,
      logger
    )

  private def detectLocalIp[F[_]: Async](fallback: IpAddress): F[IpAddress] =
    Async[F].blocking {
      import java.net.{Inet4Address, NetworkInterface}
      import scala.jdk.CollectionConverters.*
      NetworkInterface.getNetworkInterfaces.asScala
        .filterNot(i => i.isLoopback || !i.isUp || i.isVirtual)
        .flatMap(_.getInetAddresses.asScala)
        .collectFirst { case a: Inet4Address =>
          IpAddress.fromString(a.getHostAddress).getOrElse(fallback)
        }
        .getOrElse(fallback)
    }
}
