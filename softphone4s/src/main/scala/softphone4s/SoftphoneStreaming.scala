package softphone4s

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.{Queue, Supervisor}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.concurrent.{Channel, SignallingRef}
import fs2.io.net.Network
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.config.{ObservedAddresses, SoftphoneConfig, StunConfig}
import softphone4s.dns.{DnsJavaRecords, DnsRecords, TargetResolver}
import softphone4s.fsm.CallTimer
import softphone4s.metrics.{CallDirection, Metrics}
import softphone4s.model.*
import softphone4s.registration.Registration
import softphone4s.media.{AudioCodec, AudioCodecs, RtpPortRange, RtpPorts}
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.stun.StunClient
import softphone4s.transaction.{
  TransactionConfig,
  TransactionLayer,
  TransactionManager,
  TransactionTracing
}
import softphone4s.transport.{
  RoutedTransport,
  SipTransport,
  TcpTransport,
  TlsTransport
}
import softphone4s.messaging.IncomingMessage
import softphone4s.ua.{DialogInput, MediaSession, MessageSender, OutboundCall}

import javax.sound.sampled.AudioFormat

/** fs2/Cats Effect `Softphone` backed by a single shared UDP socket, routing
  * inbound SIP messages to the matching call by Call-ID. Construct via
  * [[SoftphoneStreaming.resource]].
  */
class SoftphoneStreaming[F[_]: Async: Network] private (
    config: SoftphoneConfig,
    accountList: NonEmptyList[Account],
    uplinks: Uplinks[F],
    endpoints: EndpointFactory[F],
    publicSip: SignallingRef[F, Option[SocketAddress[IpAddress]]],
    layer: TransactionLayer[F],
    routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
    entropy: Entropy[F],
    codecs: AudioCodecs,
    router: IncomingRouter[F],
    observer: CallObserver[F],
    logger: StructuredLogger[F]
) extends Softphone[F] {

  override val accounts: NonEmptyList[AccountId] = accountList.map(_.id)

  override def incoming: Stream[F, IncomingCall[F]] = router.calls

  override val messages: Messaging[F] =
    new MessagingService[F](
      accounts,
      accountNamed,
      uplinks,
      new MessageSender[F](layer, entropy),
      router.incomingMessages
    )

  override val subscriptions: Subscriptions[F] =
    new SubscriptionService[F](
      accounts,
      accountNamed,
      uplinks,
      layer,
      routes,
      entropy,
      config.hangupTimeout,
      logger
    )

  override val publications: Publications[F] =
    new PublicationService[F](
      accounts,
      accountNamed,
      uplinks,
      layer,
      entropy,
      config.hangupTimeout,
      logger
    )

  private def accountNamed(id: AccountId): F[Account] =
    Async[F].fromOption(
      accountList.find(_.id == id),
      SoftphoneError.UnknownAccount(id.render)
    )

  private def toExtensionHeaders(
      headers: Map[String, List[String]]
  ): Either[SoftphoneError, List[ExtensionHeader]] =
    headers.toList.flatTraverse { (name, values) =>
      FieldName(name)
        .leftMap(SoftphoneError.InvalidHeader(name, _))
        .map(n => values.map(ExtensionHeader(n, _)))
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
      found <- Resource.eval(uplinks.allTargets(account, target))
      resolved = found.head
      protocol <- Resource.eval(endpoints.available(resolved.transport))
      calleeAddr = resolved.address
      localIp <- Resource.eval(
        endpoints
          .localIpFor(calleeAddr)
          .flatTap(ip =>
            logger.info(Map(LogFields.Address -> ip.toString))(
              "Advertising local IP"
            )
          )
      )
      endpoint <- endpoints
        .allocateEndpoint(account, localIp, protocol)
        .evalMap(endpoints.secureIfNeeded(_, target.scheme))
        .evalMap(endpoints.seenFromOutside)
      alternatives <- Resource.eval(
        uplinks.alternativesTo(endpoint, found.tail)
      )
      streamWay <- Resource.eval(
        uplinks.streamWayTo(account, target, endpoint, calleeAddr)
      )
      callId <- Resource.eval(entropy.callId(localIp))
      sipCh  <- Resource.make(
        Channel
          .bounded[F, DialogInput[F]](128)
          .flatTap(ch => routes.update(_ + (callId -> ch)))
      )(_ => routes.update(_ - callId))
      call <- buildCall(
        callId,
        target,
        sipCh.stream,
        Uplink(endpoint, calleeAddr),
        alternatives,
        streamWay,
        extra
      )
    } yield call

  override def registerAs(id: AccountId): Resource[F, Registration[F]] =
    for {
      account      <- Resource.eval(accountNamed(id))
      registrarUri <- Resource.eval(
        Async[F].fromEither(
          SipUri
            .from(SipScheme.Sip, account.config.realm)
            .leftMap(
              SoftphoneError.InvalidSipUri(account.config.realm.toString, _)
            )
        )
      )
      found <- Resource.eval(uplinks.allTargets(account, registrarUri))
      resolved = found.head
      protocol <- Resource.eval(endpoints.available(resolved.transport))
      registrar = resolved.address
      localIp <- Resource.eval(endpoints.localIpFor(registrar))
      // A REGISTER carries no media, so the RTP port is not used.
      endpoint <- Resource.eval(
        endpoints.signallingEndpoint(account, localIp, protocol)
      )
      alternatives <- Resource.eval(
        uplinks.alternativesTo(endpoint, found.tail)
      )
      registration <- Registration.resource(
        layer,
        NonEmptyList(Uplink(endpoint, registrar), alternatives),
        publicSip,
        registrarUri,
        entropy,
        Expires.of(account.config.registerExpires),
        config.hangupTimeout,
        logger
      )
      _ <- observer.registered(account.id, registration)
    } yield registration

  private def buildCall(
      callId: CallId,
      callee: SipUri,
      dialogInput: Stream[F, DialogInput[F]],
      first: Uplink,
      alternatives: List[Uplink],
      streamWay: Option[Uplink],
      extraHeaders: List[ExtensionHeader]
  ): Resource[F, OutboundCall[F]] =
    for {
      supervisor <- Supervisor[F](await = false)
      timers     <- Timers.resource[F, CallTimer]
      media      <- Resource.eval(
        MediaSession.create(
          first.endpoint.bindAddress,
          first.endpoint.rtpPort,
          codecs,
          config.nat.rtpLatching,
          config.jitterBuffer
        )
      )
      call <- Resource.eval(
        OutboundCall.create(
          callId,
          callee,
          layer,
          first,
          alternatives,
          streamWay,
          config.maxUdpMessage.value,
          (transport, uri) => uplinks.nextHopOver(transport)(uri),
          supervisor,
          entropy,
          media,
          timers,
          // Each target gets the time one would have had.
          config.inviteTimeout * (1 + alternatives.size).toLong,
          logger,
          extraHeaders
        )
      )
      _ <- Resource.eval(
        observer.observe(
          CallDirection.Outgoing,
          call,
          call.endedBecause,
          call.jitterStats
        )
      )
      _ <- Resource.make(call.runEventLoop(dialogInput).start)(fiber =>
        call.hangup >> fiber.join.void
          .timeoutTo(config.hangupTimeout, fiber.cancel)
      )
    } yield call

  // ---- Incoming calls ----------------------------------------------------------

}

object SoftphoneStreaming {

  /** How many instant messages wait for a reader before the next is refused. */
  val MessageBacklog: Int = 64

  /** How many targets of a name are tried at most. */
  private[softphone4s] val MaxTargets: Int = 4

  /** Binds the local SIP UDP socket and returns a ready-to-use `Softphone`. The
    * socket, the transaction layer and the routers that hand messages to calls
    * are released when the resource is.
    */
  def resource[F[_]: Async: Network](
      config: SoftphoneConfig,
      audioFormat: AudioFormat,
      logger: StructuredLogger[F],
      extraCodecs: List[AudioCodec] = Nil,
      metrics: Option[Metrics[F]] = None,
      tracing: Option[TransactionTracing[F]] = None
  ): Resource[F, SoftphoneStreaming[F]] =
    resourceWith(
      config,
      audioFormat,
      logger,
      DnsJavaRecords.configured[F](config.dns),
      extraCodecs,
      metrics,
      tracing
    )

  /** As [[resource]], looking names up with `dns` instead of the system's DNS
    * servers.
    */
  def resourceWith[F[_]: Async: Network](
      config: SoftphoneConfig,
      audioFormat: AudioFormat,
      logger: StructuredLogger[F],
      dns: F[DnsRecords[F]],
      extraCodecs: List[AudioCodec] = Nil,
      metrics: Option[Metrics[F]] = None,
      tracing: Option[TransactionTracing[F]] = None
  ): Resource[F, SoftphoneStreaming[F]] =
    for {
      // What goes to `metrics` or `tracing` cannot break the stack: an error is
      // logged.
      measured = Metrics.guarded(metrics.getOrElse(Metrics.noop[F]), logger)
      traced   = TransactionTracing.guarded(
        tracing.getOrElse(TransactionTracing.noop[F]),
        logger
      )
      codecs <- Resource.eval(
        Async[F].fromEither(audioCodecs(config, audioFormat, extraCodecs))
      )
      udp <- SipTransport.udp[F](
        SocketAddress(config.bindAddress, config.localSipPort),
        logger
      )
      tcp <- TcpTransport.resource[F](
        config.bindAddress,
        Option.when(config.listen.contains(TransportProtocol.Tcp))(
          config.localSipPort
        ),
        config.tcp,
        logger
      )
      tls <- TlsTransport.resource[F](
        config.bindAddress,
        Option.when(config.listen.contains(TransportProtocol.Tls))(
          config.tlsPort
        ),
        config.tls,
        logger
      )
      transport = RoutedTransport(udp, tcp, tls)
      publicSip <- Resource.eval(
        SignallingRef.of[F, Option[SocketAddress[IpAddress]]](None)
      )
      layer <- TransactionManager.resourceObserving[F](
        transport,
        TransactionConfig(),
        logger,
        // Believed unless we are told to keep to the address we bound.
        observed =>
          publicSip
            .set(Some(observed.address))
            .whenA(config.nat.observedAddress == ObservedAddresses.Use),
        measured.transaction,
        traced
      )
      routes <- Resource.eval(
        Ref[F].of(Map.empty[CallId, Channel[F, DialogInput[F]]])
      )
      accounts <- Resource.eval(
        Async[F].fromEither(Account.all(config.accounts))
      )
      supervisor <- Supervisor[F](await = false)
      observer = new CallObserver[F](measured, supervisor)
      incomingCalls <- Resource.eval(Queue.unbounded[F, IncomingCall[F]])
      messageQueue  <- Resource.eval(
        Queue.bounded[F, IncomingMessage](SoftphoneStreaming.MessageBacklog)
      )
      range <- Resource.eval(
        Async[F].fromEither(
          RtpPortRange(config.baseRtpPort, config.lastRtpPort).leftMap(
            SoftphoneError.RtpPortsInvalid(_)
          )
        )
      )
      rtpPorts <- Resource.eval(RtpPorts.binding[F](range, config.bindAddress))
      entropy  <- Resource.eval(Entropy.secure[F])
      records  <- Resource.eval(dns)
      endpoints = new EndpointFactory[F](
        config,
        publicSip,
        rtpPorts,
        entropy,
        codecs
      )
      uplinks = new Uplinks[F](
        new TargetResolver[F](
          records,
          entropy,
          Set(
            TransportProtocol.Udp,
            TransportProtocol.Tcp,
            TransportProtocol.Tls
          )
        ),
        endpoints,
        logger
      )
      router = new IncomingRouter[F](
        config,
        accounts,
        layer,
        routes,
        endpoints,
        uplinks,
        entropy,
        codecs,
        supervisor,
        incomingCalls,
        messageQueue,
        observer,
        logger
      )
      softphone = new SoftphoneStreaming(
        config,
        accounts,
        uplinks,
        endpoints,
        publicSip,
        layer,
        routes,
        entropy,
        codecs,
        router,
        observer,
        logger
      )
      _ <- Resource.eval(supervisor.supervise(router.routeIncoming))
      _ <- Resource.eval(supervisor.supervise(router.routeUnmatched))
      _ <- config.nat.stun
        .filter(_ => config.nat.observedAddress == ObservedAddresses.Use)
        .traverse_(stun =>
          Resource.eval(
            supervisor.supervise(
              askStun(stun, udp, records, publicSip, logger)
            )
          )
        )
    } yield softphone

  /** The codecs configured, if there are some and they work at the sample rate
    * of the audio format: a different rate would have to be converted, which is
    * not done.
    */
  private def audioCodecs(
      config: SoftphoneConfig,
      audioFormat: AudioFormat,
      extraCodecs: List[AudioCodec]
  ): Either[SoftphoneError, AudioCodecs] =
    for {
      chosen <- NonEmptyList
        .fromList(config.codecs)
        .toRight(SoftphoneError.NoCodecsConfigured)
      codecs    <- AudioCodecs.resolve(chosen, extraCodecs)
      codecRate <- codecs.pcmRate.toRight(SoftphoneError.MixedCodecRates)
      deviceRate = audioFormat.getSampleRate.toInt
      _ <- Either.cond(
        codecRate == deviceRate,
        (),
        SoftphoneError.AudioFormatMismatch(codecRate, deviceRate)
      )
    } yield codecs

  /** Asks the STUN server how we look from outside, and again every `refresh`;
    * a round that fails is logged and tried again at the next.
    */
  private def askStun[F[_]: Async](
      stun: StunConfig,
      client: StunClient[F],
      records: DnsRecords[F],
      publicSip: SignallingRef[F, Option[SocketAddress[IpAddress]]],
      logger: StructuredLogger[F]
  ): F[Unit] = {
    val round: F[Unit] =
      records.addresses(stun.host).flatMap {
        case Nil =>
          logger.warn(Map(LogFields.Host -> stun.host.toString))(
            "The STUN server has no address"
          )
        case address :: _ =>
          client
            .bindingRequest(
              SocketAddress(address, stun.port),
              stun.retransmit,
              stun.attempts.value
            )
            .flatMap {
              case Right(seen) =>
                logger.debug(Map(LogFields.Address -> seen.toString))(
                  "STUN says we are seen at"
                ) >> publicSip.set(Some(seen))
              case Left(error) =>
                logger.warn(
                  Map(
                    LogFields.Host  -> stun.host.toString,
                    LogFields.Error -> error.toString
                  )
                )("The STUN server did not tell us our address")
            }
      }
    (round.handleErrorWith(e =>
      logger.warn(Map(LogFields.Error -> e.toString))("STUN round failed")
    ) >> Async[F].sleep(stun.refresh)).foreverM
  }
}
