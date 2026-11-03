package softphone4s

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.concurrent.SignallingRef
import softphone4s.config.{ObservedAddresses, SoftphoneConfig}
import softphone4s.model.*
import softphone4s.sdp.LocalCrypto
import softphone4s.srtp.SrtpPolicy
import softphone4s.media.{AudioCodecs, RtpPorts}
import softphone4s.runtime.Entropy

/** How the endpoint of a request is made: the address our messages come from
  * and our Contact says, as the network sees it; the pair of RTP ports and the
  * keys of a call; and the `sips:` Contact when a request needs one.
  */
private[softphone4s] final class EndpointFactory[F[_]: Async](
    config: SoftphoneConfig,
    publicSip: SignallingRef[F, Option[SocketAddress[IpAddress]]],
    rtpPorts: RtpPorts[F],
    entropy: Entropy[F],
    codecs: AudioCodecs,
    rtpMapping: Port => F[Option[SocketAddress[IpAddress]]]
) {

  /** The address the OS would send from to reach `remote`: what the SDP and
    * Contact must advertise for replies to come back. Connecting a UDP socket
    * only picks the route; nothing is sent. (The first network interface would
    * not do: on macOS it can be Docker Desktop's bridge, 192.168.65.1.) The SDP
    * advertises IN IP4, so anything but IPv4 falls back.
    */
  private[softphone4s] def localIpFor(
      remote: SocketAddress[IpAddress]
  ): F[IpAddress] =
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

  /** `endpoint` as the outside sees it, once a server has told us how that is
    * (and unless we are set to ignore that): behind a NAT, the Contact and the
    * media address of SDP are the public ones, which is where answers can reach
    * us. Over a stream transport nothing changes.
    */
  private[softphone4s] def seenFromOutside(
      endpoint: LocalEndpoint
  ): F[LocalEndpoint] =
    publicSip.get.flatMap {
      case Some(address)
          if config.nat.observedAddress == ObservedAddresses.Use =>
        Async[F].fromEither(endpoint.mappedTo(address))
      case _ => Async[F].pure(endpoint)
    }

  /** `endpoint`, with a `sips:` Contact if the request it is for is `sips:`. */
  private[softphone4s] def secureIfNeeded(
      endpoint: LocalEndpoint,
      scheme: SipScheme
  ): F[LocalEndpoint] =
    Async[F].fromEither(endpoint.forScheme(scheme))

  /** The port our messages over `transport` say they come from, and our Contact
    * says to reach us at: where we listen for it, if we do.
    */
  private[softphone4s] def sipPortFor(transport: TransportProtocol): Port =
    if transport == TransportProtocol.Tls &&
      config.listen.contains(TransportProtocol.Tls)
    then config.tlsPort
    else config.localSipPort

  /** `transport` itself, if we can speak it. */
  private[softphone4s] def available(
      transport: TransportProtocol
  ): F[TransportProtocol] =
    transport match {
      case TransportProtocol.Udp | TransportProtocol.Tcp |
          TransportProtocol.Tls =>
        transport.pure[F]
      case other =>
        Async[F].raiseError(SoftphoneError.TransportUnavailable(other.token))
    }

  /** The keys of a new call: none if it is not to be encrypted. */
  private def newCrypto: F[LocalCrypto] =
    if config.srtp == SrtpPolicy.Off then Async[F].pure(LocalCrypto.Off)
    else
      List
        .fill(LocalCrypto.keysNeeded)(entropy.masterKey)
        .sequence
        .map(LocalCrypto.numbered(config.srtp, _))

  /** An endpoint for a request that carries no media, such as REGISTER or
    * MESSAGE, which has no use for an RTP port.
    */
  private[softphone4s] def signallingEndpoint(
      account: Account,
      localIp: IpAddress,
      transport: TransportProtocol
  ): F[LocalEndpoint] =
    Async[F].fromEither(
      LocalEndpoint.from(
        account.config,
        config.bindAddress,
        localIp,
        sipPortFor(transport),
        config.baseRtpPort,
        transport,
        codecs.capabilities
      )
    )

  /** The endpoint of a call, with a pair of RTP ports that goes back to the
    * pool when the call's resources are released, and keys of its own. If a
    * STUN server says how the NAT maps the RTP port, that is what the SDP
    * advertises.
    */
  private[softphone4s] def allocateEndpoint(
      account: Account,
      localIp: IpAddress,
      transport: TransportProtocol
  ): Resource[F, LocalEndpoint] =
    for {
      rtpPort <- rtpPorts.allocate
      mapped  <- Resource.eval(rtpMapping(rtpPort))
      // The keys of a call are made once, and are the same over its session
      // changes and its uplinks.
      crypto   <- Resource.eval(newCrypto)
      endpoint <- Resource.eval(
        Async[F].fromEither(
          LocalEndpoint.from(
            account.config,
            config.bindAddress,
            localIp,
            sipPortFor(transport),
            rtpPort,
            transport,
            codecs.capabilities.copy(crypto = crypto)
          )
        )
      )
    } yield mapped.fold(endpoint)(endpoint.withRtpMapping)

}
