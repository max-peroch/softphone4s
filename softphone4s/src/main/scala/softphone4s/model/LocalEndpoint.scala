package softphone4s.model

import com.comcast.ip4s.{Host, IpAddress, Port, SocketAddress}
import softphone4s.config.{AccountConfig, SoftphoneConfig}

/** Where and as whom we speak SIP: the address we advertise (`ip`, `sipPort`,
  * `rtpPort`), the socket address we bind (`bindAddress`), the account's
  * address-of-record and the Contact URI that replies are sent to. Construct
  * via [[LocalEndpoint.from]], which validates the account as a SIP URI.
  */
final case class LocalEndpoint private (
    ip: IpAddress,
    sipPort: Port,
    rtpPort: Port,
    bindAddress: IpAddress,
    accountUri: SipUri,
    contact: SipUri,
    credentials: Credentials,
    transport: TransportProtocol,
    scheme: SipScheme,
    mapping: Option[SocketAddress[IpAddress]]
) {

  /** The address media should be sent to: ours as the outside sees it when a
    * server has told us what that is, else our own.
    */
  def mediaIp: IpAddress = mapping.fold(ip)(_.host)

  /** This endpoint as the outside sees it at `address`, for a UDP endpoint that
    * a NAT sits in front of: the Contact says where we can be reached from
    * outside, and so does the media address in SDP. The Via keeps our own
    * address, since responses come back by `rport`. Nothing changes if
    * `address` is ours, or for a stream transport, whose connection already
    * carries the answers.
    */
  def mappedTo(
      address: SocketAddress[IpAddress]
  ): Either[SoftphoneError, LocalEndpoint] =
    if transport != TransportProtocol.Udp ||
      (address.host == ip && address.port == sipPort)
    then Right(this)
    else
      LocalEndpoint
        .contactUri(
          accountUri.userInfo,
          address.host,
          address.port,
          transport,
          scheme
        )
        .map(contact => copy(contact = contact, mapping = Some(address)))

  /** The same account and RTP port reached over `transport` from local address
    * `ip`: what another target of the same call needs, since a different route
    * can mean a different local address and a different transport changes the
    * Via and the Contact.
    */
  def via(
      ip: IpAddress,
      transport: TransportProtocol
  ): Either[SoftphoneError, LocalEndpoint] =
    LocalEndpoint
      .contactUri(accountUri.userInfo, ip, sipPort, transport, scheme)
      .map(contact =>
        copy(ip = ip, contact = contact, transport = transport, mapping = None)
      )

  /** This endpoint with a `sips:` Contact, for a call whose Request-URI is
    * `sips:` (RFC 5630): the Contact must then be secure too. Nothing changes
    * for any other scheme.
    */
  def forScheme(requested: SipScheme): Either[SoftphoneError, LocalEndpoint] =
    if requested == scheme then Right(this)
    else
      LocalEndpoint
        .contactUri(accountUri.userInfo, ip, sipPort, transport, requested)
        .map(contact => copy(contact = contact, scheme = requested))
}

object LocalEndpoint {

  /** Fails with `InvalidSipUri` if `config.user` or `config.realm` cannot form
    * a SIP URI.
    */
  def from(
      config: SoftphoneConfig,
      ip: IpAddress,
      sipPort: Port,
      rtpPort: Port
  ): Either[SoftphoneError, LocalEndpoint] =
    from(
      config.defaultAccount,
      config.bindAddress,
      ip,
      sipPort,
      rtpPort,
      config.transport
    )

  /** The endpoint for one of the accounts, speaking SIP over `transport`. */
  def from(
      config: AccountConfig,
      bindAddress: IpAddress,
      ip: IpAddress,
      sipPort: Port,
      rtpPort: Port,
      transport: TransportProtocol
  ): Either[SoftphoneError, LocalEndpoint] =
    for {
      user    <- User(config.user).left.map(invalid(config.user, _))
      account <- addressOfRecord(config)
      contact <- contactUri(
        Some(UserInfo(user, None)),
        ip,
        sipPort,
        transport,
        SipScheme.Sip
      )
    } yield new LocalEndpoint(
      ip,
      sipPort,
      rtpPort,
      bindAddress,
      account,
      contact,
      Credentials(config.user, config.password),
      transport,
      SipScheme.Sip,
      None
    )

  /** The Contact URI for reaching us at `ip`:`port`. A `sip:` one names the
    * transport unless it is UDP; a `sips:` one does not, since `sips:` means
    * TLS (RFC 5630 deprecates `transport=tls` on it).
    */
  private[model] def contactUri(
      userInfo: Option[UserInfo],
      ip: IpAddress,
      port: Port,
      transport: TransportProtocol,
      scheme: SipScheme
  ): Either[SoftphoneError, SipUri] =
    SipUri
      .from(
        scheme,
        ip,
        userInfo,
        Some(port),
        Option.when(
          scheme == SipScheme.Sip && transport != TransportProtocol.Udp
        )(
          transport
        )
      )
      .left
      .map(invalid(userInfo.fold("")(_.user.value), _))

  /** The account's address-of-record, `sip:user@realm`. */
  def addressOfRecord(
      config: AccountConfig
  ): Either[SoftphoneError, SipUri] =
    for {
      user  <- User(config.user).left.map(invalid(config.user, _))
      realm <- Host
        .fromString(config.realm)
        .toRight(invalid(config.realm, "not a valid host"))
      aor <- SipUri
        .from(SipScheme.Sip, realm, Some(UserInfo(user, None)))
        .left
        .map(invalid(config.user, _))
    } yield aor

  private def invalid(input: String, reason: String): SoftphoneError =
    SoftphoneError.InvalidSipUri(input, reason)
}
