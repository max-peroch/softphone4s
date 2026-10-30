package softphone4s.model

import com.comcast.ip4s.{Host, IpAddress, Port}
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
    transport: TransportProtocol
) {

  /** The same account and RTP port reached over `transport` from local address
    * `ip`: what another target of the same call needs, since a different route
    * can mean a different local address and a different transport changes the
    * Via and the Contact.
    */
  def via(
      ip: IpAddress,
      transport: TransportProtocol
  ): Either[SoftphoneError, LocalEndpoint] =
    SipUri
      .from(
        SipScheme.Sip,
        ip,
        accountUri.userInfo,
        Some(sipPort),
        Option.when(transport != TransportProtocol.Udp)(transport)
      )
      .left
      .map(SoftphoneError.InvalidSipUri(accountUri.render, _))
      .map(contact => copy(ip = ip, contact = contact, transport = transport))
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
      contact <- SipUri
        .from(
          SipScheme.Sip,
          ip,
          Some(UserInfo(user, None)),
          Some(sipPort),
          Option.when(transport != TransportProtocol.Udp)(transport)
        )
        .left
        .map(invalid(config.user, _))
    } yield new LocalEndpoint(
      ip,
      sipPort,
      rtpPort,
      bindAddress,
      account,
      contact,
      Credentials(config.user, config.password),
      transport
    )

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
