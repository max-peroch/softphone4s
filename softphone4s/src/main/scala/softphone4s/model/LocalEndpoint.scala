package softphone4s.model

import com.comcast.ip4s.{Host, IpAddress, Port}
import softphone4s.config.SoftphoneConfig

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
    credentials: Credentials
)

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
    for {
      user  <- User(config.user).left.map(invalid(config.user, _))
      realm <- Host
        .fromString(config.realm)
        .toRight(invalid(config.realm, "not a valid host"))
      account <- SipUri
        .from(SipScheme.Sip, realm, Some(UserInfo(user, None)))
        .left
        .map(invalid(config.user, _))
      contact <- SipUri
        .from(
          SipScheme.Sip,
          ip,
          Some(UserInfo(user, None)),
          Some(sipPort)
        )
        .left
        .map(invalid(config.user, _))
    } yield new LocalEndpoint(
      ip,
      sipPort,
      rtpPort,
      config.bindAddress,
      account,
      contact,
      Credentials(config.user, config.password)
    )

  private def invalid(input: String, reason: String): SoftphoneError =
    SoftphoneError.InvalidSipUri(input, reason)
}
