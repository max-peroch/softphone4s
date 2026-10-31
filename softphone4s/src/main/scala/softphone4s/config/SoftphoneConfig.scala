package softphone4s.config

import cats.data.NonEmptyList
import com.comcast.ip4s.*
import pureconfig.*
import pureconfig.generic.semiauto.*
import softphone4s.dns.DnsConfig
import softphone4s.media.AudioCodecs
import softphone4s.model.TransportProtocol
import softphone4s.sdp.Capabilities
import softphone4s.transport.{StreamConfig, TlsConfig}

import scala.concurrent.duration.*

/** SIP account and network settings for a `SoftphoneStreaming`. `user`,
  * `password`, `realm`, `serverPort` and `registerExpires` describe the default
  * account; `additionalAccounts` adds more, all sharing the one SIP socket and
  * RTP port range. Loadable via pureconfig — see the `given
  * ConfigReader[SoftphoneConfig]` below.
  */
final case class SoftphoneConfig(
    user: String,
    password: String,
    realm: String,
    localSipPort: Port = port"5060",
    serverPort: Port = port"5060",
    baseRtpPort: Port = port"8000",
    bindAddress: IpAddress = ip"0.0.0.0",
    localIpFallback: IpAddress = ip"127.0.0.1",
    hangupTimeout: FiniteDuration = 5.seconds,
    inviteTimeout: FiniteDuration = 32.seconds,
    registerExpires: FiniteDuration = 1.hour,
    answerTimeout: FiniteDuration = 60.seconds,
    additionalAccounts: List[AccountConfig] = Nil,
    transport: TransportProtocol = TransportProtocol.Udp,
    tcp: StreamConfig = StreamConfig(),
    tls: TlsConfig = TlsConfig(),
    dns: DnsConfig = DnsConfig(),
    listen: Set[TransportProtocol] = Set.empty,
    tlsPort: Port = port"5061",
    maxUdpMessage: Int = 1300,
    nat: NatConfig = NatConfig(),
    codecs: List[CodecChoice] = List(CodecChoice.Pcmu, CodecChoice.Pcma)
) {

  /** What our offers and answers are made from: the codecs chosen, in order. */
  def capabilities: Capabilities =
    NonEmptyList
      .fromList(codecs)
      .flatMap(AudioCodecs.resolve(_, Nil).toOption)
      .fold(Capabilities.Default)(_.capabilities)

  /** The account `call` and `register` use. */
  def defaultAccount: AccountConfig =
    AccountConfig(
      user,
      password,
      realm,
      serverPort,
      registerExpires,
      transport
    )

  /** Every account, the default first. */
  def accounts: NonEmptyList[AccountConfig] =
    NonEmptyList(defaultAccount, additionalAccounts)
}

object SoftphoneConfig {
  import NetworkReaders.given

  given ConfigReader[SoftphoneConfig] = deriveReader
}
