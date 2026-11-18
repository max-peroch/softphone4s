package softphone4s.config

import com.comcast.ip4s.{Host, IpAddress, Port, SocketAddress, port}
import fs2.io.file.Path
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto.deriveReader
import softphone4s.dns.DnsConfig
import softphone4s.media.JitterConfig
import softphone4s.model.{
  ByteLimit,
  Callee,
  PositiveDuration,
  Secret,
  TransportProtocol,
  User
}
import softphone4s.srtp.SrtpPolicy
import softphone4s.transport.{
  ClientAuthentication,
  FramingLimits,
  HostnameVerification,
  KeepAlive,
  KeyStoreConfig,
  MaxConnections,
  StreamConfig,
  TlsConfig,
  TlsVersion
}

import scala.concurrent.duration.FiniteDuration

/** How softphone4s settings are read with
  * [[https://pureconfig.github.io/ pureconfig]]. They are in a module of their
  * own because pureconfig is for the JVM only, and the library is not.
  *
  * Import them where a configuration is loaded:
  *
  * {{{
  * import pureconfig.ConfigSource
  * import softphone4s.config.PureConfigReaders.given
  *
  * ConfigSource.default.at("softphone").load[SoftphoneConfig]
  * }}}
  *
  * A value the type refuses is a `CannotConvert` with the reason the type
  * gives, so a bad setting fails when the file is loaded.
  */
object PureConfigReaders {

  private def fromToken[A](
      what: String,
      expected: String,
      all: Array[A]
  )(token: A => String): ConfigReader[A] =
    ConfigReader[String].emap(s =>
      all.find(token(_) == s).toRight(CannotConvert(s, what, expected))
    )

  private def checked[A, B](what: String)(
      f: A => Either[String, B]
  )(using reader: ConfigReader[A]): ConfigReader[B] =
    reader.emap(a => f(a).left.map(CannotConvert(a.toString, what, _)))

  // ---------------------------------------------------------------- values

  given ConfigReader[User]      = checked[String, User]("User")(User(_))
  given ConfigReader[Secret]    = ConfigReader[String].map(Secret(_))
  given ConfigReader[Callee]    = checked[String, Callee]("Callee")(Callee(_))
  given ConfigReader[ByteLimit] =
    checked[Int, ByteLimit]("ByteLimit")(ByteLimit(_))
  given ConfigReader[PositiveDuration] =
    checked[FiniteDuration, PositiveDuration]("PositiveDuration")(
      PositiveDuration(_)
    )
  given ConfigReader[Attempts] = checked[Int, Attempts]("Attempts")(Attempts(_))
  given ConfigReader[MaxConnections] =
    checked[Int, MaxConnections]("MaxConnections")(MaxConnections(_))

  // --------------------------------------------------------------- network

  given ConfigReader[Port] = ConfigReader[Int].emap(i =>
    Port
      .fromInt(i)
      .toRight(CannotConvert(i.toString, "Port", "not in range 0-65535"))
  )
  given ConfigReader[TransportProtocol] = ConfigReader[String].emap(s =>
    TransportProtocol
      .fromToken(s)
      .toRight(CannotConvert(s, "TransportProtocol", "not a known transport"))
  )

  /** `address` or `address:port` (`[v6]:port`), the port defaulting to 53: the
    * only thing we read this for is a DNS server.
    */
  given ConfigReader[SocketAddress[IpAddress]] = ConfigReader[String].emap(s =>
    SocketAddress
      .fromStringIp(s)
      .orElse(IpAddress.fromString(s).map(SocketAddress(_, port"53")))
      .toRight(
        CannotConvert(s, "SocketAddress", "expected address or address:port")
      )
  )

  /** A host name or an IP address. */
  given ConfigReader[Host] = ConfigReader[String].emap(s =>
    Host
      .fromString(s)
      .toRight(CannotConvert(s, "Host", "not a host name or address"))
  )
  given ConfigReader[IpAddress] = ConfigReader[String].emap(s =>
    IpAddress
      .fromString(s)
      .toRight(CannotConvert(s, "IpAddress", "invalid IP address"))
  )

  // ----------------------------------------------------------------- enums

  given ConfigReader[CodecChoice] = ConfigReader[String].emap { s =>
    val token = s.trim.toLowerCase
    Either.cond(
      token.nonEmpty,
      List(CodecChoice.Pcmu, CodecChoice.Pcma, CodecChoice.G722)
        .find(_.token == token)
        .getOrElse(CodecChoice.Named(token)),
      CannotConvert(s, "CodecChoice", "a codec needs a name")
    )
  }
  given ConfigReader[RtpLatching] =
    fromToken("RtpLatching", "expected follow or ignore", RtpLatching.values)(
      _.token
    )
  given ConfigReader[ObservedAddresses] =
    fromToken(
      "ObservedAddresses",
      "expected use or ignore",
      ObservedAddresses.values
    )(_.token)
  given ConfigReader[KeyStoreType] = ConfigReader[String].emap(name =>
    KeyStoreType.values
      .find(_.javaName.equalsIgnoreCase(name.trim))
      .toRight(CannotConvert(name, "KeyStoreType", "expected PKCS12 or JKS"))
  )
  given ConfigReader[SrtpPolicy] =
    fromToken(
      "SrtpPolicy",
      "expected off, optional or required",
      SrtpPolicy.values
    )(_.token)
  given ConfigReader[ClientAuthentication] =
    fromToken(
      "ClientAuthentication",
      "expected off, request or require",
      ClientAuthentication.values
    )(_.token)
  given ConfigReader[HostnameVerification] =
    fromToken(
      "HostnameVerification",
      "expected verify or skip",
      HostnameVerification.values
    )(_.token)
  given ConfigReader[TlsVersion] =
    fromToken("TlsVersion", "expected 1.2 or 1.3", TlsVersion.values)(_.token)

  // --------------------------------------------------------------- records

  given ConfigReader[KeepAlive]     = deriveReader
  given ConfigReader[FramingLimits] = deriveReader
  given ConfigReader[StreamConfig]  = deriveReader
  given ConfigReader[Path]          =
    ConfigReader[java.nio.file.Path].map(Path.fromNioPath)
  given ConfigReader[KeyStoreConfig] = deriveReader
  given ConfigReader[TlsConfig]      = deriveReader
  given ConfigReader[StunConfig]     = deriveReader
  given ConfigReader[NatConfig]      = deriveReader
  given ConfigReader[DnsConfig]      = deriveReader
  given ConfigReader[AccountConfig]  = deriveReader

  /** The jitter buffer is read as its keys, each optional, and then checked as
    * a whole: the maximum depth defaults to twice the depth, up to the
    * capacity.
    */
  private final case class JitterKeys(
      depth: Int = 3,
      capacity: Int = 12,
      maxDepth: Option[Int] = None
  )
  private given ConfigReader[JitterKeys] = deriveReader

  given ConfigReader[JitterConfig] = ConfigReader[JitterKeys].emap(keys =>
    JitterConfig
      .adaptive(
        keys.depth,
        keys.maxDepth.getOrElse(math.min(keys.depth * 2, keys.capacity)),
        keys.capacity
      )
      .left
      .map(
        CannotConvert(
          s"${keys.depth}, ${keys.capacity}, ${keys.maxDepth}",
          "JitterConfig",
          _
        )
      )
  )

  given ConfigReader[SoftphoneConfig] = deriveReader
}
