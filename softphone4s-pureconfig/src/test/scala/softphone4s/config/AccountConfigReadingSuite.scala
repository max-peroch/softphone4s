package softphone4s.config

import com.comcast.ip4s.*
import munit.FunSuite
import pureconfig.ConfigSource
import softphone4s.SipLiterals.*
import softphone4s.config.PureConfigReaders.given
import softphone4s.model.*
import softphone4s.srtp.SrtpPolicy
import softphone4s.transport.{
  ClientAuthentication,
  HostnameVerification,
  TlsVersion
}

import scala.concurrent.duration.*

/** The settings of a softphone, read from configuration. */
class AccountConfigReadingSuite extends FunSuite {

  test(
    "an account whose user or realm is not usable is refused when the configuration is read"
  ) {
    def load(account: String) =
      ConfigSource
        .string(
          s"""user = alice
             |password = pw
             |realm = example.com
             |additional-accounts = [ { $account } ]""".stripMargin
        )
        .load[SoftphoneConfig]
    assert(load("""user = bob, password = pw, realm = example.org""").isRight)
    assert(
      load("""user = "al ice", password = pw, realm = example.org""").isLeft
    )
    assert(load("""user = bob, password = pw, realm = "not a host"""").isLeft)
    assert(load("""user = "", password = pw, realm = example.org""").isLeft)
    assert(
      ConfigSource
        .string("""user = "a b", password = pw, realm = example.com""")
        .load[SoftphoneConfig]
        .isLeft
    )
  }

  test("configuration lists further accounts under additional-accounts") {
    val parsed = ConfigSource
      .string(
        """user = alice
          |password = pw
          |realm = example.com
          |additional-accounts = [
          |  { user = bob, password = pw2, realm = example.org, server-port = 5070 }
          |]""".stripMargin
      )
      .load[SoftphoneConfig]
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(
      parsed.accounts
        .map(a => (a.user.value, a.realm.toString, a.serverPort.value))
        .toList,
      List(("alice", "example.com", 5060), ("bob", "example.org", 5070))
    )
  }

  test("transport and the TCP settings are read from configuration") {
    val parsed = ConfigSource
      .string(
        """user = alice
          |password = pw
          |realm = example.com
          |transport = tcp
          |tcp { connect-timeout = 2 seconds, idle-timeout = 10 minutes, keep-alive { interval = 90 seconds, timeout = 5 seconds } }
          |additional-accounts = [
          |  { user = bob, password = pw2, realm = example.org, transport = udp }
          |]""".stripMargin
      )
      .load[SoftphoneConfig]
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(
      parsed.accounts.map(_.transport).toList,
      List(TransportProtocol.Tcp, TransportProtocol.Udp)
    )
    assertEquals(parsed.tcp.connectTimeout.value, 2.seconds)
    assertEquals(parsed.tcp.idleTimeout.value, 10.minutes)
    assertEquals(parsed.nat.stun, None)
    assertEquals(
      parsed.tcp.keepAlive,
      Some(
        softphone4s.transport.KeepAlive(90.seconds.positive, 5.seconds.positive)
      )
    )
  }

  test("the codecs are read from configuration, in order") {
    val parsed = ConfigSource
      .string(
        """user = alice
          |password = pw
          |realm = example.com
          |codecs = ["pcma", "pcmu", "g722"]""".stripMargin
      )
      .load[SoftphoneConfig]
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(
      parsed.codecs,
      List(
        softphone4s.config.CodecChoice.Pcma,
        softphone4s.config.CodecChoice.Pcmu,
        softphone4s.config.CodecChoice.G722
      )
    )
    assertEquals(
      parsed.capabilities.codecs.map(_.encoding).toList,
      List("PCMA", "PCMU", "G722")
    )
  }

  test(
    "a name that is not one of ours is read as a codec the program may add"
  ) {
    val parsed = ConfigSource
      .string(
        """user = alice
          |password = pw
          |realm = example.com
          |codecs = ["Opus", "pcmu"]""".stripMargin
      )
      .load[SoftphoneConfig]
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(
      parsed.codecs,
      List(
        softphone4s.config.CodecChoice.Named("opus"),
        softphone4s.config.CodecChoice.Pcmu
      )
    )
  }

  test("an empty codec name is a configuration error") {
    val parsed = ConfigSource
      .string(
        """user = alice
          |password = pw
          |realm = example.com
          |codecs = [" "]""".stripMargin
      )
      .load[SoftphoneConfig]
    assert(parsed.isLeft)
  }

  test("the jitter buffer is read from configuration, each key optional") {
    def load(extra: String) =
      ConfigSource
        .string(s"user = alice\npassword = pw\nrealm = example.com\n$extra")
        .load[SoftphoneConfig]
    val both = load("jitter-buffer { depth = 5, capacity = 20 }")
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals((both.jitterBuffer.depth, both.jitterBuffer.capacity), (5, 20))
    val one = load("jitter-buffer { depth = 2 }")
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals((one.jitterBuffer.depth, one.jitterBuffer.capacity), (2, 12))
    // The most the cushion grows to is twice it, if not said.
    assertEquals(one.jitterBuffer.maxDepth, 4)
    assertEquals(both.jitterBuffer.maxDepth, 10)
    val fixed = load("jitter-buffer { depth = 4, max-depth = 4 }")
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(fixed.jitterBuffer.maxDepth, 4)
    val none = load("").fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(none.jitterBuffer, softphone4s.media.JitterConfig.Default)
  }

  test(
    "a key store type is read in any case, and one that is not known is refused"
  ) {
    def load(extra: String) =
      ConfigSource
        .string(s"user = alice\npassword = pw\nrealm = example.com\n$extra")
        .load[SoftphoneConfig]
    val store = (kind: String) =>
      load(
        s"""tls.trust-store { path = "/x", password = Secret("p"), store-type = "$kind" }"""
      )
        .map(_.tls.trustStore.map(_.storeType))
    assertEquals(store("jks"), Right(Some(KeyStoreType.Jks)))
    assertEquals(store("Pkcs12"), Right(Some(KeyStoreType.Pkcs12)))
    assert(store("PKSC12").isLeft)
  }

  test(
    "the encryption policy is read, off if not said, and one that is not known is refused"
  ) {
    def load(extra: String) =
      ConfigSource
        .string(s"user = alice\npassword = pw\nrealm = example.com\n$extra")
        .load[SoftphoneConfig]
    assertEquals(load("").map(_.srtp), Right(SrtpPolicy.Off))
    assertEquals(
      load("srtp = optional").map(_.srtp),
      Right(SrtpPolicy.Optional)
    )
    assertEquals(
      load("srtp = required").map(_.srtp),
      Right(SrtpPolicy.Required)
    )
    assert(load("srtp = always").isLeft)
  }

  test("the range of RTP ports is read, from 8000 to 17999 if not said") {
    def load(extra: String) =
      ConfigSource
        .string(s"user = alice\npassword = pw\nrealm = example.com\n$extra")
        .load[SoftphoneConfig]
    val none = load("").fold(f => fail(f.prettyPrint()), identity)
    assertEquals(
      (none.baseRtpPort, none.lastRtpPort),
      (port"8000", port"17999")
    )
    // Following the first port when only that is set, and held to the ports there are.
    assertEquals(
      load("base-rtp-port = 20000").map(_.lastRtpPort),
      Right(port"29999")
    )
    assertEquals(
      load("base-rtp-port = 60000").map(_.lastRtpPort),
      Right(port"65535")
    )
    val set = load("base-rtp-port = 30000\nmax-rtp-port = 30099")
      .fold(f => fail(f.prettyPrint()), identity)
    assertEquals((set.baseRtpPort, set.lastRtpPort), (port"30000", port"30099"))
  }

  test("a jitter buffer that cannot work is a configuration error") {
    def load(extra: String) =
      ConfigSource
        .string(s"user = alice\npassword = pw\nrealm = example.com\n$extra")
        .load[SoftphoneConfig]
    assert(load("jitter-buffer { depth = 0 }").isLeft)
    assert(load("jitter-buffer { depth = 8, capacity = 4 }").isLeft)
    assert(load("jitter-buffer { depth = 4, max-depth = 3 }").isLeft)
    assert(
      load("jitter-buffer { depth = 3, max-depth = 20, capacity = 12 }").isLeft
    )
  }

  test("a STUN server is read from configuration") {
    val parsed = ConfigSource
      .string(
        """user = alice
          |password = pw
          |realm = example.com
          |nat {
          |  stun { host = "stun.example.com", port = 19302, attempts = 3 }
          |  rtp-latching = "ignore"
          |}""".stripMargin
      )
      .load[SoftphoneConfig]
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(
      parsed.nat.stun.map(s =>
        (s.host.toString, s.port.value, s.attempts.value, s.retransmit.value)
      ),
      Some(("stun.example.com", 19302, 3, 500.millis))
    )
    assertEquals(parsed.nat.rtpLatching, softphone4s.config.RtpLatching.Ignore)
  }

  test(
    "a STUN setting that makes no sense is an error at load, not at the first round"
  ) {
    def stun(settings: String) =
      ConfigSource
        .string(
          s"""user = alice
             |password = pw
             |realm = example.com
             |nat { stun { $settings } }""".stripMargin
        )
        .load[SoftphoneConfig]
    assert(stun("""host = "stun.example.com", attempts = 0""").isLeft)
    assert(stun("""host = "stun.example.com", attempts = -2""").isLeft)
    assert(stun("""host = "not a host!"""").isLeft)
    assert(stun("""host = "stun.example.com", attempts = 1""").isRight)
  }

  test(
    "the size and connection limits are read, and refuse a number below one"
  ) {
    def load(settings: String) =
      ConfigSource
        .string(
          s"""user = alice
             |password = pw
             |realm = example.com
             |$settings""".stripMargin
        )
        .load[SoftphoneConfig]
    val parsed = load(
      """max-udp-message = 1200
        |tcp { max-incoming = 8, framing { max-header-bytes = 4096, max-body-bytes = 65536 } }""".stripMargin
    ).fold(e => fail(e.prettyPrint()), identity)
    assertEquals(parsed.maxUdpMessage.value, 1200)
    assertEquals(parsed.tcp.maxIncoming.value, 8)
    assertEquals(
      (
        parsed.tcp.framing.maxHeaderBytes.value,
        parsed.tcp.framing.maxBodyBytes.value
      ),
      (4096, 65536)
    )
    List(
      "max-udp-message = 0",
      "max-udp-message = -1",
      "tcp.max-incoming = 0",
      "tcp.framing.max-header-bytes = 0",
      "tcp.framing.max-body-bytes = -5"
    ).foreach(bad => assert(load(bad).isLeft, bad))
  }

  test("an unknown transport in configuration is an error") {
    val parsed = ConfigSource
      .string("user = a\npassword = b\nrealm = c\ntransport = carrier-pigeon")
      .load[SoftphoneConfig]
    assert(parsed.isLeft)
  }

  test("the tls block is read from configuration") {
    val parsed = ConfigSource
      .string(
        """user = alice
          |password = pw
          |realm = example.com
          |transport = tls
          |tls {
          |  trust-store { path = "/etc/sip/trust.p12", password = Secret("t") }
          |  client-certificate { path = "/etc/sip/me.jks", password = Secret("c"), store-type = "JKS" }
          |  min-version = "1.3"
          |  hostname-verification = "skip"
          |  cipher-suites = ["TLS_AES_128_GCM_SHA256"]
          |  connection { idle-timeout = 1 minute }
          |}""".stripMargin
      )
      .load[SoftphoneConfig]
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(parsed.transport, TransportProtocol.Tls)
    assertEquals(
      parsed.tls.trustStore.map(_.path.toString),
      Some("/etc/sip/trust.p12")
    )
    assertEquals(
      parsed.tls.trustStore.map(_.storeType),
      Some(KeyStoreType.Pkcs12)
    )
    assertEquals(
      parsed.tls.clientCertificate.map(_.storeType),
      Some(KeyStoreType.Jks)
    )
    assertEquals(parsed.tls.minVersion, TlsVersion.Tls13)
    assertEquals(parsed.tls.hostnameVerification, HostnameVerification.Skip)
    assertEquals(parsed.tls.cipherSuites, Some(List("TLS_AES_128_GCM_SHA256")))
    assertEquals(parsed.tls.connection.idleTimeout.value, 1.minute)
  }

  test("an unknown TLS version or verification mode is an error") {
    List("min-version = \"1.0\"", "hostname-verification = \"maybe\"").foreach {
      line =>
        val parsed = ConfigSource
          .string(s"user = a\npassword = b\nrealm = c\ntls { $line }")
          .load[SoftphoneConfig]
        assert(parsed.isLeft, line)
    }
  }

  test("DNS servers are read as address or address:port, defaulting to 53") {
    val parsed = ConfigSource
      .string(
        """user = a
          |password = b
          |realm = c
          |dns { servers = ["10.0.0.53", "192.0.2.1:5353", "[2001:db8::53]:53"], timeout = 2 seconds }""".stripMargin
      )
      .load[SoftphoneConfig]
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(
      parsed.dns.servers.map(_.toString),
      List("10.0.0.53:53", "192.0.2.1:5353", "[2001:db8::53]:53")
    )
    assertEquals(parsed.dns.timeout.value, 2.seconds)
    assertEquals(
      SoftphoneConfig(sipUser("a"), Secret("b"), host"c").dns.servers,
      Nil
    )
  }

  test("a DNS server that is not an address is an error") {
    val parsed = ConfigSource
      .string(
        "user = a\npassword = b\nrealm = c\ndns { servers = [\"dns.example.com\"] }"
      )
      .load[SoftphoneConfig]
    assert(parsed.isLeft)
  }

  test("listening is read from configuration") {
    val parsed = ConfigSource
      .string(
        """user = a
          |password = b
          |realm = c
          |listen = [tcp, tls]
          |tls-port = 5071
          |tls {
          |  server-certificate { path = "/etc/sip/server.p12", password = Secret("s") }
          |  client-authentication = "require"
          |}""".stripMargin
      )
      .load[SoftphoneConfig]
      .fold(failures => fail(failures.prettyPrint()), identity)
    assertEquals(
      parsed.listen,
      Set(TransportProtocol.Tcp, TransportProtocol.Tls)
    )
    assertEquals(parsed.tlsPort.value, 5071)
    assertEquals(parsed.tls.clientAuthentication, ClientAuthentication.Require)
    assertEquals(
      parsed.tls.serverCertificate.map(_.path.toString),
      Some("/etc/sip/server.p12")
    )
  }

}
