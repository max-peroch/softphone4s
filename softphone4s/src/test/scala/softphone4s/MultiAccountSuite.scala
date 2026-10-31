package softphone4s

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import pureconfig.ConfigSource
import softphone4s.SipLiterals.*
import softphone4s.config.{AccountConfig, KeyStoreType, SoftphoneConfig}
import softphone4s.model.*
import softphone4s.srtp.SrtpPolicy
import softphone4s.transport.{
  ClientAuthentication,
  HostnameVerification,
  TlsVersion
}
import softphone4s.transport.{Inbound, SipTransport, Transport}

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** A softphone holding two accounts over real loopback UDP, with a bare
  * transport playing the other side.
  */
class MultiAccountSuite extends CatsEffectSuite {

  private val ours   = port"19090"
  private val theirs = port"19091"
  private val target = SocketAddress(ip"127.0.0.1", ours)

  private val second = AccountConfig("alt", "pw2", "127.0.0.1", theirs)

  private def config(more: List[AccountConfig]) =
    SoftphoneConfig(
      user = "test",
      password = "test",
      realm = "127.0.0.1",
      localSipPort = ours,
      serverPort = theirs,
      baseRtpPort = port"19400",
      bindAddress = ip"127.0.0.1",
      hangupTimeout = 2.seconds,
      additionalAccounts = more
    )

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  private def scenario(
      more: List[AccountConfig] = List(second)
  ): Resource[IO, (SoftphoneStreaming[IO], Transport[IO])] =
    (
      SoftphoneStreaming
        .resource[IO](config(more), audio, NoOpLogger.impl[IO]),
      SipTransport.udp[IO](
        SocketAddress(ip"127.0.0.1", theirs),
        NoOpLogger.impl[IO]
      )
    ).tupled

  private val bob = Callee("bob@127.0.0.1").fold(err => fail(err), identity)

  private val offer =
    "v=0\r\no=- 1 1 IN IP4 127.0.0.1\r\ns=-\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\n" +
      "m=audio 40000 RTP/AVP 0\r\na=rtpmap:0 PCMU/8000\r\n"

  private def inviteTo(user: String, id: String): SipRequest =
    SipRequest(
      SipMethod.Invite,
      uri(s"sip:$user@127.0.0.1:19090"),
      SipHeaders(
        callId = callId(s"$id@host"),
        from = nameAddr("<sip:bob@127.0.0.1>;tag=ft"),
        to = nameAddr(s"<sip:$user@127.0.0.1>"),
        vias = NonEmptyList.one(
          via(s"SIP/2.0/UDP 127.0.0.1:19091;branch=z9hG4bK$id")
        ),
        cseq = cseq(1, SipMethod.Invite),
        contacts = List(nameAddr("<sip:bob@127.0.0.1:19091>")),
        contentType = Some(MediaType.Sdp)
      ),
      offer
    )

  private def requests(peer: Transport[IO], method: SipMethod, n: Int) =
    peer.messages
      .collect { case Inbound(_, r: SipRequest) if r.method == method => r }
      .take(n.toLong)
      .compile
      .toList
      .timeout(5.seconds)

  private def finalStatus(peer: Transport[IO]) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipResponse) if r.statusCode >= 200 => r.statusCode
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  private def firstCall(phone: SoftphoneStreaming[IO]) =
    phone.incoming.head.compile.lastOrError.timeout(5.seconds)

  // ---- The accounts ---------------------------------------------------------------

  test("the default account comes first, then the others in order") {
    scenario().use { (phone, _) =>
      IO(
        assertEquals(
          phone.accounts.toList.map(_.render),
          List("sip:test@127.0.0.1", "sip:alt@127.0.0.1")
        )
      )
    }
  }

  test("two accounts cannot share a user") {
    val duplicate = second.copy(user = "test", realm = "other.example")
    SoftphoneStreaming
      .resource[IO](config(List(duplicate)), audio, NoOpLogger.impl[IO])
      .use_
      .attempt
      .map(r => assertEquals(r, Left(SoftphoneError.DuplicateAccount("test"))))
  }

  test("an account that is not a SIP address stops the softphone being built") {
    SoftphoneStreaming
      .resource[IO](
        config(List(second.copy(realm = "not a host"))),
        audio,
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map {
        case Left(SoftphoneError.InvalidSipUri(_, _)) => ()
        case other => fail(s"unexpected $other")
      }
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
      parsed.accounts.map(a => (a.user, a.realm, a.serverPort.value)).toList,
      List(("alice", "example.com", 5060), ("bob", "example.org", 5070))
    )
  }

  test("a single-account configuration has just the one account") {
    assertEquals(SoftphoneConfig("a", "b", "c").accounts.size, 1)
  }

  // ---- Incoming calls -------------------------------------------------------------

  test("an incoming call says which account was called") {
    scenario().use { (phone, peer) =>
      def account(user: String, id: String) =
        peer.send(target, inviteTo(user, id)) >> firstCall(phone).map(_.account)
      for {
        alt  <- account("alt", "a1")
        main <- account("test", "a2")
      } yield {
        assertEquals(alt.render, "sip:alt@127.0.0.1")
        assertEquals(main.render, "sip:test@127.0.0.1")
      }
    }
  }

  test("with several accounts a call to an unknown user is refused 404") {
    scenario().use { (phone, peer) =>
      for {
        _      <- peer.send(target, inviteTo("nobody", "n1"))
        status <- finalStatus(peer)
        none   <- phone.incoming.head.compile.last.timeout(300.millis).attempt
      } yield {
        assertEquals(status, 404)
        assert(none.isLeft, "no call must be offered")
      }
    }
  }

  test("with a single account a call is taken whatever user it names") {
    scenario(more = Nil).use { (phone, peer) =>
      peer.send(target, inviteTo("anyone", "s1")) >>
        firstCall(phone).map(c =>
          assertEquals(c.account.render, "sip:test@127.0.0.1")
        )
    }
  }

  // ---- Outgoing calls -------------------------------------------------------------

  test("callAs places the call as that account; call uses the default") {
    scenario().use { (phone, peer) =>
      def invitedAs(user: String, use: Resource[IO, Call[IO]]) =
        use.use(_ =>
          peer.messages
            .collectFirst {
              case Inbound(_, r: SipRequest)
                  if r.method == SipMethod.Invite &&
                    r.headers.from.uri.user.map(_.value).contains(user) =>
                r
            }
            .compile
            .lastOrError
            .timeout(5.seconds)
        )
      for {
        alt  <- invitedAs("alt", phone.callAs(phone.accounts.last, bob))
        main <- invitedAs("test", phone.call(bob))
      } yield {
        assertEquals(alt.headers.from.uri.render, "sip:alt@127.0.0.1")
        assertEquals(
          alt.headers.contacts.map(_.uri.user.map(_.value)),
          List(Some("alt"))
        )
        assertEquals(main.headers.from.uri.render, "sip:test@127.0.0.1")
        assertEquals(
          main.headers.contacts.map(_.uri.user.map(_.value)),
          List(Some("test"))
        )
      }
    }
  }

  test("an account the softphone does not hold is refused") {
    scenario().use { (phone, _) =>
      val stranger = AccountId(uri("sip:nobody@127.0.0.1"))
      phone
        .callAs(stranger, bob)
        .use_
        .attempt
        .map(r =>
          assertEquals(
            r,
            Left(SoftphoneError.UnknownAccount("sip:nobody@127.0.0.1"))
          )
        ) >>
        phone
          .registerAs(stranger)
          .use_
          .attempt
          .map(r =>
            assertEquals(
              r,
              Left(SoftphoneError.UnknownAccount("sip:nobody@127.0.0.1"))
            )
          )
    }
  }

  // ---- Registration ---------------------------------------------------------------

  test("each account registers on its own, with its own Contact and Call-ID") {
    scenario().use { (phone, peer) =>
      (
        phone.registerAs(phone.accounts.head),
        phone.registerAs(phone.accounts.last)
      ).tupled
        .use { _ =>
          requests(peer, SipMethod.Register, 2).map { registers =>
            assertEquals(
              registers.map(_.headers.to.uri.render).toSet,
              Set("sip:test@127.0.0.1", "sip:alt@127.0.0.1")
            )
            assertEquals(registers.map(_.headers.callId).distinct.size, 2)
            assertEquals(
              registers
                .flatMap(_.headers.contacts.flatMap(_.uri.user.map(_.value)))
                .toSet,
              Set("test", "alt")
            )
          }
        }
    }
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
    assertEquals(parsed.tcp.connectTimeout, 2.seconds)
    assertEquals(parsed.tcp.idleTimeout, 10.minutes)
    assertEquals(parsed.nat.stun, None)
    assertEquals(
      parsed.tcp.keepAlive,
      Some(softphone4s.transport.KeepAlive(90.seconds, 5.seconds))
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
        s"""tls.trust-store { path = "/x", password = "p", store-type = "$kind" }"""
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
        (s.host, s.port.value, s.attempts, s.retransmit)
      ),
      Some(("stun.example.com", 19302, 3, 500.millis))
    )
    assertEquals(parsed.nat.rtpLatching, softphone4s.config.RtpLatching.Ignore)
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
          |  trust-store { path = "/etc/sip/trust.p12", password = "t" }
          |  client-certificate { path = "/etc/sip/me.jks", password = "c", store-type = "JKS" }
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
    assertEquals(parsed.tls.connection.idleTimeout, 1.minute)
  }

  test("TLS is verified by default, from 1.2 up") {
    val defaults = SoftphoneConfig("a", "b", "c").tls
    assertEquals(defaults.hostnameVerification, HostnameVerification.Verify)
    assertEquals(defaults.minVersion, TlsVersion.Tls12)
    assertEquals(defaults.trustStore, None)
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
    assertEquals(parsed.dns.timeout, 2.seconds)
    assertEquals(SoftphoneConfig("a", "b", "c").dns.servers, Nil)
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
          |  server-certificate { path = "/etc/sip/server.p12", password = "s" }
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

  test(
    "by default we listen for nothing but UDP, and ask for no client certificate"
  ) {
    val defaults = SoftphoneConfig("a", "b", "c")
    assertEquals(defaults.listen, Set.empty[TransportProtocol])
    assertEquals(defaults.tls.clientAuthentication, ClientAuthentication.Off)
  }
}
