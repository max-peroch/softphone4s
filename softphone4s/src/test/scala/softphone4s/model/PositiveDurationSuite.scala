package softphone4s.model

import munit.FunSuite
import pureconfig.ConfigSource
import softphone4s.config.{AccountConfig, SoftphoneConfig}

import scala.concurrent.duration.*

class PositiveDurationSuite extends FunSuite {

  test("zero and a negative duration are refused, the smallest one is not") {
    assert(PositiveDuration(Duration.Zero).isLeft)
    assert(PositiveDuration((-1).second).isLeft)
    assert(PositiveDuration((-1).nanosecond).isLeft)
    assertEquals(
      PositiveDuration(1.nanosecond).map(_.value),
      Right(1.nanosecond)
    )
    assertEquals(PositiveDuration(5.seconds).map(_.value), Right(5.seconds))
  }

  private def loadWith(extra: String) =
    ConfigSource
      .string(
        s"""user = alice
           |password = pw
           |realm = example.com
           |nat.stun.host = "stun.example.com"
           |tcp.keep-alive {}
           |$extra""".stripMargin
      )
      .load[SoftphoneConfig]

  private def load(path: String, value: String) = loadWith(s"$path = $value")

  private val settings: List[String] = List(
    "hangup-timeout",
    "invite-timeout",
    "register-expires",
    "answer-timeout",
    "tcp.connect-timeout",
    "tcp.idle-timeout",
    "tls.connection.connect-timeout",
    "tls.connection.idle-timeout",
    "tcp.keep-alive.interval",
    "tcp.keep-alive.timeout",
    "dns.timeout",
    "nat.stun.retransmit",
    "nat.stun.refresh"
  )

  test("every duration setting refuses zero and a negative value at load") {
    settings.foreach { path =>
      assert(load(path, "1 second").isRight, s"$path = 1 second was refused")
      assert(load(path, "0 seconds").isLeft, s"$path = 0 seconds was taken")
      assert(load(path, "-5 seconds").isLeft, s"$path = -5 seconds was taken")
    }
  }

  test("a registration of an account refuses a zero lifetime too") {
    def account(lifetime: String) =
      ConfigSource
        .string(
          s"""user = alice
             |password = pw
             |realm = example.com
             |additional-accounts = [
             |  { user = bob, password = pw, realm = example.org,
             |    register-expires = "$lifetime" }
             |]""".stripMargin
        )
        .load[SoftphoneConfig]
    assert(account("1 hour").isRight)
    assert(account("0 seconds").isLeft)
    assert(account("-1 hour").isLeft)
  }

  test("every default is above zero: the unchecked constants are right") {
    val soft = loadWith("").fold(e => fail(e.prettyPrint()), identity)
    val stun = soft.nat.stun.getOrElse(fail("no stun"))
    val keep = soft.tcp.keepAlive.getOrElse(fail("no keep-alive"))
    val defaults: List[(String, PositiveDuration)] = List(
      "hangupTimeout"   -> soft.hangupTimeout,
      "inviteTimeout"   -> soft.inviteTimeout,
      "registerExpires" -> soft.registerExpires,
      "answerTimeout"   -> soft.answerTimeout,
      "tcp.connect"     -> soft.tcp.connectTimeout,
      "tcp.idle"        -> soft.tcp.idleTimeout,
      "tls.connect"     -> soft.tls.connection.connectTimeout,
      "tls.idle"        -> soft.tls.connection.idleTimeout,
      "dns.timeout"     -> soft.dns.timeout,
      "keepAlive.int"   -> keep.interval,
      "keepAlive.out"   -> keep.timeout,
      "stun.retransmit" -> stun.retransmit,
      "stun.refresh"    -> stun.refresh,
      "account.expires" -> AccountConfig(
        soft.user,
        soft.password,
        soft.realm
      ).registerExpires
    )
    defaults.foreach { (name, d) =>
      assert(PositiveDuration(d.value).isRight, s"the default of $name")
    }
  }

  test("a registration asks for whole seconds, and never for zero") {
    assertEquals(
      Expires.forRegistration(
        PositiveDuration(1.hour).fold(sys.error, identity)
      ),
      Expires(3600).fold(sys.error, identity)
    )
    assertEquals(
      Expires
        .forRegistration(PositiveDuration(400.millis).fold(sys.error, identity))
        .value,
      1
    )
    assertEquals(
      Expires
        .forRegistration(
          PositiveDuration(1500.millis).fold(sys.error, identity)
        )
        .value,
      1
    )
  }
}
