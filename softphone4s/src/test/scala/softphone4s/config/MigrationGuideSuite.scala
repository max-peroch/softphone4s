package softphone4s.config

import com.comcast.ip4s.*
import munit.FunSuite
import pureconfig.ConfigSource
import softphone4s.model.{Secret, User}

/** What MIGRATION.md tells a user of 0.1.0 to write, and to expect. */
class MigrationGuideSuite extends FunSuite {

  test("the configuration the guide builds is a configuration") {
    val config = User("alice").map(user =>
      SoftphoneConfig(
        user = user,
        password = Secret("secret"),
        realm = host"sip.example.com"
      )
    )
    assertEquals(config.map(_.realm), Right(host"sip.example.com"))
    assertEquals(config.map(_.password.toString), Right("***"))
  }

  test(
    "a configuration file written for 0.1.0 still loads, with the same keys"
  ) {
    val loaded = ConfigSource
      .string(
        """user = alice
          |password = secret
          |realm = "sip.example.com"
          |local-sip-port = 5070
          |""".stripMargin
      )
      .load[SoftphoneConfig]
    assertEquals(loaded.map(_.realm), Right(host"sip.example.com"))
    assertEquals(loaded.map(_.localSipPort), Right(port"5070"))
    assertEquals(loaded.map(_.password.reveal), Right("secret"))
  }

  test("a value that is not valid fails when the file is loaded") {
    val loaded = ConfigSource
      .string("""user = "not a user"
                |password = secret
                |realm = "sip.example.com"
                |""".stripMargin)
      .load[SoftphoneConfig]
    assert(loaded.isLeft, loaded.toString)
  }
}
