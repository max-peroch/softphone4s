package softphone4s.model

import munit.FunSuite
import pureconfig.ConfigSource
import softphone4s.config.PureConfigReaders.given
import softphone4s.config.SoftphoneConfig

class SecretReadingSuite extends FunSuite {

  private val text = "s3cr3t-pw"

  test("a password in a configuration file is read as a secret") {
    val parsed = ConfigSource
      .string(
        """user = "alice"
          |password = "s3cr3t-pw"
          |realm = "example.org"
          |additional-accounts = [ { user = "bob", password = "other", realm = "example.org" } ]
          |tls.client-certificate = { path = "/c.p12", password = "keypass", store-type = "PKCS12" }""".stripMargin
      )
      .load[SoftphoneConfig]
    val config = parsed.fold(e => fail(e.prettyPrint()), identity)
    assertEquals(config.password, Secret(text))
    assertEquals(
      config.additionalAccounts.map(_.password),
      List(Secret("other"))
    )
    assertEquals(
      config.tls.clientCertificate.map(_.password),
      Some(Secret("keypass"))
    )
    assert(!config.toString.contains(text), config.toString)
    assert(!config.toString.contains("keypass"), config.toString)
  }
}
