package softphone4s.model

import cats.syntax.all.*
import com.comcast.ip4s.host
import munit.FunSuite
import pureconfig.ConfigSource
import softphone4s.SipLiterals.sipUser
import softphone4s.config.{AccountConfig, SoftphoneConfig}
import softphone4s.transport.KeyStoreConfig

import java.nio.file.Path

class SecretSuite extends FunSuite {

  private val text = "s3cr3t-pw"

  test("a secret prints as stars however it is printed") {
    val secret = Secret(text)
    assertEquals(secret.toString, "***")
    assertEquals(secret.show, "***")
    assertEquals(s"password is $secret", "password is ***")
  }

  test("the text is had only by asking for it") {
    assertEquals(Secret(text).reveal, text)
    assertEquals(Secret("").reveal, "")
  }

  test("two secrets are equal if their text is") {
    assertEquals(Secret(text), Secret(text))
    assertNotEquals(Secret(text), Secret(text + "x"))
    assertNotEquals[Any, Any](Secret(text), text)
    assertEquals(Secret(text).hashCode, Secret(text).hashCode)
  }

  test(
    "nothing that holds one prints it: the configuration, the account, the key store, the credentials"
  ) {
    val config =
      SoftphoneConfig(sipUser("alice"), Secret(text), host"example.org")
    val account = AccountConfig(sipUser("bob"), Secret(text), host"example.org")
    val store   = KeyStoreConfig(Path.of("/keys.p12"), Secret(text))
    val creds   = Credentials("alice", Secret(text))
    List(
      config.toString,
      account.toString,
      store.toString,
      creds.toString,
      config.accounts.toString
    )
      .foreach(printed => assert(!printed.contains(text), printed))
    // The copy of a configuration that was printed to a log has all else in it.
    assert(config.toString.contains("alice"), config.toString)
  }

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
