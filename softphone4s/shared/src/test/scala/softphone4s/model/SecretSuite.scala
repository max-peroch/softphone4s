package softphone4s.model

import cats.syntax.all.*
import com.comcast.ip4s.host
import fs2.io.file.Path
import munit.FunSuite
import softphone4s.SipLiterals.sipUser
import softphone4s.config.{AccountConfig, SoftphoneConfig}
import softphone4s.transport.KeyStoreConfig

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
    val store   = KeyStoreConfig(Path("/keys.p12"), Secret(text))
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
}
