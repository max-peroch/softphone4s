package softphone4s.interop

import cats.syntax.all.*
import softphone4s.messaging.MessageContent
import softphone4s.model.Callee

import scala.concurrent.duration.*

class MessageInteropSuite extends InteropSuite {

  private def callee(name: String) = Callee(name).fold(e => fail(e), identity)

  test("a message from one account reaches another through Asterisk") {
    phoneAndServer.use { (phone, _) =>
      val alice = phone.accounts.head
      val bob   = phone.accounts.last
      (phone.registerAs(alice), phone.registerAs(bob)).tupled.use { (a, b) =>
        for {
          _ <- (a.awaitRegistered, b.awaitRegistered).parTupled
            .timeout(20.seconds)
          sent <- phone.messages.sendAs(
            alice,
            callee("bob"),
            MessageContent.plain("hello bob")
          )
          got <- phone.messages.incoming.head.compile.lastOrError
            .timeout(15.seconds)
        } yield {
          assertEquals(sent, Right(()))
          assertEquals(got.account, bob)
          assertEquals(got.content.text, "hello bob")
          assertEquals(got.from.uri.user.map(_.value), Some("alice"))
        }
      }
    }
  }

  test("text with accents and symbols arrives as it was sent") {
    phoneAndServer.use { (phone, _) =>
      val alice = phone.accounts.head
      val bob   = phone.accounts.last
      val text  = "héllo bob ✓ — ça va ?"
      (phone.registerAs(alice), phone.registerAs(bob)).tupled.use { (a, b) =>
        for {
          _ <- (a.awaitRegistered, b.awaitRegistered).parTupled
            .timeout(20.seconds)
          _ <- phone.messages.sendAs(
            alice,
            callee("bob"),
            MessageContent.plain(text)
          )
          got <- phone.messages.incoming.head.compile.lastOrError
            .timeout(15.seconds)
        } yield assertEquals(got.content.text, text)
      }
    }
  }
}
