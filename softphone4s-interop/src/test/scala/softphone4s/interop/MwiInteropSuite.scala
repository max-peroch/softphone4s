package softphone4s.interop

import cats.effect.IO
import softphone4s.model.Callee
import softphone4s.subscription.{MessageClass, MessageSummary}

import scala.concurrent.duration.*

class MwiInteropSuite extends InteropSuite {

  test(
    "a message-waiting subscription is told of the mailbox, and of a message that arrives"
  ) {
    phoneAndServer.use { (phone, asterisk) =>
      phone.register.use { registration =>
        for {
          _      <- registration.awaitRegistered.timeout(20.seconds)
          result <- phone.subscriptions
            .watch(
              Callee("alice").fold(e => fail(e), identity),
              MessageSummary.Package
            )
            .use { watched =>
              for {
                active <- watched.subscription.awaitActive.timeout(15.seconds)
                status <- watched.subscription.status.get
                _     <- IO(assertEquals(active, Right(()), s"status: $status"))
                first <- watched.states.head.compile.lastOrError
                  .timeout(15.seconds)
                _     <- asterisk.leaveVoicemail("alice", 0, 2)
                later <- watched.states
                  .collectFirst { case Right(s) if s.waiting => s }
                  .compile
                  .lastOrError
                  .timeout(20.seconds)
              } yield (active, first, later)
            }
        } yield {
          val (active, first, later) = result
          assertEquals(active, Right(()))
          assertEquals(first.map(_.waiting), Right(false))
          assertEquals(
            later.counts.get(MessageClass.Voice).map(_.newMessages),
            Some(2)
          )
        }
      }
    }
  }
}
