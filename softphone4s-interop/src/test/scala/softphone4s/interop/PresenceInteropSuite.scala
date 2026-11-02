package softphone4s.interop

import softphone4s.model.Callee
import softphone4s.presence.Presence

import scala.concurrent.duration.*

class PresenceInteropSuite extends InteropSuite {

  test("a presence subscription follows a device as it becomes busy and free") {
    phoneAndServer.use { (phone, asterisk) =>
      phone.register.use { registration =>
        for {
          _      <- registration.awaitRegistered.timeout(20.seconds)
          _      <- asterisk.setDeviceState("carol", "NOT_INUSE")
          result <- phone.subscriptions
            .watch(
              Callee("carol").fold(e => fail(e), identity),
              Presence.Package
            )
            .use { watched =>
              for {
                active <- watched.subscription.awaitActive.timeout(15.seconds)
                first  <- watched.states.head.compile.lastOrError
                  .timeout(15.seconds)
                _     <- asterisk.setDeviceState("carol", "UNAVAILABLE")
                later <- watched.states
                  .collectFirst {
                    case Right(p)
                        if p.tuples.exists(
                          _.basic.exists(_.toString == "Closed")
                        ) =>
                      p
                  }
                  .compile
                  .lastOrError
                  .timeout(20.seconds)
              } yield (active, first, later)
            }
        } yield {
          val (active, first, later) = result
          assertEquals(active, Right(()))
          assert(first.exists(_.isOpen), first.toString)
          assert(!later.isOpen, later.toString)
        }
      }
    }
  }
}
