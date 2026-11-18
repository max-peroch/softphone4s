package softphone4s.loopback

import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite
import softphone4s.model.SipMethod
import softphone4s.transaction.*

import scala.concurrent.duration.*

/** What a real call, over real UDP, leaves in the transaction traces. */
class CallTracingSuite extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 2.minutes

  test(
    "a call that is answered and hung up: an INVITE and a BYE span on each side"
  ) {
    for {
      alice <- RecordingTracing()
      bob   <- RecordingTracing()
      _     <- Loopback
        .pair(aliceTracing = Some(alice), bobTracing = Some(bob))
        .use { p =>
          p.alice.call(p.callee()).allocated.flatMap { (call, hangUp) =>
            for {
              incoming <- p.bob.incoming.head.compile.lastOrError
                .timeout(10.seconds)
              _ <- incoming.accept.use { bobCall =>
                (call.awaitPickup, bobCall.awaitPickup).parTupled
                  .timeout(10.seconds) >>
                  hangUp >> bobCall.awaitHangup.timeout(10.seconds)
              }
            } yield ()
          }
        }
      _           <- IO.sleep(500.millis)
      aliceClient <- alice.ended(TransactionSide.Client)
      bobServer   <- bob.ended(TransactionSide.Server)
      aliceBegan  <- alice.began(TransactionSide.Client)
      bobBegan    <- bob.began(TransactionSide.Server)
    } yield {
      def outcomes(spans: Vector[(TransactionInfo, TransactionOutcome)]) =
        spans.map((info, outcome) => info.method -> outcome).toSet

      assert(
        outcomes(aliceClient).contains(
          SipMethod.Invite -> TransactionOutcome.Answered(200)
        ),
        aliceClient.toString
      )
      assert(
        outcomes(aliceClient).contains(
          SipMethod.Bye -> TransactionOutcome.Answered(200)
        ),
        aliceClient.toString
      )
      assert(
        outcomes(bobServer).contains(
          SipMethod.Invite -> TransactionOutcome.Responded(200)
        ),
        bobServer.toString
      )
      assert(
        outcomes(bobServer).contains(
          SipMethod.Bye -> TransactionOutcome.Responded(200)
        ),
        bobServer.toString
      )
      // each began once and ended once: no span is left open or ended twice
      assertEquals(aliceBegan.size, aliceClient.size)
      assertEquals(bobBegan.size, bobServer.size)
      // the INVITE is one transaction seen from both ends: one call-id, one branch
      def invite(spans: Vector[TransactionInfo]) =
        spans
          .filter(_.method == SipMethod.Invite)
          .map(i => (i.callId, i.branch))
      assertEquals(invite(aliceBegan).size, 1)
      assertEquals(invite(aliceBegan), invite(bobBegan))
    }
  }
}
