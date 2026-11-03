package softphone4s.loopback

import softphone4s.SipLiterals.*
import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite
import softphone4s.Call
import softphone4s.model.SoftphoneError

import LossyRelay.{Fault, Plan, Way}
import scala.concurrent.duration.*

/** Calls whose set-up crosses a network that loses, duplicates and reorders SIP
  * messages: the transactions must make up for it, and a message that arrives
  * twice or late must change nothing.
  */
class LossySignallingSuite extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 3.minutes

  /** Alice calls Bob through the relay; `onCall` is given what each side has.
    */
  private def throughRelay[A](
      plan: Plan,
      settings: Loopback.Settings = Loopback.Settings()
  )(
      onCall: (
          LossyRelay.Relay,
          Loopback.Pair,
          Call[IO],
          IO[Unit]
      ) => IO[A]
  ): IO[A] =
    LossyRelay.resource(plan).use { relay =>
      Loopback.pair(settings).use { p =>
        p.alice
          .call(p.callee(port = LossyRelay.CallerSide))
          .allocated
          .flatMap((call, hangUp) => onCall(relay, p, call, hangUp))
      }
    }

  /** Bob answers the call that comes in and both sides see it connected. */
  private def connected(p: Loopback.Pair, call: Call[IO]): IO[Call[IO]] =
    for {
      incoming <- p.bob.incoming.head.compile.lastOrError.timeout(20.seconds)
      _        <- incoming.ringing
      (bobCall, _) <- incoming.accept.allocated
      _ <- (call.awaitPickup, bobCall.awaitPickup).parTupled.timeout(20.seconds)
    } yield bobCall

  /** Bob is not offered a second call in the next seconds. */
  private def noSecondCall(p: Loopback.Pair): IO[Unit] =
    p.bob.incoming
      .interruptAfter(3.seconds)
      .compile
      .toList
      .map(extra => assertEquals(extra.size, 0, "a copy came in as a new call"))

  test(
    "an INVITE that is lost twice is sent again, and the call connects once"
  ) {
    val plan: Plan = {
      case LossyRelay.Seen(Way.ToCallee, "INVITE", n) if n <= 2 => Fault.Drop
      case _                                                    => Fault.Pass
    }
    throughRelay(plan) { (relay, p, call, hangUp) =>
      for {
        bobCall <- connected(p, call)
        _       <- noSecondCall(p)
        invites <- relay.count(Way.ToCallee, "INVITE")
        _       <- hangUp
        _       <- bobCall.awaitHangup.timeout(10.seconds)
      } yield assert(invites >= 3, s"$invites INVITEs reached the relay")
    }
  }

  test("a 200 that is lost is sent again until the call connects") {
    val plan: Plan = {
      case LossyRelay.Seen(Way.ToCaller, "200", 1) => Fault.Drop
      case _                                       => Fault.Pass
    }
    throughRelay(plan) { (relay, p, call, hangUp) =>
      for {
        bobCall <- connected(p, call)
        oks     <- relay.count(Way.ToCaller, "200")
        _       <- hangUp
        _       <- bobCall.awaitHangup.timeout(10.seconds)
      } yield assert(oks >= 2, s"$oks 200s reached the relay")
    }
  }

  test(
    "every message delivered twice: one call, one pickup, and it ends cleanly"
  ) {
    throughRelay(_ => Fault.Duplicate) { (relay, p, call, hangUp) =>
      for {
        bobCall   <- connected(p, call)
        _         <- noSecondCall(p)
        _         <- hangUp
        _         <- bobCall.awaitHangup.timeout(10.seconds)
        seen      <- relay.seen
        delivered <- relay.delivered
      } yield {
        // both ways: the INVITE went out twice, and so did what came back
        assert(seen.exists(_.way == Way.ToCallee), seen.toString)
        assert(seen.exists(_.way == Way.ToCaller), seen.toString)
        assertEquals(delivered.size, 2 * seen.size)
      }
    }
  }

  test("a 180 that arrives after the 200 changes nothing") {
    val plan: Plan = {
      case LossyRelay.Seen(Way.ToCaller, "180", _) => Fault.Delay(1500.millis)
      case _                                       => Fault.Pass
    }
    throughRelay(plan) { (relay, p, call, hangUp) =>
      for {
        incoming <- p.bob.incoming.head.compile.lastOrError.timeout(20.seconds)
        _        <- incoming.ringing
        (bobCall, _) <- incoming.accept.allocated
        _            <- (call.awaitPickup, bobCall.awaitPickup).parTupled
          .timeout(20.seconds)
        _         <- IO.sleep(2.seconds) // the late 180 has come by now
        delivered <- relay.delivered
        _         <- hangUp
        _         <- bobCall.awaitHangup.timeout(10.seconds)
      } yield {
        val back = delivered.filter(_.way == Way.ToCaller).map(_.kind)
        val ok   = back.indexOf("200")
        val ring = back.indexOf("180")
        assert(ok >= 0 && ring >= 0, back.toString)
        assert(ring > ok, s"the 180 did not come after the 200: $back")
      }
    }
  }

  test(
    "a call whose INVITEs are all lost times out, and Bob never hears of it"
  ) {
    val plan: Plan = {
      case LossyRelay.Seen(Way.ToCallee, _, _) => Fault.Drop
      case _                                   => Fault.Pass
    }
    throughRelay(plan, Loopback.Settings(inviteTimeout = 3.seconds.positive)) {
      (relay, p, call, _) =>
        for {
          result  <- call.awaitPickup.attempt.timeout(20.seconds)
          invites <- relay.count(Way.ToCallee, "INVITE")
          _       <- noSecondCall(p)
        } yield {
          assertEquals(result, Left(SoftphoneError.CallTimedOut))
          assert(invites >= 2, s"only $invites INVITEs were sent in 3 seconds")
        }
    }
  }
}
