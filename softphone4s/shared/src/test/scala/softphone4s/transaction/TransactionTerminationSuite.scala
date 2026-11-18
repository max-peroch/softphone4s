package softphone4s.transaction

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import softphone4s.transaction.TransactionExamples.*

/** Whatever happens, a terminated transaction stays terminated and silent, and
  * no machine ever fails on an event it does not expect.
  */
class TransactionTerminationSuite extends ScalaCheckSuite {

  private val config = TransactionConfig()

  private val timers = Gen.oneOf(TransactionTimer.values.toList)
  private val codes  = Gen.oneOf(100, 180, 183, 200, 302, 404, 486, 503, 603)

  private def clientEvents(of: softphone4s.model.SipRequest): Gen[ClientEvent] =
    Gen.oneOf(
      codes.map(c => ClientEvent.Response(response(of, c))),
      timers.map(ClientEvent.TimerFired(_)),
      Gen.const(ClientEvent.TransportError)
    )

  private def serverEvents(of: softphone4s.model.SipRequest): Gen[ServerEvent] =
    Gen.oneOf(
      codes.map(c => ServerEvent.Respond(response(of, c))),
      timers.map(ServerEvent.TimerFired(_)),
      Gen.const(ServerEvent.RequestRetransmitted),
      Gen.const(ServerEvent.AckReceived),
      Gen.const(ServerEvent.TransportError)
    )

  private val reliability = Gen.oneOf(Reliability.values.toList)

  private def runs[S, E, A](
      events: List[E],
      initial: S,
      transition: E => cats.data.State[S, List[A]],
      terminated: S => Boolean
  ): Boolean = {
    val (_, ok) = events.foldLeft((initial, true)) {
      case ((state, ok), event) =>
        val (next, actions) = transition(event).run(state).value
        val stayedSilent    =
          !terminated(state) || (terminated(next) && actions.isEmpty)
        (next, ok && stayedSilent)
    }
    ok
  }

  property("an INVITE client stays terminated and silent once terminated") {
    forAll(reliability, Gen.listOfN(25, clientEvents(invite))) {
      (rel, events) =>
        assert(
          runs(
            events,
            InviteClientTransaction.start(invite, rel, config)._1,
            InviteClientTransaction.transition,
            _ == InviteClientState.Terminated
          )
        )
    }
  }

  property("a non-INVITE client stays terminated and silent once terminated") {
    forAll(reliability, Gen.listOfN(25, clientEvents(options))) {
      (rel, events) =>
        assert(
          runs(
            events,
            NonInviteClientTransaction.start(options, rel, config)._1,
            NonInviteClientTransaction.transition,
            _ == NonInviteClientState.Terminated
          )
        )
    }
  }

  property("an INVITE server stays terminated and silent once terminated") {
    forAll(reliability, Gen.listOfN(25, serverEvents(invite))) {
      (rel, events) =>
        assert(
          runs(
            events,
            InviteServerTransaction.start(invite, rel, config)._1,
            InviteServerTransaction.transition,
            _ == InviteServerState.Terminated
          )
        )
    }
  }

  property("a non-INVITE server stays terminated and silent once terminated") {
    forAll(reliability, Gen.listOfN(25, serverEvents(options))) {
      (rel, events) =>
        assert(
          runs(
            events,
            NonInviteServerTransaction.start(options, rel, config)._1,
            NonInviteServerTransaction.transition,
            _ == NonInviteServerState.Terminated
          )
        )
    }
  }
}
