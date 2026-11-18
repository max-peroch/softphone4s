package softphone4s.transaction

import cats.data.State
import cats.syntax.all.*
import softphone4s.fsm.TimerAction
import softphone4s.model.{SipRequest, SipResponse}

import scala.concurrent.duration.FiniteDuration

/** What a non-INVITE client transaction remembers about its request. */
final case class NonInviteClientContext(
    request: SipRequest,
    reliability: Reliability,
    config: TransactionConfig
)

/** The states of RFC 3261 Figure 6. */
enum NonInviteClientState {

  /** Request sent; `interval` is the next Timer E retransmit delay. */
  case Trying(context: NonInviteClientContext, interval: FiniteDuration)

  case Proceeding(context: NonInviteClientContext)

  /** A final response arrived; Timer K absorbs its retransmits. */
  case Completed(context: NonInviteClientContext)

  case Terminated
}

/** The non-INVITE client transaction (RFC 3261 §17.1.2) as a pure state
  * machine.
  */
object NonInviteClientTransaction {
  import ClientAction.*
  import NonInviteClientState.*

  type Transition = State[NonInviteClientState, List[ClientAction]]

  /** Sends `request` and starts the timers. */
  def start(
      request: SipRequest,
      reliability: Reliability,
      config: TransactionConfig
  ): (NonInviteClientState, List[ClientAction]) = {
    val context    = NonInviteClientContext(request, reliability, config)
    val retransmit = reliability match {
      case Reliability.Unreliable =>
        List(startTimer(TransactionTimer.E, config.t1))
      case Reliability.Reliable => Nil
    }
    (
      Trying(context, config.t1),
      Transmit(request) :: retransmit ::: List(
        startTimer(TransactionTimer.F, config.transactionTimeout)
      )
    )
  }

  def transition(event: ClientEvent): Transition =
    State.get[NonInviteClientState].flatMap { state =>
      (state, event) match {

        // ---- Trying -------------------------------------------------------
        case (
              Trying(context, interval),
              ClientEvent.TimerFired(TransactionTimer.E)
            ) =>
          val next = context.config.nextCapped(interval)
          go(
            Trying(context, next),
            List(
              Transmit(context.request),
              startTimer(TransactionTimer.E, next)
            )
          )

        case (Trying(context, _), ClientEvent.Response(response)) =>
          answered(context, response)

        // ---- Proceeding: retransmit at the T2 cap -------------------------
        case (
              Proceeding(context),
              ClientEvent.TimerFired(TransactionTimer.E)
            ) =>
          go(
            state,
            List(
              Transmit(context.request),
              startTimer(TransactionTimer.E, context.config.t2)
            )
          )

        case (Proceeding(context), ClientEvent.Response(response)) =>
          answered(context, response)

        // Timer F ends the wait in Trying and in Proceeding alike.
        case (
              Trying(_, _) | Proceeding(_),
              ClientEvent.TimerFired(TransactionTimer.F)
            ) =>
          go(Terminated, TimedOut :: stopAll)

        // ---- Completed ----------------------------------------------------
        case (Completed(_), ClientEvent.TimerFired(TransactionTimer.K)) =>
          go(Terminated, Nil)

        // ---- Transport ----------------------------------------------------
        case (
              Trying(_, _) | Proceeding(_) | Completed(_),
              ClientEvent.TransportError
            ) =>
          go(Terminated, TransportFailed :: stopAll)

        // Anything else (including a response in Completed) is absorbed.
        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------

  /** A response in Trying or Proceeding (Figure 6). */
  private def answered(
      context: NonInviteClientContext,
      response: SipResponse
  ): Transition =
    ResponseClass.of(response) match {
      case ResponseClass.Provisional =>
        // Timer E keeps running; in Proceeding it fires at the T2 cap.
        go(Proceeding(context), List(PassUp(response)))

      case ResponseClass.Success | ResponseClass.Failure =>
        val passed = PassUp(response) :: stopAll
        context.reliability match {
          case Reliability.Reliable   => go(Terminated, passed)
          case Reliability.Unreliable =>
            go(
              Completed(context),
              passed :+ startTimer(TransactionTimer.K, context.config.t4)
            )
        }
    }

  private def stopAll: List[ClientAction] =
    List(cancelTimer(TransactionTimer.E), cancelTimer(TransactionTimer.F))

  private def startTimer(
      timer: TransactionTimer,
      after: FiniteDuration
  ): ClientAction =
    Timer(TimerAction.Start(timer, after))

  private def cancelTimer(timer: TransactionTimer): ClientAction =
    Timer(TimerAction.Cancel(timer))

  private def go(
      next: NonInviteClientState,
      actions: List[ClientAction]
  ): Transition =
    State.set(next).as(actions)
}
