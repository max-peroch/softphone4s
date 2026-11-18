package softphone4s.transaction

import cats.data.State
import cats.syntax.all.*
import softphone4s.fsm.TimerAction
import softphone4s.model.{SipRequest, SipResponse}

import scala.concurrent.duration.FiniteDuration

/** What an INVITE client transaction remembers about its request. */
final case class InviteClientContext(
    request: SipRequest,
    reliability: Reliability,
    config: TransactionConfig
)

/** The states of RFC 3261 Figure 5. */
enum InviteClientState {

  /** Request sent; `interval` is the next Timer A retransmit delay. */
  case Calling(context: InviteClientContext, interval: FiniteDuration)

  case Proceeding(context: InviteClientContext)

  /** A failure was answered with `ack`, which is sent again for every
    * retransmitted failure until Timer D ends the wait.
    */
  case Completed(context: InviteClientContext, ack: SipRequest)

  case Terminated
}

/** The INVITE client transaction (RFC 3261 §17.1.1) as a pure state machine. */
object InviteClientTransaction {
  import ClientAction.*
  import InviteClientState.*

  type Transition = State[InviteClientState, List[ClientAction]]

  /** Sends `request` and starts the timers. */
  def start(
      request: SipRequest,
      reliability: Reliability,
      config: TransactionConfig
  ): (InviteClientState, List[ClientAction]) = {
    val context    = InviteClientContext(request, reliability, config)
    val retransmit = reliability match {
      case Reliability.Unreliable =>
        List(startTimer(TransactionTimer.A, config.t1))
      case Reliability.Reliable => Nil
    }
    (
      Calling(context, config.t1),
      Transmit(request) :: retransmit ::: List(
        startTimer(TransactionTimer.B, config.transactionTimeout)
      )
    )
  }

  def transition(event: ClientEvent): Transition =
    State.get[InviteClientState].flatMap { state =>
      (state, event) match {

        // ---- Calling ------------------------------------------------------
        case (
              Calling(context, interval),
              ClientEvent.TimerFired(TransactionTimer.A)
            ) =>
          val next = interval * 2
          go(
            Calling(context, next),
            List(
              Transmit(context.request),
              startTimer(TransactionTimer.A, next)
            )
          )

        case (Calling(_, _), ClientEvent.TimerFired(TransactionTimer.B)) =>
          go(Terminated, TimedOut :: stopRetransmit)

        case (Calling(context, _), ClientEvent.Response(response)) =>
          answered(context, response)

        // ---- Proceeding ---------------------------------------------------
        case (Proceeding(context), ClientEvent.Response(response)) =>
          answered(context, response)

        // ---- Completed ----------------------------------------------------
        case (Completed(_, ack), ClientEvent.Response(response))
            if ResponseClass.of(response) == ResponseClass.Failure =>
          State.pure(List(Transmit(ack)))

        case (Completed(_, _), ClientEvent.TimerFired(TransactionTimer.D)) =>
          go(Terminated, Nil)

        // ---- Transport ----------------------------------------------------
        case (
              Calling(_, _) | Proceeding(_) | Completed(_, _),
              ClientEvent.TransportError
            ) =>
          go(Terminated, TransportFailed :: stopAll)

        // Anything else is a response or timer that no longer applies.
        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------

  /** A response in Calling or Proceeding (Figure 5). */
  private def answered(
      context: InviteClientContext,
      response: SipResponse
  ): Transition =
    ResponseClass.of(response) match {
      case ResponseClass.Provisional =>
        go(Proceeding(context), PassUp(response) :: stopAll)

      // The ACK for a 2xx is the dialog's to send.
      case ResponseClass.Success =>
        go(Terminated, PassUp(response) :: stopAll)

      case ResponseClass.Failure =>
        val ack  = TransactionMessages.ackForFailure(context.request, response)
        val sent = PassUp(response) :: stopAll ::: List(Transmit(ack))
        context.reliability match {
          case Reliability.Reliable   => go(Terminated, sent)
          case Reliability.Unreliable =>
            go(
              Completed(context, ack),
              sent :+ startTimer(TransactionTimer.D, context.config.timerD)
            )
        }
    }

  private def stopRetransmit: List[ClientAction] =
    List(cancelTimer(TransactionTimer.A))

  private def stopAll: List[ClientAction] =
    List(cancelTimer(TransactionTimer.A), cancelTimer(TransactionTimer.B))

  private def startTimer(
      timer: TransactionTimer,
      after: FiniteDuration
  ): ClientAction =
    Timer(TimerAction.Start(timer, after))

  private def cancelTimer(timer: TransactionTimer): ClientAction =
    Timer(TimerAction.Cancel(timer))

  private def go(
      next: InviteClientState,
      actions: List[ClientAction]
  ): Transition =
    State.set(next).as(actions)
}
