package softphone4s.transaction

import cats.data.State
import cats.syntax.all.*
import softphone4s.fsm.TimerAction
import softphone4s.model.{SipRequest, SipResponse}

import scala.concurrent.duration.FiniteDuration

/** What a non-INVITE server transaction remembers about its request. */
final case class NonInviteServerContext(
    request: SipRequest,
    reliability: Reliability,
    config: TransactionConfig
)

/** The states of RFC 3261 Figure 8. */
enum NonInviteServerState {

  /** Request passed up; no response yet. */
  case Trying(context: NonInviteServerContext)

  /** `last` is the provisional response sent, repeated when the request is. */
  case Proceeding(context: NonInviteServerContext, last: SipResponse)

  /** A final `response` was sent; Timer J absorbs request retransmits. */
  case Completed(context: NonInviteServerContext, response: SipResponse)

  case Terminated
}

/** The non-INVITE server transaction (RFC 3261 §17.2.2) as a pure state
  * machine.
  */
object NonInviteServerTransaction {
  import NonInviteServerState.*
  import ServerAction.*

  type Transition = State[NonInviteServerState, List[ServerAction]]

  /** Passes `request` up to the transaction user. */
  def start(
      request: SipRequest,
      reliability: Reliability,
      config: TransactionConfig
  ): (NonInviteServerState, List[ServerAction]) =
    (
      Trying(NonInviteServerContext(request, reliability, config)),
      List(PassUp(request))
    )

  def transition(event: ServerEvent): Transition =
    State.get[NonInviteServerState].flatMap { state =>
      (state, event) match {

        // ---- Trying -------------------------------------------------------
        // A retransmitted request is discarded: nothing has been sent yet.
        case (Trying(context), ServerEvent.Respond(response)) =>
          responded(context, response)

        // ---- Proceeding ---------------------------------------------------
        case (Proceeding(_, last), ServerEvent.RequestRetransmitted) =>
          State.pure(List(Transmit(last)))

        case (Proceeding(context, _), ServerEvent.Respond(response)) =>
          responded(context, response)

        // ---- Completed ----------------------------------------------------
        case (Completed(_, response), ServerEvent.RequestRetransmitted) =>
          State.pure(List(Transmit(response)))

        case (Completed(_, _), ServerEvent.TimerFired(TransactionTimer.J)) =>
          go(Terminated, Nil)

        // ---- Transport ----------------------------------------------------
        case (
              Trying(_) | Proceeding(_, _) | Completed(_, _),
              ServerEvent.TransportError
            ) =>
          go(Terminated, List(TransportFailed))

        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------

  /** The user's response in Trying or Proceeding (Figure 8). */
  private def responded(
      context: NonInviteServerContext,
      response: SipResponse
  ): Transition =
    ResponseClass.of(response) match {
      case ResponseClass.Provisional =>
        go(Proceeding(context, response), List(Transmit(response)))
      case ResponseClass.Success | ResponseClass.Failure =>
        context.reliability match {
          case Reliability.Reliable => go(Terminated, List(Transmit(response)))
          case Reliability.Unreliable =>
            go(
              Completed(context, response),
              List(
                Transmit(response),
                startTimer(
                  TransactionTimer.J,
                  context.config.transactionTimeout
                )
              )
            )
        }
    }

  private def startTimer(
      timer: TransactionTimer,
      after: FiniteDuration
  ): ServerAction =
    Timer(TimerAction.Start(timer, after))

  private def go(
      next: NonInviteServerState,
      actions: List[ServerAction]
  ): Transition =
    State.set(next).as(actions)
}
