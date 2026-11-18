package softphone4s.transaction

import cats.data.State
import cats.syntax.all.*
import softphone4s.fsm.TimerAction
import softphone4s.model.{SipRequest, SipResponse}

import scala.concurrent.duration.FiniteDuration

/** What an INVITE server transaction remembers about its request. */
final case class InviteServerContext(
    request: SipRequest,
    reliability: Reliability,
    config: TransactionConfig
)

/** The states of RFC 3261 Figure 7. */
enum InviteServerState {

  /** `last` is the most recent response sent, repeated when the request is. */
  case Proceeding(context: InviteServerContext, last: SipResponse)

  /** A failure `response` was sent and awaits its ACK; `interval` is the next
    * Timer G retransmit delay.
    */
  case Completed(
      context: InviteServerContext,
      response: SipResponse,
      interval: FiniteDuration
  )

  /** The ACK arrived; Timer I absorbs its retransmits. */
  case Confirmed(context: InviteServerContext)

  case Terminated
}

/** The INVITE server transaction (RFC 3261 §17.2.1) as a pure state machine. A
  * 2xx ends it at once: retransmitting the 2xx until the ACK comes is the user
  * agent's job (§13.3.1.4).
  */
object InviteServerTransaction {
  import InviteServerState.*
  import ServerAction.*

  type Transition = State[InviteServerState, List[ServerAction]]

  /** Passes `request` up and answers `100 Trying` (§17.2.1). */
  def start(
      request: SipRequest,
      reliability: Reliability,
      config: TransactionConfig
  ): (InviteServerState, List[ServerAction]) = {
    val trying = TransactionMessages.trying(request)
    (
      Proceeding(InviteServerContext(request, reliability, config), trying),
      List(PassUp(request), Transmit(trying))
    )
  }

  def transition(event: ServerEvent): Transition =
    State.get[InviteServerState].flatMap { state =>
      (state, event) match {

        // ---- Proceeding ---------------------------------------------------
        case (Proceeding(_, last), ServerEvent.RequestRetransmitted) =>
          State.pure(List(Transmit(last)))

        case (Proceeding(context, _), ServerEvent.Respond(response)) =>
          ResponseClass.of(response) match {
            case ResponseClass.Provisional =>
              go(Proceeding(context, response), List(Transmit(response)))
            case ResponseClass.Success =>
              go(Terminated, List(Transmit(response)))
            case ResponseClass.Failure =>
              val sent   = List(Transmit(response))
              val timers = context.reliability match {
                case Reliability.Unreliable =>
                  List(startTimer(TransactionTimer.G, context.config.t1))
                case Reliability.Reliable => Nil
              }
              go(
                Completed(context, response, context.config.t1),
                sent ::: timers ::: List(
                  startTimer(
                    TransactionTimer.H,
                    context.config.transactionTimeout
                  )
                )
              )
          }

        // ---- Completed ----------------------------------------------------
        case (Completed(_, response, _), ServerEvent.RequestRetransmitted) =>
          State.pure(List(Transmit(response)))

        case (
              Completed(context, response, interval),
              ServerEvent.TimerFired(TransactionTimer.G)
            ) =>
          val next = context.config.nextCapped(interval)
          go(
            Completed(context, response, next),
            List(Transmit(response), startTimer(TransactionTimer.G, next))
          )

        case (Completed(_, _, _), ServerEvent.TimerFired(TransactionTimer.H)) =>
          go(Terminated, AckTimedOut :: stopWaiting)

        case (Completed(context, _, _), ServerEvent.AckReceived) =>
          context.reliability match {
            case Reliability.Reliable   => go(Terminated, stopWaiting)
            case Reliability.Unreliable =>
              go(
                Confirmed(context),
                stopWaiting :+ startTimer(TransactionTimer.I, context.config.t4)
              )
          }

        // ---- Confirmed ----------------------------------------------------
        case (Confirmed(_), ServerEvent.TimerFired(TransactionTimer.I)) =>
          go(Terminated, Nil)

        // ---- Transport ----------------------------------------------------
        case (
              Proceeding(_, _) | Completed(_, _, _) | Confirmed(_),
              ServerEvent.TransportError
            ) =>
          go(Terminated, TransportFailed :: stopWaiting)

        // Retransmitted ACKs in Confirmed, stale timers, and so on.
        case _ => State.pure(Nil)
      }
    }

  // -------------------------------------------------------------------------

  private def stopWaiting: List[ServerAction] =
    List(cancelTimer(TransactionTimer.G), cancelTimer(TransactionTimer.H))

  private def startTimer(
      timer: TransactionTimer,
      after: FiniteDuration
  ): ServerAction =
    Timer(TimerAction.Start(timer, after))

  private def cancelTimer(timer: TransactionTimer): ServerAction =
    Timer(TimerAction.Cancel(timer))

  private def go(
      next: InviteServerState,
      actions: List[ServerAction]
  ): Transition =
    State.set(next).as(actions)
}
