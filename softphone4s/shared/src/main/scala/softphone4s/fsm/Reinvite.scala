package softphone4s.fsm

import softphone4s.model.{DialogRole, SipResponse}

import scala.concurrent.duration.*

/** The rules for a re-INVITE we send (RFC 3261 §14.1). */
object Reinvite {

  /** How a re-INVITE we sent was answered. */
  enum Outcome {

    /** Still trying: a provisional response. */
    case Progress

    /** The change was accepted. */
    case Accepted

    /** Both sides sent a re-INVITE at once (491): try again later. */
    case Glare

    /** The dialog is gone (408 or 481): the call is over. */
    case DialogGone

    /** Refused for another reason: the session stays as it was. */
    case Refused
  }

  def outcome(response: SipResponse): Outcome = response.statusCode match {
    case code if code < 200 => Outcome.Progress
    case code if code < 300 => Outcome.Accepted
    case 491                => Outcome.Glare
    case 408 | 481          => Outcome.DialogGone
    case _                  => Outcome.Refused
  }

  private val OwnerMin  = 2100.millis
  private val OwnerSpan = 1900.millis
  private val OtherSpan = 2000.millis

  /** How long to wait before sending the re-INVITE again after a 491: for the
    * party that placed the call (owner of the Call-ID) between 2.1 and 4
    * seconds, for the other between 0 and 2, so that the two cannot collide
    * again. `fraction` is a random number in [0, 1).
    */
  def glareDelay(role: DialogRole, fraction: Double): FiniteDuration = {
    val f = fraction.max(0.0).min(1.0)
    role match {
      case DialogRole.Uac =>
        (OwnerMin.toMillis + (OwnerSpan.toMillis * f).toLong).millis
      case DialogRole.Uas => (OtherSpan.toMillis * f).toLong.millis
    }
  }
}
