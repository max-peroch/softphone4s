package softphone4s.fsm

import softphone4s.model.*
import scala.concurrent.duration.*

enum InboundState {

  /** The INVITE is with the application; only `100 Trying` has been sent. */
  case Offered(ctx: InboundCtx)

  /** `180 Ringing` has been sent. */
  case Ringing(ctx: InboundCtx)

  /** `200 OK` is out and is repeated until the ACK; `interval` is the next
    * repeat delay.
    */
  case Answered(ctx: InboundCtx, interval: FiniteDuration)

  case Active(dialog: SipDialog)

  /** Our re-INVITE for `change` is in flight; `dialog` already carries its
    * CSeq.
    */
  case Reinviting(dialog: SipDialog, change: SessionChange)

  /** Our re-INVITE met a 491; the retry timer runs. */
  case Retrying(dialog: SipDialog, change: SessionChange)
  case ByeSent(dialog: SipDialog)
  case Closed
}
