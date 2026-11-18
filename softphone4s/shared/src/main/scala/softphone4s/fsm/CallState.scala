package softphone4s.fsm

import softphone4s.model.{InviteCtx, SipDialog}

/** Where an outgoing call stands. */
enum CallState {
  case Idle
  case Inviting(ctx: InviteCtx)
  case Ringing(ctx: InviteCtx, dialog: SipDialog)
  case Active(dialog: SipDialog)

  /** Our re-INVITE for `change` is in flight; `dialog` already carries its
    * CSeq.
    */
  case Reinviting(dialog: SipDialog, change: SessionChange)

  /** Our re-INVITE met a 491; the retry timer runs. */
  case Retrying(dialog: SipDialog, change: SessionChange)
  case ByeSent(dialog: SipDialog)
  case Closed(reason: TerminationReason)
  case Failed(code: Int, reason: String)
}
