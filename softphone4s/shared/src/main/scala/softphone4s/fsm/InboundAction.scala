package softphone4s.fsm

import softphone4s.codec.SdpCodec.AudioOffer
import softphone4s.model.*

import scala.concurrent.duration.FiniteDuration

enum InboundAction {
  case SendRinging(ctx: InboundCtx)
  case SendInviteFailure(
      invite: SipRequest,
      localTag: Tag,
      status: FailureStatus
  )

  /** Answer the INVITE with a 200 OK carrying our SDP, and remember it for
    * retransmission.
    */
  case SendInviteOk(ctx: InboundCtx)

  /** Send the 200 OK again. */
  case ResendOk

  /** A final response of our own making to `request` (CANCEL's 200, a 481 ...).
    */
  case Reply(request: SipRequest, status: Int, reason: String)
  case SendByeOk(request: SipRequest)
  case SendInfoOk(request: SipRequest)
  case SendOptionsOk(request: SipRequest)

  /** A 200 to a re-INVITE that had no SDP: it carries our offer, and the answer
    * comes in the ACK.
    */
  case SendOfferOk(request: SipRequest)

  /** A 200 to a re-INVITE or UPDATE: with our answer to `offer` if there is
    * one, after which media goes where the offer says (or stops, on hold).
    */
  case SendSessionOk(request: SipRequest, offer: Option[AudioOffer])
  case SendBye(dialog: SipDialog)

  /** A digit in a SIP INFO within `dialog`, whose CSeq is already advanced. */
  case SendInfo(dialog: SipDialog, digit: DtmfDigit, duration: FiniteDuration)

  /** Send a re-INVITE offering `change`; `dialog` carries its CSeq. */
  case SendReinvite(dialog: SipDialog, change: SessionChange)

  /** The ACK of the 2xx to our re-INVITE. */
  case SendAck(dialog: SipDialog)

  /** The remote party's answer to our re-INVITE: media goes where it says. */
  case ApplyAnswer(response: SipResponse)

  /** Wait a random time suited to `role`, then send the re-INVITE again. */
  case ScheduleReinviteRetry(role: DialogRole)
  case StartMedia(offer: AudioOffer)
  case StopMedia

  /** The ACK came: the call is established. */
  case NotifyEstablished
  case NotifyHangup(reason: TerminationReason)
  case NotifyDtmf(digit: DtmfDigit)
  case Timer(action: TimerAction[CallTimer])
}
