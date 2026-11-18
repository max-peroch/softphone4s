package softphone4s.fsm

import softphone4s.codec.SdpCodec.AudioOffer
import softphone4s.model.*

import scala.concurrent.duration.FiniteDuration

/** Semantic actions: interpreted by the effect layer, never by the state
  * machine itself.
  */
enum SipAction {
  // Requests the interpreter must build (need random Via branches)
  case SendInvite(
      callee: SipUri,
      callId: CallId,
      localTag: Tag,
      cseq: CSeqNumber,
      authHeader: Option[ExtensionHeader] = None
  )
  case RetryInviteWithAuth(
      ctx: InviteCtx,
      kind: AuthKind,
      challenge: DigestChallenge
  )
  case SendAck(
      dialog: SipDialog,
      remoteSdpForAnswer: Option[String] // Some(body) when offer was in 200 OK
  )
  case SendCancel(ctx: InviteCtx)
  case SendBye(dialog: SipDialog)

  /** A digit in a SIP INFO within `dialog`, whose CSeq is already advanced. */
  case SendInfo(dialog: SipDialog, digit: DtmfDigit, duration: FiniteDuration)

  // Responses the interpreter builds deterministically
  case SendByeOk(request: SipRequest)
  case SendInfoOk(request: SipRequest)
  case SendOptionsOk(request: SipRequest)

  /** A 200 to a re-INVITE that had no SDP: it carries our offer, and the answer
    * comes in the ACK.
    */
  case SendOfferOk(request: SipRequest)

  /** Send a re-INVITE offering `change`; `dialog` carries its CSeq. */
  case SendReinvite(dialog: SipDialog, change: SessionChange)

  /** The remote party's answer to our re-INVITE: media goes where it says. */
  case ApplyAnswer(response: SipResponse)

  /** Wait a random time suited to `role`, then send the re-INVITE again. */
  case ScheduleReinviteRetry(role: DialogRole)

  /** A 200 to a re-INVITE or UPDATE: with our answer to `offer` if there is
    * one, after which media goes where the offer says (or stops, on hold).
    */
  case SendSessionOk(request: SipRequest, offer: Option[AudioOffer])

  /** A final failure response to a request within the dialog. */
  case SendReply(request: SipRequest, status: FailureStatus)

  // Media
  case StartMedia(sdpBody: String)
  case StopMedia

  // Notifications
  case NotifyAnswered
  case NotifyHangup(reason: TerminationReason)
  case NotifyDtmf(digit: DtmfDigit)

  // Timers
  case Timer(action: TimerAction[CallTimer])
}
