package softphone4s.fsm

import softphone4s.codec.SdpCodec
import softphone4s.codec.SdpCodec.AudioOffer
import softphone4s.model.{FailureStatus, SipMethod, SipRequest}

/** What a request within an established dialog, other than BYE, INFO and
  * CANCEL, asks of us, and how to answer it.
  */
enum DialogRequest {

  /** OPTIONS: a capability query, answered 200. */
  case Query

  /** UPDATE without a body, a session refresh: answered 200. */
  case Refresh

  /** A re-INVITE or UPDATE whose SDP we can answer: media may move or be put on
    * hold.
    */
  case Offer(offer: AudioOffer)

  /** A refusal: an SDP we cannot answer, or a method we do not handle. */
  case Refused(status: FailureStatus)
}

object DialogRequest {

  def of(request: SipRequest): DialogRequest = request.method match {
    case SipMethod.Options                   => Query
    case SipMethod.Invite | SipMethod.Update =>
      if request.body.isEmpty then
        // A re-INVITE without an offer would have us make the offer in the 200
        // and take the answer from the ACK; not supported yet.
        if request.method == SipMethod.Update then Refresh
        else Refused(FailureStatus.NotAcceptableHere)
      else
        SdpCodec.parseOffer(request.body).filter(_.isAcceptable) match {
          case Some(offer) => Offer(offer)
          case None        => Refused(FailureStatus.NotAcceptableHere)
        }
    case _ => Refused(FailureStatus.NotImplemented)
  }
}
