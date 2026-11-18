package softphone4s.subscription

import softphone4s.model.{MediaType, SipMethod, SipRequest}

/** What a NOTIFY says, read from the request (RFC 6665 §4.1.2): which
  * subscription it is for, where that stands, and the state it carries.
  *
  * @param cseq
  *   the CSeq number, with which NOTIFYs of a dialog are put in order.
  */
final case class NotifyInfo(
    event: EventHeader,
    state: NotifyState,
    contentType: Option[MediaType],
    body: String,
    cseq: Long
)

object NotifyInfo {

  /** `request` as a NOTIFY, or why it is not one we can take. */
  def from(request: SipRequest): Either[NotifyError, NotifyInfo] =
    for {
      _ <- Either.cond(
        request.method == SipMethod.Notify,
        (),
        NotifyError.NotANotify
      )
      eventText <- header(request, "Event", "o").toRight(
        NotifyError.MissingEvent
      )
      event <- EventHeader.parse(eventText).left.map(NotifyError.BadEvent(_))
      stateText <- header(request, "Subscription-State")
        .toRight(NotifyError.MissingSubscriptionState)
      state <- NotifyState
        .parse(stateText)
        .left
        .map(NotifyError.BadSubscriptionState(_))
      _ <- Either.cond(
        request.body.isEmpty || request.headers.contentType.isDefined,
        (),
        NotifyError.MissingContentType
      )
    } yield NotifyInfo(
      event,
      state,
      request.headers.contentType,
      request.body,
      request.headers.cseq.seq.value.toLong
    )

  /** The value of the first header of any of these names (a compact form among
    * them), whatever its case.
    */
  private def header(request: SipRequest, names: String*): Option[String] =
    request.headers.others
      .find(h => names.exists(h.name.matches))
      .map(_.value.trim)
}
