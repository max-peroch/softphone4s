package softphone4s.model

import cats.data.NonEmptyList

/** The headers of a SIP message. The ones the stack acts on are typed;
  * everything else is carried unchanged, in order, in `others`.
  *
  * @param vias
  *   every Via, topmost first (at least one).
  * @param contacts
  *   every Contact value; the `*` form of REGISTER is not modelled yet.
  */
final case class SipHeaders(
    callId: CallId,
    from: NameAddr,
    to: NameAddr,
    vias: NonEmptyList[Via],
    cseq: CSeq,
    contacts: List[NameAddr] = Nil,
    maxForwards: MaxForwards = MaxForwards.Default,
    contentType: Option[MediaType] = None,
    routes: List[NameAddr] = Nil,
    recordRoutes: List[NameAddr] = Nil,
    wwwAuthenticate: List[AuthChallenge] = Nil,
    proxyAuthenticate: List[AuthChallenge] = Nil,
    expires: Option[Expires] = None,
    minExpires: Option[Expires] = None,
    others: List[ExtensionHeader] = Nil
) {
  def topVia: Via = vias.head
}
