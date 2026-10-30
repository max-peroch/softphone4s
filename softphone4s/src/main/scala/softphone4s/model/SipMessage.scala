package softphone4s.model

import cats.data.NonEmptyList

enum SipMethod {
  case Invite, Ack, Bye, Cancel, Options, Info, Register
}

final case class CSeq(seq: Int, method: SipMethod)

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
    maxForwards: Int = 70,
    contentType: Option[String] = None,
    routes: List[NameAddr] = Nil,
    recordRoutes: List[NameAddr] = Nil,
    wwwAuthenticate: Option[String] = None,
    proxyAuthenticate: Option[String] = None,
    others: List[ExtensionHeader] = Nil
) {
  def topVia: Via = vias.head
}

sealed trait SipMessage {
  def headers: SipHeaders
}

final case class SipRequest(
    method: SipMethod,
    requestUri: SipUri,
    headers: SipHeaders,
    body: String = ""
) extends SipMessage

final case class SipResponse(
    statusCode: Int,
    reason: String,
    headers: SipHeaders,
    body: String = ""
) extends SipMessage
