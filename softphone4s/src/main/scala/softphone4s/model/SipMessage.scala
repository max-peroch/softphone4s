package softphone4s.model

enum SipMethod {
  case Invite, Ack, Bye, Cancel, Options, Info, Register
}

final case class CSeq(seq: Int, method: SipMethod)

final case class SipHeaders(
    callId: String,
    from: String, // full From header value, e.g. <sip:user@host>;tag=abc
    to: String,   // full To header value
    via: String,  // topmost Via header value
    cseq: CSeq,
    contact: String,
    maxForwards: Int = 70,
    contentType: Option[String] = None,
    contentLength: Int = 0,
    route: List[String] = Nil,
    recordRoute: List[String] = Nil,
    wwwAuthenticate: Option[String] = None,
    proxyAuthenticate: Option[String] = None,
    extraHeaders: Map[String, List[String]] = Map.empty
)

sealed trait SipMessage {
  def headers: SipHeaders
}

final case class SipRequest(
    method: SipMethod,
    requestUri: String,
    headers: SipHeaders,
    body: String = ""
) extends SipMessage

final case class SipResponse(
    statusCode: Int,
    reason: String,
    headers: SipHeaders,
    body: String = ""
) extends SipMessage
