package softphone4s.model

sealed trait SipMessage {
  def headers: SipHeaders
  def body: String
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
