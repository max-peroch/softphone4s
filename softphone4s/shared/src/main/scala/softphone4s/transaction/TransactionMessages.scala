package softphone4s.transaction

import cats.data.NonEmptyList
import com.comcast.ip4s.{IpAddress, SocketAddress}
import softphone4s.model.*

/** Messages a transaction builds itself, per RFC 3261 §17 and §8.2.6. */
object TransactionMessages {

  /** The ACK for a 3xx-6xx response to `request` (§17.1.1.3): the same
    * request-URI, Call-ID, From, Route and topmost Via, the To of the response,
    * and the CSeq number of the request with method ACK. It belongs to the
    * transaction, unlike the ACK of a 2xx, which is the dialog's.
    */
  def ackForFailure(request: SipRequest, response: SipResponse): SipRequest =
    SipRequest(
      method = SipMethod.Ack,
      requestUri = request.requestUri,
      headers = SipHeaders(
        callId = request.headers.callId,
        from = request.headers.from,
        to = response.headers.to,
        vias = NonEmptyList.one(request.headers.topVia),
        cseq = CSeq(request.headers.cseq.seq, SipMethod.Ack),
        routes = request.headers.routes
      )
    )

  /** A response to `request` with the headers a response must copy (§8.2.6.2):
    * every Via, and From, To, Call-ID and CSeq. `toTag` is added to To unless
    * it already has one.
    */
  def responseTo(
      request: SipRequest,
      statusCode: Int,
      reason: String,
      toTag: Option[Tag] = None
  ): SipResponse =
    SipResponse(
      statusCode,
      reason,
      SipHeaders(
        callId = request.headers.callId,
        from = request.headers.from,
        to = request.headers.to.copy(
          tag = request.headers.to.tag.orElse(toTag)
        ),
        vias = request.headers.vias,
        cseq = request.headers.cseq
      )
    )

  /** `response` with its topmost Via as it records where the request came from
    * (`received`, and `rport` if it was asked for), per RFC 3261 §18.2.1 and
    * RFC 3581. Requests that are answered to `source` anyway, as ours are, get
    * the same answer either way; this is what tells the sender what it looks
    * like from outside.
    */
  def seenFrom(
      response: SipResponse,
      source: SocketAddress[IpAddress]
  ): SipResponse = {
    val vias = response.headers.vias
    response.copy(headers =
      response.headers.copy(vias =
        NonEmptyList(vias.head.seenFrom(source), vias.tail)
      )
    )
  }

  /** The `100 Trying` an INVITE server transaction sends at once. */
  def trying(request: SipRequest): SipResponse =
    responseTo(request, 100, "Trying")
}
