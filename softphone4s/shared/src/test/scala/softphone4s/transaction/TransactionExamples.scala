package softphone4s.transaction

import cats.data.NonEmptyList
import softphone4s.SipLiterals.*
import softphone4s.model.*

/** Requests and responses for the transaction tests. */
object TransactionExamples {

  def request(
      method: SipMethod,
      branchText: String = "z9hG4bKtxn1",
      cseqNumber: Int = 7
  ): SipRequest =
    SipRequest(
      method,
      uri("sip:bob@example.com"),
      SipHeaders(
        callId = callId("txn@host"),
        from = nameAddr("<sip:alice@example.com>;tag=a1"),
        to = nameAddr("<sip:bob@example.com>"),
        vias = NonEmptyList.one(
          via(s"SIP/2.0/UDP 192.0.2.1:5060;branch=$branchText")
        ),
        cseq = cseq(cseqNumber, method),
        routes = List(nameAddr("<sip:proxy.example.com;lr>"))
      )
    )

  val invite: SipRequest  = request(SipMethod.Invite)
  val options: SipRequest = request(SipMethod.Options)

  def response(
      of: SipRequest,
      code: Int,
      reason: String = "Reason"
  ): SipResponse =
    TransactionMessages.responseTo(of, code, reason, Some(tag("b1")))
}
