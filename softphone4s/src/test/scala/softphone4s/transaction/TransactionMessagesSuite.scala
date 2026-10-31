package softphone4s.transaction

import cats.data.NonEmptyList
import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.model.*

class TransactionMessagesSuite extends FunSuite {

  private val source = SocketAddress(ip"203.0.113.7", port"41000")

  private val request = SipRequest(
    SipMethod.Options,
    uri("sip:bob@example.com"),
    SipHeaders(
      callId = callId("m@h"),
      from = nameAddr("<sip:alice@example.com>;tag=a"),
      to = nameAddr("<sip:bob@example.com>"),
      vias = NonEmptyList.of(
        via("SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKtop;rport"),
        via("SIP/2.0/UDP proxy.example.com;branch=z9hG4bKbelow")
      ),
      cseq = cseq(1, SipMethod.Options)
    )
  )

  test(
    "only the topmost Via of a response records where the request came from"
  ) {
    val response = TransactionMessages.seenFrom(
      TransactionMessages.responseTo(request, 200, "OK"),
      source
    )
    val vias = response.headers.vias
    assertEquals(vias.head.rport, Some(RPort.Assigned(port"41000")))
    assertEquals(vias.head.received, Some(ip"203.0.113.7"))
    assertEquals(vias.tail, request.headers.vias.tail)
  }

  test("everything else in the response is as it was") {
    val plain =
      TransactionMessages.responseTo(request, 486, "Busy Here", Some(tag("t")))
    val stamped = TransactionMessages.seenFrom(plain, source)
    assertEquals(stamped.statusCode, plain.statusCode)
    assertEquals(stamped.headers.to, plain.headers.to)
    assertEquals(stamped.headers.cseq, plain.headers.cseq)
  }
}
