package softphone4s.messaging

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.model.*

class MessageFailureSuite extends FunSuite {

  private def response(
      code: Int,
      reason: String,
      others: List[ExtensionHeader] = Nil
  ): SipResponse =
    SipResponse(
      code,
      reason,
      SipHeaders(
        callId = callId("m@h"),
        from = nameAddr("<sip:a@h>;tag=1"),
        to = nameAddr("<sip:b@h>"),
        vias =
          NonEmptyList.one(via("SIP/2.0/UDP 127.0.0.1:5060;branch=z9hG4bKx")),
        cseq = cseq(1, SipMethod.Message),
        others = others
      )
    )

  private def header(name: String, value: String) =
    ExtensionHeader(FieldName(name).fold(e => fail(e), identity), value)

  test("any 2xx is delivery") {
    assertEquals(MessageFailure.of(response(200, "OK")), Right(()))
    assertEquals(MessageFailure.of(response(202, "Accepted")), Right(()))
  }

  test(
    "a challenge that is still there after we answered one is not authorised"
  ) {
    assertEquals(
      MessageFailure.of(response(401, "Unauthorized")),
      Left(MessageFailure.Unauthorized)
    )
    assertEquals(
      MessageFailure.of(response(407, "Proxy Authentication Required")),
      Left(MessageFailure.Unauthorized)
    )
  }

  test("a 415 lists the types it takes, leaving out what is not a type") {
    val refusal = response(
      415,
      "Unsupported Media Type",
      List(header("Accept", "text/plain, application/sdp, nonsense"))
    )
    assertEquals(
      MessageFailure.of(refusal),
      Left(
        MessageFailure.UnsupportedContent(List(MediaType.Plain, MediaType.Sdp))
      )
    )
  }

  test("a 415 that says nothing of what it takes has an empty list") {
    assertEquals(
      MessageFailure.of(response(415, "Unsupported Media Type")),
      Left(MessageFailure.UnsupportedContent(Nil))
    )
  }

  test("other refusals carry their status and reason") {
    List(
      404 -> "Not Found",
      486 -> "Busy Here",
      503 -> "Service Unavailable",
      302 -> "Moved Temporarily"
    ).foreach((code, reason) =>
      assertEquals(
        MessageFailure.of(response(code, reason)),
        Left(MessageFailure.Rejected(code, reason))
      )
    )
  }

  test("each failure says what happened") {
    assert(MessageFailure.Rejected(404, "Not Found").message.contains("404"))
    assert(MessageFailure.TimedOut.message.nonEmpty)
    assert(
      MessageFailure
        .UnsupportedContent(List(MediaType.Plain))
        .message
        .contains("text/plain")
    )
  }
}
