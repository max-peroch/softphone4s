package softphone4s.publication

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.model.*

class EntityTagSuite extends FunSuite {

  test("a tag is a token, and means nothing more") {
    assertEquals(EntityTag.parse("dx200xyz").map(_.value), Right("dx200xyz"))
    assertEquals(EntityTag.parse(" a.b-c_d! ").map(_.value), Right("a.b-c_d!"))
  }

  test("what is not a token is not a tag") {
    List("", "  ", "a b", "a,b", "a\"b", "a;b", "é").foreach(text =>
      assert(EntityTag.parse(text).isLeft, text)
    )
  }

  test("the tag of a response is the one in SIP-ETag, if it has one") {
    def response(headers: List[ExtensionHeader]) =
      SipResponse(
        200,
        "OK",
        SipHeaders(
          callId = callId("pub@host"),
          from = nameAddr("<sip:alice@test.local>;tag=ltag"),
          to = nameAddr("<sip:alice@test.local>;tag=srv"),
          vias = NonEmptyList.one(
            via("SIP/2.0/UDP 192.0.2.1:5060;branch=z9hG4bKp")
          ),
          cseq = cseq(1, SipMethod.Publish),
          others = headers
        )
      )
    assertEquals(EntityTag.of(response(Nil)), None)
    assertEquals(
      EntityTag
        .of(response(List(ExtensionHeader(FieldName.SipETag, "t9"))))
        .map(_.map(_.value)),
      Some(Right("t9"))
    )
    assert(
      EntityTag
        .of(response(List(ExtensionHeader(FieldName.SipETag, "not ok"))))
        .exists(_.isLeft)
    )
  }
}
