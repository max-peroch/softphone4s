package softphone4s.messaging

import munit.FunSuite
import softphone4s.model.MediaType

class MessageContentSuite extends FunSuite {

  private def mediaType(text: String) =
    MediaType.parse(text).fold(e => fail(e), identity)

  test("text/plain is read, with or without a character set we can read") {
    List(
      "text/plain",
      "text/plain;charset=utf-8",
      "text/plain; charset=UTF-8",
      "text/plain;charset=us-ascii",
      "text/plain;charset=\"utf-8\"",
      "TEXT/Plain"
    ).foreach(text => assert(MessageContent.canRead(mediaType(text)), text))
  }

  test("another character set would be garbled, so it is not read") {
    assert(!MessageContent.canRead(mediaType("text/plain;charset=iso-8859-1")))
    assert(!MessageContent.canRead(mediaType("text/plain;charset=utf-16")))
  }

  test("another type is not read") {
    List("text/html", "application/sdp", "image/jpeg", "message/cpim").foreach(
      text => assert(!MessageContent.canRead(mediaType(text)), text)
    )
  }

  test("the charset parameter is found whatever its case or quoting") {
    assertEquals(mediaType("text/plain;charset=UTF-8").charset, Some("utf-8"))
    assertEquals(
      mediaType("text/plain;charset=\"Utf-8\"").charset,
      Some("utf-8")
    )
    assertEquals(mediaType("text/plain").charset, None)
  }

  test("what we say we take is what we read") {
    assert(MessageContent.Accepted.forall(MessageContent.canRead))
  }
}
