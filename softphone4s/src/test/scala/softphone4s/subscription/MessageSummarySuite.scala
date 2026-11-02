package softphone4s.subscription

import munit.FunSuite
import softphone4s.model.{MediaType, SipUri}

class MessageSummarySuite extends FunSuite {

  private def notification(
      body: String,
      contentType: Option[MediaType] = Some(MediaType.SimpleMessageSummary)
  ) = Notification(NotifyState.Active(None), contentType, body)

  private def counts(n: Int, o: Int, un: Int = 0, uo: Int = 0) =
    MessageCounts.of(n, o, un, uo).fold(e => fail(e), identity)

  test(
    "the example of RFC 3842: voice messages, new and old, and the urgent ones"
  ) {
    val body =
      "Messages-Waiting: yes\r\n" +
        "Message-Account: sip:alice@vmail.example.com\r\n" +
        "Voice-Message: 2/8 (0/2)\r\n"
    assertEquals(
      MessageSummary.Package.decode(notification(body)),
      Right(
        MessageSummary(
          true,
          Some(
            SipUri
              .parse("sip:alice@vmail.example.com")
              .fold(e => fail(e), identity)
          ),
          Map(MessageClass.Voice -> counts(2, 8, 0, 2))
        )
      )
    )
  }

  test("a mailbox that is empty, with no counts and no account") {
    assertEquals(
      MessageSummary.parse("Messages-Waiting: no\r\n"),
      Right(MessageSummary(false, None, Map.empty))
    )
  }

  test(
    "names are not case-sensitive, classes are told apart, unknown fields are skipped"
  ) {
    val body =
      "messages-WAITING: Yes\nFax-Message: 1/0\nText-Message: 0/3 (0/0)\nX-Vendor: whatever\n"
    assertEquals(
      MessageSummary.parse(body),
      Right(
        MessageSummary(
          true,
          None,
          Map(
            MessageClass.Fax  -> counts(1, 0),
            MessageClass.Text -> counts(0, 3)
          )
        )
      )
    )
  }

  test(
    "a body without Messages-Waiting, or with an answer that is neither, is malformed"
  ) {
    assert(MessageSummary.parse("Voice-Message: 1/0\n").isLeft)
    assert(MessageSummary.parse("Messages-Waiting: maybe\n").isLeft)
  }

  test("counts that are impossible are refused") {
    List(
      "Voice-Message: 1/x",
      "Voice-Message: 1/2 (3/0)",
      "Voice-Message: 99999999999/0",
      "Voice-Message: 3"
    ).foreach { line =>
      assert(
        MessageSummary.parse(s"Messages-Waiting: yes\n$line\n").isLeft,
        line
      )
    }
  }

  test("a line that is not a field is malformed") {
    assert(MessageSummary.parse("Messages-Waiting: yes\nnonsense\n").isLeft)
  }

  test("no body, or another type, is said so and not read") {
    assertEquals(
      MessageSummary.Package.decode(notification("")),
      Left(PackageError.NoBody)
    )
    assertEquals(
      MessageSummary.Package.decode(
        notification("Messages-Waiting: yes", Some(MediaType.Plain))
      ),
      Left(PackageError.UnexpectedType(Some(MediaType.Plain)))
    )
    assertEquals(
      MessageSummary.Package.decode(
        notification("Messages-Waiting: yes", None)
      ),
      Left(PackageError.UnexpectedType(None))
    )
  }

  test("counts cannot be made but through of, which checks them") {
    assert(compileErrors("MessageCounts(1, 2, 3, 4)").nonEmpty)
    assert(
      compileErrors(
        "MessageCounts.of(1, 2).toOption.get.copy(newMessages = -1)"
      ).nonEmpty
    )
  }

  test("a body over the limit is refused, and one at it is read") {
    val most                = EventPackage.MaxBodyLength
    val head                = "Messages-Waiting: yes\r\nX-Pad: "
    def padded(length: Int) = head + "x" * (length - head.length)
    assert(MessageSummary.parse(padded(most)).isRight)
    assertEquals(
      MessageSummary.parse(padded(most + 1)),
      Left(PackageError.TooLarge(most + 1, most))
    )
  }

  test("the package asks for message-summary and takes its type") {
    assertEquals(MessageSummary.Package.event.render, "message-summary")
    assertEquals(
      MessageSummary.Package.accept,
      List(MediaType.SimpleMessageSummary)
    )
  }
}
