package softphone4s.fsm

import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*

class SipMessageBuilderSuite extends FunSuite {

  private def ok[A](e: Either[SoftphoneError, A]): A =
    e.fold(err => fail(err.getMessage), identity)

  private def callee(s: String): Callee =
    Callee(s).fold(err => fail(err), identity)

  private val config =
    SoftphoneConfig(user = "alice", password = "pw", realm = "sip.example.com")

  private val builder =
    ok(
      SipMessageBuilder.create(config, ip"192.168.1.5", port"5060", port"8000")
    )

  test("calleeUri puts a bare number at the account's realm") {
    assertEquals(
      ok(SipMessageBuilder.calleeUri(callee("1234"), "sip.example.com")).render,
      "sip:1234@sip.example.com"
    )
  }

  test("calleeUri keeps an explicit host and port") {
    val uri = ok(
      SipMessageBuilder.calleeUri(callee("bob@10.0.0.2:5070"), "ignored.com")
    )
    assertEquals(uri.render, "sip:bob@10.0.0.2:5070")
    assertEquals(uri.port, Some(port"5070"))
  }

  test("calleeUri percent-encodes '#' and keeps '*' and '+'") {
    assertEquals(
      ok(SipMessageBuilder.calleeUri(callee("*67#"), "h.com")).render,
      "sip:*67%23@h.com"
    )
    assertEquals(
      ok(SipMessageBuilder.calleeUri(callee("+15551234567"), "h.com")).render,
      "sip:+15551234567@h.com"
    )
  }

  test("calleeUri fails on an invalid realm") {
    assert(
      SipMessageBuilder.calleeUri(callee("1234"), "not a host").isLeft
    )
  }

  test("create derives the account address and Contact from typed parts") {
    assertEquals(builder.localSipAddress.render, "sip:alice@sip.example.com")
    val invite = builder.buildInvite(
      ok(SipMessageBuilder.calleeUri(callee("1234"), "sip.example.com")),
      "cid",
      "ltag",
      1,
      "z9hG4bKx"
    )
    assertEquals(invite.requestUri.render, "sip:1234@sip.example.com")
    assertEquals(invite.headers.contact, "<sip:alice@192.168.1.5:5060>")
    assertEquals(invite.headers.from, "<sip:alice@sip.example.com>;tag=ltag")
    assertEquals(invite.headers.to, "<sip:1234@sip.example.com>")
  }

  test("create fails on an unusable user or realm") {
    List(
      config.copy(user = "al ice"),
      config.copy(realm = "not a host")
    ).foreach(bad =>
      assert(
        SipMessageBuilder
          .create(bad, ip"192.168.1.5", port"5060", port"8000")
          .isLeft
      )
    )
  }
}
