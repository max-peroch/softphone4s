package softphone4s.fsm

import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.config.SoftphoneConfig
import softphone4s.fsm.SipMessages.*
import softphone4s.model.*

class SipMessagesSuite extends FunSuite {

  private def ok[A](e: Either[SoftphoneError, A]): A =
    e.fold(err => fail(err.getMessage), identity)

  private def callee(s: String): Callee =
    Callee(s).fold(err => fail(err), identity)

  private val config =
    SoftphoneConfig(user = "alice", password = "pw", realm = "sip.example.com")

  private val endpoint =
    ok(LocalEndpoint.from(config, ip"192.168.1.5", port"5060", port"8000"))

  test("toSipUri puts a bare number at the account's realm") {
    assertEquals(
      ok(callee("1234").toSipUri("sip.example.com")).render,
      "sip:1234@sip.example.com"
    )
  }

  test("toSipUri keeps an explicit host and port") {
    val uri = ok(
      callee("bob@10.0.0.2:5070").toSipUri("ignored.com")
    )
    assertEquals(uri.render, "sip:bob@10.0.0.2:5070")
    assertEquals(uri.port, Some(port"5070"))
  }

  test("toSipUri percent-encodes '#' and keeps '*' and '+'") {
    assertEquals(
      ok(callee("*67#").toSipUri("h.com")).render,
      "sip:*67%23@h.com"
    )
    assertEquals(
      ok(callee("+15551234567").toSipUri("h.com")).render,
      "sip:+15551234567@h.com"
    )
  }

  test("toSipUri fails on an invalid realm") {
    assert(
      callee("1234").toSipUri("not a host").isLeft
    )
  }

  test(
    "LocalEndpoint derives the account address and Contact from typed parts"
  ) {
    assertEquals(endpoint.accountUri.render, "sip:alice@sip.example.com")
    val invite = endpoint.buildInvite(
      ok(callee("1234").toSipUri("sip.example.com")),
      callId("cid"),
      tag("ltag"),
      1,
      branch("z9hG4bKx")
    )
    assertEquals(invite.requestUri.render, "sip:1234@sip.example.com")
    assertEquals(
      invite.headers.contacts.map(_.render),
      List("<sip:alice@192.168.1.5:5060>")
    )
    assertEquals(
      invite.headers.from.render,
      "<sip:alice@sip.example.com>;tag=ltag"
    )
    assertEquals(invite.headers.to.render, "<sip:1234@sip.example.com>")
    assertEquals(
      invite.headers.topVia.render,
      "SIP/2.0/UDP 192.168.1.5:5060;branch=z9hG4bKx"
    )
  }

  test("LocalEndpoint.from fails on an unusable user or realm") {
    List(
      config.copy(user = "al ice"),
      config.copy(realm = "not a host")
    ).foreach(bad =>
      assert(
        LocalEndpoint
          .from(bad, ip"192.168.1.5", port"5060", port"8000")
          .isLeft
      )
    )
  }
}
