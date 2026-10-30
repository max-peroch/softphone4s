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
      cseqNumber(1),
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

  test("buildRegister binds our Contact at the registrar for the lifetime") {
    val lifetime = Expires(600).fold(err => fail(err), identity)
    val register = endpoint.buildRegister(
      uri("sip:sip.example.com"),
      callId("reg@host"),
      tag("ltag"),
      cseqNumber(3),
      branch("z9hG4bKreg"),
      lifetime
    )
    assertEquals(register.method, SipMethod.Register)
    assertEquals(register.requestUri.render, "sip:sip.example.com")
    assertEquals(
      register.headers.from.render,
      "<sip:alice@sip.example.com>;tag=ltag"
    )
    assertEquals(register.headers.to.render, "<sip:alice@sip.example.com>")
    assertEquals(register.headers.cseq, cseq(3, SipMethod.Register))
    assertEquals(register.headers.expires, Some(lifetime))
    assertEquals(
      register.headers.contacts.map(_.render),
      List("<sip:alice@192.168.1.5:5060>;expires=600")
    )
    assertEquals(register.body, "")
  }

  test("an authorization for a REGISTER is computed over the REGISTER method") {
    val challenge =
      AuthChallenge.parse("""Digest realm="r", nonce="n"""") match {
        case AuthChallenge.Digest(c) => c
        case other                   => fail(s"unexpected: $other")
      }
    val cnonce = softphone4s.model.Cnonce("c").fold(err => fail(err), identity)
    def header(method: SipMethod) = endpoint
      .authorizationFor(
        AuthKind.Origin,
        method,
        uri("sip:sip.example.com"),
        challenge,
        cnonce,
        NonceCount.One
      )
      .value
    assertNotEquals(header(SipMethod.Register), header(SipMethod.Invite))
    assert(header(SipMethod.Register).startsWith("Digest username="))
  }
}
