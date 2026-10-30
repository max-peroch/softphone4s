package softphone4s.model

import munit.FunSuite

class CalleeSuite extends FunSuite {

  test("apply accepts digits only") {
    assert(Callee("1234").isRight)
  }

  test("apply accepts letters only") {
    assert(Callee("alice").isRight)
  }

  test("apply accepts a leading plus") {
    assert(Callee("+15551234567").isRight)
  }

  test("apply accepts star and hash") {
    assert(Callee("*67#").isRight)
  }

  test("apply accepts user@host") {
    assert(Callee("alice@sip.example.com").isRight)
  }

  test("apply accepts user@host:port") {
    assert(Callee("alice@192.168.1.10:5060").isRight)
  }

  test("apply rejects empty string") {
    assert(Callee("").isLeft)
  }

  test("apply rejects spaces") {
    assert(Callee("555 1234").isLeft)
    assert(Callee("alice@ho st").isLeft)
  }

  test("apply rejects pause/wait dial modifiers") {
    assert(Callee("1,800,555,1234").isLeft)
    assert(Callee("555!1234").isLeft)
  }

  test("apply rejects an empty host after @") {
    assert(Callee("alice@").isLeft)
  }

  test("apply rejects a non-numeric port") {
    assert(Callee("alice@host:abc").isLeft)
  }

  test("apply rejects multiple @ signs") {
    assert(Callee("alice@bob@host").isLeft)
  }

  test("value extension returns underlying string") {
    val callee = Callee.unsafe("alice@sip.example.com")
    assertEquals(callee.value, "alice@sip.example.com")
  }

  // ---- Whole SIP URIs ---------------------------------------------------------------

  private def dialled(s: String): SipUri =
    Callee(s)
      .flatMap(_.toSipUri("default.example").left.map(_.getMessage))
      .fold(err => fail(err), identity)

  test("a whole SIP URI is accepted and dialled as it is, whatever the realm") {
    val uri = dialled("sip:alice@example.com")
    assertEquals(uri.render, "sip:alice@example.com")
    assertEquals(uri.scheme, SipScheme.Sip)
  }

  test("a sips: URI asks for TLS") {
    val uri = dialled("sips:alice@example.com")
    assertEquals(uri.scheme, SipScheme.Sips)
    assertEquals(uri.render, "sips:alice@example.com")
  }

  test("a transport parameter is kept") {
    assertEquals(
      dialled("sip:alice@example.com;transport=tcp").transport,
      Some(TransportProtocol.Tcp)
    )
    assertEquals(
      dialled("sip:alice@example.com:5070;transport=tls").port.map(_.value),
      Some(5070)
    )
  }

  test("the scheme is recognised whatever its case") {
    assertEquals(dialled("SIPS:alice@example.com").scheme, SipScheme.Sips)
  }

  test("a malformed URI is rejected, rather than dialled") {
    assert(Callee("sip:").isLeft)
    assert(Callee("sips:@").isLeft)
    assert(Callee("sip:alice@exa mple.com").isLeft)
    assert(Callee("sip:alice@example.com;transport=").isLeft)
  }

  test("the short forms are dialled at the realm as before") {
    assertEquals(dialled("alice").render, "sip:alice@default.example")
    assertEquals(
      dialled("alice@other.example").render,
      "sip:alice@other.example"
    )
  }
}
