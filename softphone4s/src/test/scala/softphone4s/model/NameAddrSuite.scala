package softphone4s.model

import munit.FunSuite

class NameAddrSuite extends FunSuite {

  private def parsed(s: String): NameAddr =
    NameAddr.parse(s).fold(err => fail(err), identity)

  test("parses an angle-bracketed address with a tag") {
    val na = parsed("<sip:alice@example.com>;tag=1928301774")
    assertEquals(na.displayName, None)
    assertEquals(na.uri.render, "sip:alice@example.com")
    assertEquals(na.tag.map(_.value), Some("1928301774"))
  }

  test("parses quoted and unquoted display names") {
    assertEquals(
      parsed("\"Alice \\\"A\\\" Smith\" <sip:a@b.com>").displayName,
      Some("Alice \"A\" Smith")
    )
    assertEquals(parsed("Alice <sip:a@b.com>").displayName, Some("Alice"))
  }

  test("a '<' inside a quoted display name does not start the address") {
    val na = parsed("\"a<b\" <sip:a@b.com>")
    assertEquals(na.displayName, Some("a<b"))
    assertEquals(na.uri.render, "sip:a@b.com")
  }

  test("parameters of a bare URI belong to the header, not the URI") {
    val na = parsed("sip:alice@example.com;tag=abc")
    assertEquals(na.uri.render, "sip:alice@example.com")
    assertEquals(na.tag.map(_.value), Some("abc"))
  }

  test("URI parameters inside the brackets stay on the URI") {
    val na = parsed("<sip:proxy.example.com;lr>;tag=1")
    assert(na.uri.isLooseRouting)
  }

  test("keeps other header parameters, including quoted values") {
    val na = parsed("<sip:a@b.com>;expires=3600;+sip.instance=\"<urn:uuid:1>\"")
    assertEquals(
      na.params.toList.map((n, v) => n.value -> v.map(_.value)),
      List(
        "expires"       -> Some("3600"),
        "+sip.instance" -> Some("\"<urn:uuid:1>\"")
      )
    )
  }

  test("render round-trips and always brackets the URI") {
    List(
      "<sip:alice@example.com>",
      "<sip:alice@example.com>;tag=abc",
      "\"Alice\" <sip:alice@example.com>;tag=abc;expires=60"
    ).foreach(s => assertEquals(parsed(s).render, s))
    assertEquals(parsed("sip:a@b.com;tag=x").render, "<sip:a@b.com>;tag=x")
  }

  test("rejects malformed values") {
    List(
      "",
      "<sip:a@b.com",
      "<tel:+1555>",
      "<sip:a@b.com>;tag",
      "<sip:a@b.com>;tag=a b",
      "<sip:a@b.com>;x=1;x=2"
    ).foreach(s => assert(NameAddr.parse(s).isLeft, s"expected Left for: '$s'"))
  }
}
