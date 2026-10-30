package softphone4s.model

import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.FunSuite

import scala.collection.immutable.ListMap

class SipUriSuite extends FunSuite {

  private def parsed(s: String): SipUri =
    SipUri.parse(s).fold(err => fail(err), identity)

  private def ok[A](e: Either[String, A]): A =
    e.fold(err => fail(err), identity)

  test("parses a host-only URI") {
    val uri = parsed("sip:example.com")
    assertEquals(uri.scheme, SipScheme.Sip)
    assertEquals(uri.userInfo, None)
    assertEquals(uri.host, host"example.com")
    assertEquals(uri.port, None)
    assertEquals(uri.routing, Routing.Strict)
  }

  test("parses user, password, host and port") {
    val uri = parsed("sips:alice:s3cr3t@sip.example.com:5061")
    assertEquals(uri.scheme, SipScheme.Sips)
    assertEquals(uri.user.map(_.value), Some("alice"))
    assertEquals(
      uri.userInfo.flatMap(_.password).map(_.value),
      Some("s3cr3t")
    )
    assertEquals(uri.host, host"sip.example.com")
    assertEquals(uri.port, Some(port"5061"))
  }

  test("host names are case-insensitive") {
    assertEquals(parsed("sip:a@EXAMPLE.com").host, host"example.com")
  }

  test("parses an IPv4 host") {
    assertEquals(parsed("sip:bob@192.168.1.10:5060").host, ip"192.168.1.10")
  }

  test("parses a bracketed IPv6 host with a port") {
    val uri = parsed("sip:bob@[2001:db8::1]:5060")
    assertEquals(uri.host, ip"2001:db8::1")
    assertEquals(uri.port, Some(port"5060"))
  }

  test("transport and lr become typed fields, case-insensitively") {
    val uri = parsed("sip:proxy.example.com;Transport=TCP;LR")
    assertEquals(uri.transport, Some(TransportProtocol.Tcp))
    assertEquals(uri.routing, Routing.Loose)
    assert(uri.isLooseRouting)
    assert(uri.params.isEmpty)
  }

  test("other parameters keep their order, with flags as None") {
    val uri = parsed("sip:a@b.com;x=1;flag;y=2")
    assertEquals(
      uri.params.toList.map((n, v) => n.value -> v.map(_.value)),
      List("x" -> Some("1"), "flag" -> None, "y" -> Some("2"))
    )
    assertEquals(uri.param(ok(ParamName("FLAG"))), Some(None))
  }

  test("parses URI headers, tolerating '@' in values") {
    val uri = parsed("sip:a@b.com?subject=hi&to=carol@example.com")
    assertEquals(
      uri.headers.map(h => h.name.value -> h.value.value),
      List("subject" -> "hi", "to" -> "carol@example.com")
    )
  }

  test("keeps user parameters (e.g. phone-context) inside the user part") {
    val uri = parsed("sip:+1555;phone-context=x.com@gw.example.com;user=phone")
    assertEquals(uri.user.map(_.value), Some("+1555;phone-context=x.com"))
    assertEquals(uri.params.keys.map(_.value).toList, List("user"))
  }

  test("scheme is case-insensitive") {
    assertEquals(parsed("SIP:a@b.com").scheme, SipScheme.Sip)
  }

  test("render round-trips canonical URIs") {
    List(
      "sip:example.com",
      "sip:alice@example.com",
      "sips:alice:pw@example.com:5061",
      "sip:bob@[2001:db8::1]:5060",
      "sip:proxy.example.com;transport=tcp;lr",
      "sip:a@b.com;x=1;flag?subject=hi&to=c@d.com"
    ).foreach(s => assertEquals(parsed(s).render, s))
  }

  test("the password never appears in Show output") {
    val password =
      parsed("sip:alice:s3cr3t@example.com").userInfo.flatMap(_.password)
    assertEquals(password.map(_.show), Some("***"))
  }

  test("equality ignores parameter order") {
    assertEquals(
      parsed("sip:a@b.com;x=1;y=2"),
      parsed("sip:a@b.com;y=2;x=1")
    )
  }

  test("withParam adds and replaces free parameters") {
    val uri = ok(
      parsed("sip:a@b.com;x=1")
        .withParam(ok(ParamName("x")), Some(ok(ParamValue("2"))))
    )
    assertEquals(uri.render, "sip:a@b.com;x=2")
  }

  test("withParam rejects the typed parameters") {
    assert(parsed("sip:a@b.com").withParam(ok(ParamName("transport"))).isLeft)
    assert(parsed("sip:a@b.com").withParam(ok(ParamName("lr"))).isLeft)
  }

  test("withTransport and withLooseRouting set the typed fields") {
    assertEquals(
      parsed("sip:a@b.com")
        .withTransport(TransportProtocol.Tls)
        .withLooseRouting
        .render,
      "sip:a@b.com;transport=tls;lr"
    )
  }

  test("bare drops parameters and headers") {
    assertEquals(
      parsed("sip:a@b.com;transport=tcp;lr;x=1?h=v").bare.render,
      "sip:a@b.com"
    )
  }

  test("from rejects transport and lr as free parameters") {
    val params =
      ListMap(ok(ParamName("transport")) -> Some(ok(ParamValue("udp"))))
    assert(SipUri.from(SipScheme.Sip, host"a.com", params = params).isLeft)
  }

  test("the constructor and copy are not accessible outside SipUri") {
    assert(
      compileErrors(
        "new SipUri(SipScheme.Sip, None, host\"a.com\", None, None, Routing.Strict, scala.collection.immutable.ListMap.empty, Nil)"
      ).nonEmpty
    )
    assert(
      compileErrors(
        """SipUri.parse("sip:a@b.com").toOption.get.copy(port = None)"""
      ).nonEmpty
    )
  }

  test("component types reject invalid text") {
    assert(User("").isLeft)
    assert(User("a b").isLeft)
    assert(User("a@b").isLeft)
    assert(ParamName("").isLeft)
    assert(ParamValue("a;b").isLeft)
    assert(HeaderName("a=b").isLeft)
    assert(User("%41lice").isRight)
    assert(User("%4").isLeft)
  }

  test("rejects malformed URIs") {
    List(
      "alice@example.com",
      "tel:+15551234567",
      "sip:",
      "sip:@example.com",
      "sip:a@b.com:99999",
      "sip:a@b.com:abc",
      "sip:a@[2001:db8::1",
      "sip:a@b.com;=v",
      "sip:a@b.com?novalue",
      "sip:a@b.com;x=1;X=2",
      "sip:a@b.com;transport=carrier-pigeon",
      "sip:a@b.com;transport",
      "sip:a@b.com;lr=1"
    ).foreach(s => assert(SipUri.parse(s).isLeft, s"expected Left for: $s"))
  }
}
