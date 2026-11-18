package softphone4s.model

import com.comcast.ip4s.*
import munit.FunSuite

class ViaSuite extends FunSuite {

  private def parsed(s: String): Via =
    Via.parse(s).fold(err => fail(err), identity)

  test("parses a typical request Via") {
    val via = parsed("SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bK776asdhds")
    assertEquals(via.transport, TransportProtocol.Udp)
    assertEquals(via.host, ip"192.168.1.1")
    assertEquals(via.port, Some(port"5060"))
    assertEquals(via.branch.map(_.value), Some("z9hG4bK776asdhds"))
    assert(via.branch.exists(_.isRfc3261))
  }

  test("parses rport and received, requested and assigned") {
    assertEquals(parsed("SIP/2.0/UDP h.com;rport").rport, Some(RPort.Requested))
    val via = parsed("SIP/2.0/UDP h.com;rport=5099;received=203.0.113.7")
    assertEquals(via.rport, Some(RPort.Assigned(port"5099")))
    assertEquals(via.received, Some(ip"203.0.113.7"))
  }

  test("tolerates whitespace around the protocol and a lower-case transport") {
    assertEquals(
      parsed("SIP / 2.0 / tcp h.com:5070").transport,
      TransportProtocol.Tcp
    )
  }

  test("keeps other parameters, including quoted values") {
    val via = parsed("SIP/2.0/TLS [2001:db8::1]:5061;ttl=16;x=\"a;b\"")
    assertEquals(via.host, ip"2001:db8::1")
    assertEquals(
      via.params.toList.map((n, v) => n.value -> v.map(_.value)),
      List("ttl" -> Some("16"), "x" -> Some("\"a;b\""))
    )
  }

  test("render round-trips") {
    List(
      "SIP/2.0/UDP 192.168.1.1:5060;branch=z9hG4bK1",
      "SIP/2.0/UDP h.com;branch=z9hG4bK1;rport;received=1.2.3.4",
      "SIP/2.0/TLS [2001:db8::1]:5061;branch=z9hG4bK2;ttl=16"
    ).foreach(s => assertEquals(parsed(s).render, s))
  }

  test("rejects malformed Vias") {
    List(
      "",
      "UDP 1.2.3.4",
      "SIP/2.0/CARRIER 1.2.3.4",
      "SIP/2.0/UDP",
      "SIP/2.0/UDP 1.2.3.4:99999",
      "SIP/2.0/UDP 1.2.3.4;rport=abc",
      "SIP/2.0/UDP 1.2.3.4;received=not-an-ip",
      "SIP/2.0/UDP 1.2.3.4;a=1;a=2"
    ).foreach(s => assert(Via.parse(s).isLeft, s"expected Left for: '$s'"))
  }

  test("Tag, Branch and CallId from bytes are valid and well formed") {
    val bytes = Array[Byte](1, 2, 3, -1)
    assertEquals(Tag.fromBytes(bytes).value, "010203ff")
    assertEquals(Branch.fromBytes(bytes).value, "z9hG4bK010203ff")
    assert(Branch.fromBytes(bytes).isRfc3261)
    assertEquals(
      CallId.fromBytes(bytes, ip"10.0.0.1").value,
      "010203ff@10.0.0.1"
    )
  }

  test("Tag, Branch and CallId reject invalid text") {
    assert(Tag("").isLeft)
    assert(Tag("a b").isLeft)
    assert(Branch("a;b").isLeft)
    assert(CallId("abc@host:5060").isRight)
    assert(CallId("a b").isLeft)
  }

  // ---- rport and received (RFC 3581) -----------------------------------------------

  private val source = SocketAddress(ip"203.0.113.7", port"41000")

  test("a request can ask for rport, and it renders as a bare parameter") {
    assertEquals(
      parsed(
        "SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKa"
      ).requestingRport.render,
      "SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKa;rport"
    )
  }

  test(
    "the server fills in rport and received when rport was asked for, even if they match"
  ) {
    val seen =
      parsed("SIP/2.0/UDP 203.0.113.7:5060;branch=z9hG4bKa;rport").seenFrom(
        source
      )
    assertEquals(seen.rport, Some(RPort.Assigned(port"41000")))
    assertEquals(seen.received, Some(ip"203.0.113.7"))
  }

  test(
    "without rport, received is added only if the sent-by is not where it came from"
  ) {
    val same =
      parsed("SIP/2.0/UDP 203.0.113.7:5060;branch=z9hG4bKa").seenFrom(source)
    assertEquals(same.received, None)
    assertEquals(same.rport, None)
    val other =
      parsed("SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKa").seenFrom(source)
    assertEquals(other.received, Some(ip"203.0.113.7"))
    assertEquals(other.rport, None)
  }

  test("a sent-by that is a name always gets received") {
    assertEquals(
      parsed("SIP/2.0/UDP phone.example.com;branch=z9hG4bKa")
        .seenFrom(source)
        .received,
      Some(ip"203.0.113.7")
    )
  }

  test("an rport that already has a value is left as it is") {
    val seen =
      parsed("SIP/2.0/UDP 10.0.0.2;rport=5099;branch=z9hG4bKa").seenFrom(source)
    assertEquals(seen.rport, Some(RPort.Assigned(port"5099")))
  }

  test("the address a server saw is read from received and rport") {
    assertEquals(
      parsed(
        "SIP/2.0/UDP 10.0.0.2:5060;rport=41000;received=203.0.113.7"
      ).observedAddress,
      Some(source)
    )
  }

  test("a part the server does not give is ours from the sent-by") {
    assertEquals(
      parsed("SIP/2.0/UDP 10.0.0.2:5060;received=203.0.113.7").observedAddress,
      Some(SocketAddress(ip"203.0.113.7", port"5060"))
    )
    assertEquals(
      parsed("SIP/2.0/UDP 10.0.0.2:5060;rport=41000").observedAddress,
      Some(SocketAddress(ip"10.0.0.2", port"41000"))
    )
  }

  test("a Via with neither received nor an assigned rport observes nothing") {
    assertEquals(
      parsed("SIP/2.0/UDP 10.0.0.2:5060;rport").observedAddress,
      None
    )
    assertEquals(
      parsed("SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKa").observedAddress,
      None
    )
  }

  test("seenFrom then observedAddress gives the source back") {
    val sent =
      parsed("SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKa").requestingRport
    assertEquals(sent.seenFrom(source).observedAddress, Some(source))
  }
}
