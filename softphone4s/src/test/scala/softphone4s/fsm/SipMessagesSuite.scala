package softphone4s.fsm

import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.config.{AccountConfig, SoftphoneConfig}
import softphone4s.fsm.SipMessages.*
import softphone4s.model.*

import scala.concurrent.duration.*

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
      "SIP/2.0/UDP 192.168.1.5:5060;branch=z9hG4bKx;rport"
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
        "",
        challenge,
        cnonce,
        NonceCount.One
      )
      .value
    assertNotEquals(header(SipMethod.Register), header(SipMethod.Invite))
    assert(header(SipMethod.Register).startsWith("Digest username="))
  }

  private val incomingInvite = SipRequest(
    SipMethod.Invite,
    uri("sip:alice@sip.example.com"),
    SipHeaders(
      callId = callId("in@host"),
      from = nameAddr("<sip:bob@example.com>;tag=ft"),
      to = nameAddr("<sip:alice@sip.example.com>"),
      vias = cats.data.NonEmptyList
        .one(via("SIP/2.0/UDP 192.0.2.5;branch=z9hG4bKin")),
      cseq = cseq(4, SipMethod.Invite),
      contacts = List(nameAddr("<sip:bob@192.0.2.5>")),
      recordRoutes = List(nameAddr("<sip:p1.example.com;lr>"))
    )
  )

  test(
    "a 200 to an incoming INVITE carries our tag, Contact, route and answer"
  ) {
    val ok = endpoint.buildInviteResponse(
      incomingInvite,
      200,
      "OK",
      tag("lt"),
      Some("v=0\r\n")
    )
    assertEquals(ok.statusCode, 200)
    assertEquals(ok.headers.to.tag.map(_.value), Some("lt"))
    assertEquals(ok.headers.from, incomingInvite.headers.from)
    assertEquals(ok.headers.cseq, incomingInvite.headers.cseq)
    assertEquals(ok.headers.vias, incomingInvite.headers.vias)
    assertEquals(
      ok.headers.contacts.map(_.render),
      List("<sip:alice@192.168.1.5:5060>")
    )
    assertEquals(ok.headers.recordRoutes, incomingInvite.headers.recordRoutes)
    assertEquals(ok.headers.contentType, Some(MediaType.Sdp))
    assertEquals(ok.body, "v=0\r\n")
  }

  test(
    "180 forms the dialog too; a failure carries neither Contact nor route"
  ) {
    val ringing =
      endpoint.buildInviteResponse(incomingInvite, 180, "Ringing", tag("lt"))
    assertEquals(ringing.headers.contacts.size, 1)
    assertEquals(ringing.headers.recordRoutes.size, 1)
    val busy =
      endpoint.buildInviteResponse(incomingInvite, 486, "Busy Here", tag("lt"))
    assertEquals(busy.headers.contacts, Nil)
    assertEquals(busy.headers.recordRoutes, Nil)
    assertEquals(busy.headers.to.tag.map(_.value), Some("lt"))
    assertEquals(busy.body, "")
  }

  private def routedDialog(routes: String*): SipDialog =
    SipDialog(
      callId = callId("rt@host"),
      localTag = tag("lt"),
      remoteTag = Some(tag("rt")),
      localUri = uri("sip:alice@sip.example.com"),
      remoteUri = uri("sip:bob@example.com"),
      remoteTarget = uri("sip:bob@192.0.2.5:5070"),
      localCseq = cseqNumber(1),
      routeSet = routes.toList.map(nameAddr),
      role = DialogRole.Uas
    )

  test(
    "a BYE and an ACK follow a loose route set: target as request-URI, routes as Route headers"
  ) {
    val d = routedDialog("<sip:p1.example.com;lr>")
    List(
      endpoint.buildBye(d, branch("z9hG4bKb")),
      endpoint.buildAck(d, branch("z9hG4bKa"), None)
    ).foreach { request =>
      assertEquals(request.requestUri, uri("sip:bob@192.0.2.5:5070"))
      assertEquals(
        request.headers.routes.map(_.render),
        List("<sip:p1.example.com;lr>")
      )
    }
  }

  test(
    "a BYE follows a strict route: the router's URI is the request-URI and the target is last"
  ) {
    val bye = endpoint.buildBye(
      routedDialog("<sip:strict.example.com>"),
      branch("z9hG4bKb")
    )
    assertEquals(bye.requestUri, uri("sip:strict.example.com"))
    assertEquals(
      bye.headers.routes.map(_.render),
      List("<sip:bob@192.0.2.5:5070>")
    )
  }

  test("without a route set a BYE carries no Route") {
    val bye = endpoint.buildBye(routedDialog(), branch("z9hG4bKb"))
    assertEquals(bye.requestUri, uri("sip:bob@192.0.2.5:5070"))
    assertEquals(bye.headers.routes, Nil)
  }

  test(
    "an INFO carries its digit in a dtmf-relay body, with the dialog's CSeq and route"
  ) {
    val d =
      routedDialog("<sip:p1.example.com;lr>").copy(localCseq = cseqNumber(4))
    val info =
      endpoint.buildInfo(d, branch("z9hG4bKi"), DtmfDigit.Hash, 250.millis)
    assertEquals(info.method, SipMethod.Info)
    assertEquals(info.requestUri, uri("sip:bob@192.0.2.5:5070"))
    assertEquals(info.headers.cseq, cseq(4, SipMethod.Info))
    assertEquals(info.headers.contentType, Some(MediaType.DtmfRelay))
    assertEquals(info.body, "Signal=#\r\nDuration=250\r\n")
    assertEquals(
      info.headers.routes.map(_.render),
      List("<sip:p1.example.com;lr>")
    )
  }

  test(
    "a re-INVITE carries the dialog's next CSeq, its route, and an SDP offer in the chosen direction"
  ) {
    val d = routedDialog("<sip:p1.example.com;lr>")
      .copy(localCseq = cseqNumber(4))
    val r =
      endpoint.buildReinvite(d, branch("z9hG4bKr"), MediaDirection.SendOnly)
    assertEquals(r.method, SipMethod.Invite)
    assertEquals(r.headers.cseq, CSeq(cseqNumber(4), SipMethod.Invite))
    assertEquals(r.requestUri, uri("sip:bob@192.0.2.5:5070"))
    assertEquals(
      r.headers.routes.map(_.render),
      List("<sip:p1.example.com;lr>")
    )
    assertEquals(r.headers.to.tag, Some(tag("rt")))
    assertEquals(r.headers.from.tag, Some(tag("lt")))
    assertEquals(r.headers.contentType, Some(MediaType.Sdp))
    assert(r.body.contains("a=sendonly\r\n"), r.body)
    assert(
      endpoint
        .buildReinvite(d, branch("z9hG4bKr"), MediaDirection.SendRecv)
        .body
        .contains("a=sendrecv")
    )
  }

  test("an UPDATE is built like the re-INVITE, with its own method and CSeq") {
    val d =
      routedDialog("<sip:p1.example.com;lr>").copy(localCseq = cseqNumber(5))
    val r = endpoint.buildSessionRequest(
      d,
      branch("z9hG4bKu"),
      SessionChange.HoldByUpdate
    )
    assertEquals(r.method, SipMethod.Update)
    assertEquals(r.headers.cseq, CSeq(cseqNumber(5), SipMethod.Update))
    assertEquals(
      r.headers.routes.map(_.render),
      List("<sip:p1.example.com;lr>")
    )
    assert(r.body.contains("a=sendonly"), r.body)
  }

  test("an endpoint speaking TCP says so in its Via, and in its Contact") {
    val tcp = ok(
      LocalEndpoint.from(
        AccountConfig("alice", "pw", "sip.example.com"),
        ip"0.0.0.0",
        ip"192.168.1.5",
        port"5060",
        port"8000",
        TransportProtocol.Tcp
      )
    )
    val invite = tcp.buildInvite(
      uri("sip:bob@example.com"),
      callId("t@h"),
      tag("lt"),
      cseqNumber(1),
      branch("z9hG4bKt")
    )
    assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
    assertEquals(
      invite.headers.contacts.map(_.render),
      List("<sip:alice@192.168.1.5:5060;transport=tcp>")
    )
    // The default endpoint is unchanged.
    val udp = endpoint.buildInvite(
      uri("sip:bob@example.com"),
      callId("t@h"),
      tag("lt"),
      cseqNumber(1),
      branch("z9hG4bKt")
    )
    assertEquals(udp.headers.topVia.transport, TransportProtocol.Udp)
    assert(!udp.headers.contacts.head.render.contains("transport"))
  }

  test(
    "an endpoint for a sips: request has a sips: Contact with no transport parameter"
  ) {
    val tls = ok(
      LocalEndpoint.from(
        AccountConfig("alice", "pw", "sip.example.com"),
        ip"0.0.0.0",
        ip"192.168.1.5",
        port"5061",
        port"8000",
        TransportProtocol.Tls
      )
    )
    assertEquals(tls.contact.render, "sip:alice@192.168.1.5:5061;transport=tls")
    val secure =
      tls.forScheme(SipScheme.Sips).fold(e => fail(e.getMessage), identity)
    assertEquals(secure.contact.render, "sips:alice@192.168.1.5:5061")
    // It stays secure when the call moves to another address.
    val moved = secure
      .via(ip"10.0.0.9", TransportProtocol.Tls)
      .fold(e => fail(e.getMessage), identity)
    assertEquals(moved.contact.render, "sips:alice@10.0.0.9:5061")
  }

  test("an endpoint is left as it is for a sip: request") {
    assertEquals(endpoint.forScheme(SipScheme.Sip), Right(endpoint))
  }

  test(
    "a REGISTER, even with an exaggerated credential, stays under a datagram's limit"
  ) {
    // Why REGISTER needs no switch to TCP: its content is fixed (one Contact,
    // no headers of the user's). Every field of this credential is several
    // times the usual length and the request is still 400 bytes short.
    val credentials = ExtensionHeader(
      FieldName.Authorization,
      "Digest username=\"" + "u" * 64 + "\", realm=\"" + "r" * 64 + "\", nonce=\"" + "n" * 128 +
        "\", uri=\"sip:" + "d" * 64 + "\", response=\"" + "f" * 64 + "\", algorithm=SHA-256, " +
        "cnonce=\"" + "c" * 32 + "\", qop=auth, nc=00000001, opaque=\"" + "o" * 64 + "\""
    )
    val register = endpoint.buildRegister(
      uri("sip:sip.example.com"),
      callId("reg@h"),
      tag("lt"),
      cseqNumber(1),
      branch("z9hG4bKr"),
      Expires.of(scala.concurrent.duration.FiniteDuration(1, "hour")),
      Some(credentials)
    )
    val size = softphone4s.codec.SipCodec.encode(register).size
    assert(size < 1300 - 300, s"a REGISTER of $size bytes")
  }

  // ---- Behind a NAT ---------------------------------------------------------------------

  private val outsideAddress = SocketAddress(ip"203.0.113.7", port"41000")

  test(
    "a UDP endpoint seen at another address has the Contact and media address of that one"
  ) {
    val seen =
      endpoint.mappedTo(outsideAddress).fold(e => fail(e.getMessage), identity)
    assertEquals(seen.contact.render, "sip:alice@203.0.113.7:41000")
    assertEquals(seen.mediaIp, ip"203.0.113.7")
    // The Via stays our own: answers find us by rport.
    assertEquals(seen.ip, endpoint.ip)
  }

  test(
    "the INVITE of an endpoint seen from outside offers the outside address in its SDP and Contact"
  ) {
    val seen =
      endpoint.mappedTo(outsideAddress).fold(e => fail(e.getMessage), identity)
    val invite = seen.buildInvite(
      uri("sip:bob@example.com"),
      callId("n@h"),
      tag("lt"),
      cseqNumber(1),
      branch("z9hG4bKn")
    )
    assert(invite.body.contains("c=IN IP4 203.0.113.7"), invite.body)
    assertEquals(
      invite.headers.contacts.map(_.uri.render),
      List("sip:alice@203.0.113.7:41000")
    )
    assertEquals(invite.headers.topVia.host, endpoint.ip)
    assert(invite.headers.topVia.rport.contains(RPort.Requested))
  }

  test(
    "an endpoint seen at its own address, or over a stream, is left as it is"
  ) {
    val own = SocketAddress(endpoint.ip, endpoint.sipPort)
    assertEquals(endpoint.mappedTo(own), Right(endpoint))
    val tcp = endpoint
      .via(endpoint.ip, TransportProtocol.Tcp)
      .fold(e => fail(e.getMessage), identity)
    assertEquals(tcp.mappedTo(outsideAddress), Right(tcp))
  }

  test("moving to another target forgets what the outside looked like") {
    val seen =
      endpoint.mappedTo(outsideAddress).fold(e => fail(e.getMessage), identity)
    val moved = seen
      .via(ip"10.9.8.7", TransportProtocol.Udp)
      .fold(e => fail(e.getMessage), identity)
    assertEquals(moved.mapping, None)
    assertEquals(moved.mediaIp, ip"10.9.8.7")
  }
}
