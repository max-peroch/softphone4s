package softphone4s.fsm

import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.config.{AccountConfig, SoftphoneConfig}
import softphone4s.codec.SipCodec
import softphone4s.fsm.SipMessages.*
import softphone4s.messaging.MessageContent
import softphone4s.model.*
import softphone4s.publication.{EntityTag, PublicationBody}

import scala.concurrent.duration.*

class SipMessagesSuite extends FunSuite {

  private def ok[A](e: Either[SoftphoneError, A]): A =
    e.fold(err => fail(err.getMessage), identity)

  private def callee(s: String): Callee =
    Callee(s).fold(err => fail(err), identity)

  private val config =
    SoftphoneConfig(
      user = sipUser("alice"),
      password = Secret("pw"),
      realm = host"sip.example.com"
    )

  private val endpoint =
    ok(LocalEndpoint.from(config, ip"192.168.1.5", port"5060", port"8000"))

  test("toSipUri puts a bare number at the account's realm") {
    assertEquals(
      ok(callee("1234").toSipUri(host"sip.example.com")).render,
      "sip:1234@sip.example.com"
    )
  }

  test("toSipUri keeps an explicit host and port") {
    val uri = ok(
      callee("bob@10.0.0.2:5070").toSipUri(host"ignored.com")
    )
    assertEquals(uri.render, "sip:bob@10.0.0.2:5070")
    assertEquals(uri.port, Some(port"5070"))
  }

  test("toSipUri percent-encodes '#' and keeps '*' and '+'") {
    assertEquals(
      ok(callee("*67#").toSipUri(host"h.com")).render,
      "sip:*67%23@h.com"
    )
    assertEquals(
      ok(callee("+15551234567").toSipUri(host"h.com")).render,
      "sip:+15551234567@h.com"
    )
  }

  test("toSipUri puts an IPv6 realm in brackets") {
    assertEquals(
      ok(callee("1234").toSipUri(ip"::1")).render,
      "sip:1234@[::1]"
    )
  }

  test(
    "LocalEndpoint derives the account address and Contact from typed parts"
  ) {
    assertEquals(endpoint.accountUri.render, "sip:alice@sip.example.com")
    val invite = endpoint.buildInvite(
      ok(callee("1234").toSipUri(host"sip.example.com")),
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
        AccountConfig(sipUser("alice"), Secret("pw"), host"sip.example.com"),
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
        AccountConfig(sipUser("alice"), Secret("pw"), host"sip.example.com"),
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

  // ---- MESSAGE ----------------------------------------------------------------------

  private def message(
      content: MessageContent = MessageContent.plain("hello"),
      auth: Option[ExtensionHeader] = None
  ) =
    endpoint.buildMessage(
      uri("sip:bob@sip.example.com"),
      callId("msg@host"),
      tag("ltag"),
      cseqNumber(1),
      branch("z9hG4bKmsg"),
      content,
      auth
    )

  test(
    "buildMessage is a request outside any dialog, from us to them, with the text in the body"
  ) {
    val m = message()
    assertEquals(m.method, SipMethod.Message)
    assertEquals(m.requestUri.render, "sip:bob@sip.example.com")
    assertEquals(m.headers.from.render, "<sip:alice@sip.example.com>;tag=ltag")
    // The To has no tag: there is no dialog.
    assertEquals(m.headers.to.render, "<sip:bob@sip.example.com>")
    assertEquals(m.headers.cseq, cseq(1, SipMethod.Message))
    assertEquals(m.headers.contentType, Some(MediaType.Plain))
    assertEquals(m.headers.contacts, Nil)
    assertEquals(m.body, "hello")
  }

  test(
    "buildMessage carries the authorization of a retry, and a later CSeq keeps the Call-ID"
  ) {
    val auth  = ExtensionHeader(FieldName.Authorization, "Digest x")
    val retry = endpoint.buildMessage(
      uri("sip:bob@sip.example.com"),
      callId("msg@host"),
      tag("ltag"),
      cseqNumber(2),
      branch("z9hG4bKmsg2"),
      MessageContent.plain("hello"),
      Some(auth)
    )
    assertEquals(retry.headers.others, List(auth))
    assertEquals(retry.headers.callId, message().headers.callId)
    assertEquals(retry.headers.cseq, cseq(2, SipMethod.Message))
  }

  test(
    "a message goes over the wire as UTF-8, with its length in bytes, and is read back"
  ) {
    val text = "héllo ✓ 日本"
    val wire =
      softphone4s.codec.SipCodec.encode(message(MessageContent.plain(text)))
    val read = softphone4s.codec.SipCodec
      .decode(wire)
      .fold(e => fail(e.message), identity)
    read match {
      case request: SipRequest =>
        assertEquals(request.method, SipMethod.Message)
        assertEquals(request.body, text)
      case other => fail(s"expected a request: $other")
    }
    val raw = new String(wire.toArray, "UTF-8")
    assert(raw.startsWith("MESSAGE sip:bob@sip.example.com SIP/2.0"), raw)
    assert(
      raw.contains(s"Content-Length: ${text.getBytes("UTF-8").length}"),
      raw
    )
  }

  test("a type other than text/plain is carried as it is") {
    val html =
      MediaType.parse("text/html;charset=utf-8").fold(e => fail(e), identity)
    assertEquals(
      message(MessageContent(html, "<b>hi</b>")).headers.contentType
        .map(_.render),
      Some("text/html;charset=utf-8")
    )
  }

  test("the 200 to a MESSAGE answers it and carries no body") {
    val ok200 = messageOk(message())
    assertEquals((ok200.statusCode, ok200.body), (200, ""))
    assertEquals(ok200.headers.cseq, cseq(1, SipMethod.Message))
  }

  test("the 415 to a MESSAGE we cannot read says what we can") {
    val refusal = unsupportedMessage(
      message(),
      List(MediaType.Plain, MediaType.Sdp)
    )
    assertEquals(refusal.statusCode, 415)
    assertEquals(
      refusal.headers.others.map(h => (h.name.value, h.value)),
      List(("Accept", "text/plain, application/sdp"))
    )
  }

  test("the answer to OPTIONS lists MESSAGE among what we take") {
    val options = message().copy(method = SipMethod.Options)
    val allow   = endpoint
      .buildOptionsOk(options)
      .headers
      .others
      .find(_.name.matches("Allow"))
      .map(_.value)
    assert(allow.exists(_.contains("MESSAGE")), allow)
  }

  // ---- SUBSCRIBE --------------------------------------------------------------------

  private val presenceEvent =
    softphone4s.subscription
      .EventHeader("presence")
      .fold(e => fail(e), identity)
  private val pidf =
    MediaType.parse("application/pidf+xml").fold(e => fail(e), identity)

  private def subscribe(
      accept: List[MediaType] = List(pidf),
      auth: Option[ExtensionHeader] = None
  ) =
    endpoint.buildSubscribe(
      uri("sip:bob@sip.example.com"),
      callId("sub@host"),
      tag("ltag"),
      cseqNumber(1),
      branch("z9hG4bKsub"),
      presenceEvent,
      Expires(3600).fold(e => fail(e), identity),
      accept,
      auth
    )

  private def etag(text: String) =
    EntityTag.parse(text).fold(e => fail(e), identity)

  private def publish(
      entityTag: Option[EntityTag],
      body: Option[PublicationBody],
      lifetime: Int = 3600,
      auth: Option[ExtensionHeader] = None
  ) =
    endpoint.buildPublish(
      uri("sip:alice@sip.example.com"),
      callId("pub@host"),
      tag("ltag"),
      cseqNumber(1),
      branch("z9hG4bKpub"),
      presenceEvent,
      Expires(lifetime).fold(e => fail(e), identity),
      entityTag,
      body,
      auth
    )

  private val document = PublicationBody(pidf, "<presence/>")

  test("the first PUBLISH carries the state and no tag") {
    val request = publish(None, Some(document))
    assertEquals(request.method, SipMethod.Publish)
    assertEquals(request.requestUri.render, "sip:alice@sip.example.com")
    assertEquals(
      request.headers.from.render,
      "<sip:alice@sip.example.com>;tag=ltag"
    )
    assertEquals(request.headers.to.render, "<sip:alice@sip.example.com>")
    assertEquals(request.headers.cseq, cseq(1, SipMethod.Publish))
    assertEquals(extra(request, "Event"), Some("presence"))
    assertEquals(extra(request, "SIP-If-Match"), None)
    assertEquals(request.headers.expires.map(_.value), Some(3600))
    assertEquals(request.headers.contentType, Some(pidf))
    assertEquals(request.body, "<presence/>")
    // Nothing comes back to us in a PUBLISH.
    assertEquals(request.headers.contacts, Nil)
  }

  test(
    "a refresh has the tag and no body, a change has both, a removal Expires 0"
  ) {
    val refresh = publish(Some(etag("dx200xyz")), None)
    assertEquals(extra(refresh, "SIP-If-Match"), Some("dx200xyz"))
    assertEquals(refresh.body, "")
    assertEquals(refresh.headers.contentType, None)
    val change = publish(Some(etag("dx200xyz")), Some(document))
    assertEquals(extra(change, "SIP-If-Match"), Some("dx200xyz"))
    assertEquals(change.body, "<presence/>")
    val removal = publish(Some(etag("dx200xyz")), None, lifetime = 0)
    assert(removal.headers.expires.exists(_.isZero))
  }

  test("a retry after a challenge carries the Authorization") {
    val auth = ExtensionHeader(FieldName.Authorization, "Digest x")
    assertEquals(
      extra(publish(None, Some(document), auth = Some(auth)), "Authorization"),
      Some("Digest x")
    )
  }

  test("a PUBLISH is encoded and read back") {
    val request = publish(Some(etag("t1")), Some(document))
    assertEquals(SipCodec.decode(SipCodec.encode(request)), Right(request))
  }

  private def extra(request: SipRequest, name: String) =
    request.headers.others.find(_.name.matches(name)).map(_.value)

  test(
    "buildSubscribe is outside any dialog: from us, to the resource, with our Contact"
  ) {
    val request = subscribe()
    assertEquals(request.method, SipMethod.Subscribe)
    assertEquals(request.requestUri.render, "sip:bob@sip.example.com")
    assertEquals(
      request.headers.from.render,
      "<sip:alice@sip.example.com>;tag=ltag"
    )
    assertEquals(request.headers.to.render, "<sip:bob@sip.example.com>")
    assertEquals(request.headers.cseq, cseq(1, SipMethod.Subscribe))
    // The NOTIFYs come to the Contact, so a SUBSCRIBE must have one.
    assertEquals(
      request.headers.contacts.map(_.render),
      List("<sip:alice@192.168.1.5:5060>")
    )
    assertEquals(request.body, "")
  }

  test("it names the event, the lifetime asked for and the types it takes") {
    val request = subscribe()
    assertEquals(extra(request, "Event"), Some("presence"))
    assertEquals(extra(request, "Accept"), Some("application/pidf+xml"))
    assertEquals(request.headers.expires.map(_.value), Some(3600))
  }

  test(
    "with no types to accept there is no Accept, and a retry carries the Authorization"
  ) {
    assertEquals(extra(subscribe(accept = Nil), "Accept"), None)
    val auth = ExtensionHeader(FieldName.Authorization, "Digest x")
    assertEquals(
      extra(subscribe(auth = Some(auth)), "Authorization"),
      Some("Digest x")
    )
  }

  test("a SUBSCRIBE goes over the wire with its Event, Expires and Accept") {
    val text = new String(
      softphone4s.codec.SipCodec.encode(subscribe()).toArray,
      "UTF-8"
    )
    assert(text.startsWith("SUBSCRIBE sip:bob@sip.example.com SIP/2.0"), text)
    assert(text.contains("Event: presence"), text)
    assert(text.contains("Expires: 3600"), text)
    assert(text.contains("Accept: application/pidf+xml"), text)
    assert(text.contains("CSeq: 1 SUBSCRIBE"), text)
  }

  private def subscriptionDialog(routes: List[NameAddr] = Nil) =
    SipDialog(
      callId = callId("sub@host"),
      localTag = tag("ltag"),
      remoteTag = Some(tag("srv")),
      localUri = uri("sip:alice@sip.example.com"),
      remoteUri = uri("sip:bob@sip.example.com"),
      remoteTarget = uri("sip:bob@198.51.100.7:5070"),
      localCseq = cseqNumber(1),
      routeSet = routes
    )

  private def refresh(dialog: SipDialog, lifetime: Int = 3600) =
    endpoint.buildSubscribeIn(
      dialog,
      cseqNumber(2),
      branch("z9hG4bKref"),
      presenceEvent,
      Expires(lifetime).fold(e => fail(e), identity),
      List(pidf)
    )

  test(
    "a SUBSCRIBE in the dialog goes to the notifier's Contact, with the tags of the dialog and the CSeq given"
  ) {
    val request = refresh(subscriptionDialog())
    assertEquals(request.requestUri.render, "sip:bob@198.51.100.7:5070")
    assertEquals(
      request.headers.from.render,
      "<sip:alice@sip.example.com>;tag=ltag"
    )
    assertEquals(request.headers.to.render, "<sip:bob@sip.example.com>;tag=srv")
    assertEquals(request.headers.callId, callId("sub@host"))
    assertEquals(request.headers.cseq, cseq(2, SipMethod.Subscribe))
    assertEquals(request.headers.routes, Nil)
    assertEquals(extra(request, "Event"), Some("presence"))
  }

  test(
    "ending a subscription is the same SUBSCRIBE with a lifetime of nothing"
  ) {
    assertEquals(
      refresh(subscriptionDialog(), lifetime = 0).headers.expires.map(_.value),
      Some(0)
    )
  }

  test(
    "a route set becomes Route headers, and a loose router first leaves the Contact as the Request-URI"
  ) {
    val routes = List(
      nameAddr("<sip:p1.example.com;lr>"),
      nameAddr("<sip:p2.example.com;lr>")
    )
    val request = refresh(subscriptionDialog(routes))
    // For the caller the route set is reversed (RFC 3261 §12.1.2).
    assertEquals(
      request.headers.routes.map(_.uri.render),
      List("sip:p2.example.com;lr", "sip:p1.example.com;lr")
    )
    assertEquals(request.requestUri.render, "sip:bob@198.51.100.7:5070")
  }
}
