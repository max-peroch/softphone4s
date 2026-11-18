package softphone4s.fsm

import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.config.AccountConfig
import softphone4s.fsm.SipMessages.*
import softphone4s.model.*

class StreamSwitchSuite extends FunSuite {

  private val udp = LocalEndpoint
    .from(
      AccountConfig(sipUser("alice"), Secret("pw"), host"sip.example.com"),
      ip"0.0.0.0",
      ip"192.168.1.5",
      port"5060",
      port"8000",
      TransportProtocol.Udp
    )
    .fold(e => fail(e.getMessage), identity)

  private val tcp = udp
    .via(ip"10.9.8.7", TransportProtocol.Tcp)
    .fold(e => fail(e.getMessage), identity)

  private def request(extra: String): SipRequest =
    udp.buildInvite(
      uri("sip:bob@example.com"),
      callId("s@h"),
      tag("lt"),
      cseqNumber(1),
      branch("z9hG4bKswitch"),
      extraHeaders = List(
        ExtensionHeader(FieldName("X-Pad").fold(fail(_), identity), extra)
      )
    )

  test("a UDP request is too big once it is longer than the limit") {
    assert(!StreamSwitch.tooBigForUdp(request("x"), 1300))
    assert(StreamSwitch.tooBigForUdp(request("x" * 1500), 1300))
    assert(StreamSwitch.tooBigForUdp(request("x"), 100))
  }

  test("a request that is not for UDP is never too big") {
    val overTcp = StreamSwitch.over(request("x" * 1500), tcp)
    assert(!StreamSwitch.tooBigForUdp(overTcp, 100))
  }

  test(
    "over: the top Via and the Contact become those of the stream endpoint"
  ) {
    val before = request("x")
    val after  = StreamSwitch.over(before, tcp)
    assertEquals(after.headers.topVia.transport, TransportProtocol.Tcp)
    assertEquals(after.headers.topVia.host, ip"10.9.8.7")
    assertEquals(after.headers.topVia.port, Some(port"5060"))
    assert(
      after.headers.topVia.hasAlias,
      "a stream Via asks for the connection to be reused"
    )
    assertEquals(after.headers.contacts.map(_.uri), List(tcp.contact))
  }

  test(
    "over: it is the same transaction, so the branch, the target and the body stay"
  ) {
    val before = request("x")
    val after  = StreamSwitch.over(before, tcp)
    assertEquals(after.headers.topVia.branch, before.headers.topVia.branch)
    assertEquals(after.requestUri, before.requestUri)
    assertEquals(after.body, before.body)
    assertEquals(after.headers.callId, before.headers.callId)
    assertEquals(after.headers.cseq, before.headers.cseq)
  }

  test("over: Vias below the top are left alone") {
    val before  = request("x")
    val below   = via("SIP/2.0/UDP proxy.example.com;branch=z9hG4bKbelow")
    val stacked = before.copy(headers =
      before.headers.copy(vias =
        cats.data.NonEmptyList.of(before.headers.topVia, below)
      )
    )
    assertEquals(StreamSwitch.over(stacked, tcp).headers.vias.tail, List(below))
  }

  test("over: a request with no Contact is given none") {
    val bare =
      request("x").copy(headers = request("x").headers.copy(contacts = Nil))
    assertEquals(StreamSwitch.over(bare, tcp).headers.contacts, Nil)
  }

  test("reissued: only the branch changes") {
    val before = request("x")
    val after  = StreamSwitch.reissued(before, branch("z9hG4bKagain"))
    assertEquals(after.headers.topVia.branch, Some(branch("z9hG4bKagain")))
    assertEquals(
      after.headers.topVia.transport,
      before.headers.topVia.transport
    )
    assertEquals(
      after.copy(headers = after.headers.copy(vias = before.headers.vias)),
      before
    )
  }
}
