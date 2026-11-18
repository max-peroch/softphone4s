package softphone4s.model

import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.SipLiterals.sipUser
import softphone4s.config.AccountConfig

/** The address and port media is advertised at: the NAT's mapping of the RTP
  * port if a server said, then its mapping of the SIP port (for the address),
  * then our own.
  */
class LocalEndpointMediaSuite extends FunSuite {

  private val account = AccountConfig(
    sipUser("alice"),
    Secret("secret"),
    host"example.com"
  )

  private val plain = LocalEndpoint
    .from(
      account,
      ip"0.0.0.0",
      ip"192.168.1.10",
      port"5060",
      port"8000",
      TransportProtocol.Udp
    )
    .fold(e => fail(e.toString), identity)

  private val sipSeen = SocketAddress(ip"203.0.113.7", port"41000")
  private val rtpSeen = SocketAddress(ip"203.0.113.9", port"52000")

  test("with no mapping, media is advertised where we are") {
    assertEquals(plain.mediaIp, ip"192.168.1.10")
    assertEquals(plain.mediaPort, port"8000")
  }

  test(
    "the mapping of the SIP port gives the address, and not the port, which is the RTP port's"
  ) {
    val seen = plain.mappedTo(sipSeen).fold(e => fail(e.toString), identity)
    assertEquals(seen.mediaIp, ip"203.0.113.7")
    assertEquals(seen.mediaPort, port"8000")
  }

  test(
    "the mapping of the RTP port gives the address and the port, over the SIP port's"
  ) {
    val seen = plain
      .mappedTo(sipSeen)
      .fold(e => fail(e.toString), identity)
      .withRtpMapping(rtpSeen)
    assertEquals(seen.mediaIp, ip"203.0.113.9")
    assertEquals(seen.mediaPort, port"52000")
    // the port we bind is still ours
    assertEquals(seen.rtpPort, port"8000")
  }

  test(
    "the mapping of the RTP port survives the endpoint being made for another route"
  ) {
    val seen  = plain.withRtpMapping(rtpSeen)
    val other = seen
      .via(ip"10.0.0.5", TransportProtocol.Tcp)
      .fold(e => fail(e.toString), identity)
    assertEquals(other.mediaPort, port"52000")
  }
}
