package softphone4s.dns

import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.model.TransportProtocol

class ServerLocationSuite extends FunSuite {

  import TransportProtocol.*

  private def naptr(
      order: Int,
      pref: Int,
      service: String,
      replacement: String,
      flags: String = "S"
  ) =
    NaptrRecord(order, pref, flags, service, replacement)

  test("SRV names carry the service and transport, and end in a dot") {
    assertEquals(
      ServerLocation.srvName(Udp, "example.com"),
      Some("_sip._udp.example.com.")
    )
    assertEquals(
      ServerLocation.srvName(Tcp, "example.com."),
      Some("_sip._tcp.example.com.")
    )
    assertEquals(
      ServerLocation.srvName(Tls, "example.com"),
      Some("_sips._tcp.example.com.")
    )
    assertEquals(ServerLocation.srvName(Ws, "example.com"), None)
  }

  test("default ports: 5061 for TLS, 5060 otherwise") {
    assertEquals(ServerLocation.defaultPort(Tls), port"5061")
    assertEquals(ServerLocation.defaultPort(Udp), port"5060")
    assertEquals(ServerLocation.defaultPort(Tcp), port"5060")
  }

  test("NAPTR service tags map to transports, whatever their case") {
    assertEquals(ServerLocation.transportOf("SIP+D2U"), Some(Udp))
    assertEquals(ServerLocation.transportOf("sip+d2t"), Some(Tcp))
    assertEquals(ServerLocation.transportOf("SIPS+D2T"), Some(Tls))
    assertEquals(ServerLocation.transportOf("SIP+D2S"), None)
    assertEquals(ServerLocation.transportOf("E2U+sip"), None)
  }

  test("NAPTR records are taken by order, then preference") {
    val records = List(
      naptr(20, 1, "SIP+D2U", "_sip._udp.x."),
      naptr(10, 5, "SIP+D2T", "_sip._tcp.x."),
      naptr(10, 2, "SIPS+D2T", "_sips._tcp.x.")
    )
    assertEquals(
      ServerLocation.naptrTransports(records, Set(Udp, Tcp, Tls)).map(_._1),
      List(Tls, Tcp, Udp)
    )
  }

  test(
    "records for transports we do not support, or that are not SRV pointers, are dropped"
  ) {
    val records = List(
      naptr(1, 1, "SIPS+D2T", "_sips._tcp.x."),
      naptr(2, 1, "SIP+D2T", "_sip._tcp.x.", flags = "A"),
      naptr(3, 1, "SIP+D2U", "_sip._udp.x."),
      naptr(4, 1, "SIP+D2S", "_sip._sctp.x.")
    )
    assertEquals(
      ServerLocation.naptrTransports(records, Set(Udp, Tcp)),
      List(Udp -> "_sip._udp.x.")
    )
  }

  test("a transport offered twice is kept once, at its best place") {
    val records = List(
      naptr(1, 1, "SIP+D2T", "_sip._tcp.first."),
      naptr(2, 1, "SIP+D2T", "_sip._tcp.second.")
    )
    assertEquals(
      ServerLocation.naptrTransports(records, Set(Tcp)),
      List(Tcp -> "_sip._tcp.first.")
    )
  }

  test(
    "without NAPTR the preferred transport is probed first, then secure, then the rest"
  ) {
    assertEquals(
      ServerLocation.probeOrder(Udp, Set(Udp, Tcp, Tls)),
      List(Udp, Tls, Tcp)
    )
    assertEquals(
      ServerLocation.probeOrder(Tcp, Set(Udp, Tcp, Tls)),
      List(Tcp, Tls, Udp)
    )
    assertEquals(ServerLocation.probeOrder(Udp, Set(Udp, Tcp)), List(Udp, Tcp))
  }
}
