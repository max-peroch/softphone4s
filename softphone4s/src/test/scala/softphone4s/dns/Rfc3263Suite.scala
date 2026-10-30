package softphone4s.dns

import com.comcast.ip4s.*
import munit.FunSuite
import softphone4s.model.TransportProtocol

class Rfc3263Suite extends FunSuite {

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
      Rfc3263.srvName(Udp, "example.com"),
      Some("_sip._udp.example.com.")
    )
    assertEquals(
      Rfc3263.srvName(Tcp, "example.com."),
      Some("_sip._tcp.example.com.")
    )
    assertEquals(
      Rfc3263.srvName(Tls, "example.com"),
      Some("_sips._tcp.example.com.")
    )
    assertEquals(Rfc3263.srvName(Ws, "example.com"), None)
  }

  test("default ports: 5061 for TLS, 5060 otherwise") {
    assertEquals(Rfc3263.defaultPort(Tls), port"5061")
    assertEquals(Rfc3263.defaultPort(Udp), port"5060")
    assertEquals(Rfc3263.defaultPort(Tcp), port"5060")
  }

  test("NAPTR service tags map to transports, whatever their case") {
    assertEquals(Rfc3263.transportOf("SIP+D2U"), Some(Udp))
    assertEquals(Rfc3263.transportOf("sip+d2t"), Some(Tcp))
    assertEquals(Rfc3263.transportOf("SIPS+D2T"), Some(Tls))
    assertEquals(Rfc3263.transportOf("SIP+D2S"), None)
    assertEquals(Rfc3263.transportOf("E2U+sip"), None)
  }

  test("NAPTR records are taken by order, then preference") {
    val records = List(
      naptr(20, 1, "SIP+D2U", "_sip._udp.x."),
      naptr(10, 5, "SIP+D2T", "_sip._tcp.x."),
      naptr(10, 2, "SIPS+D2T", "_sips._tcp.x.")
    )
    assertEquals(
      Rfc3263.naptrTransports(records, Set(Udp, Tcp, Tls)).map(_._1),
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
      Rfc3263.naptrTransports(records, Set(Udp, Tcp)),
      List(Udp -> "_sip._udp.x.")
    )
  }

  test("a transport offered twice is kept once, at its best place") {
    val records = List(
      naptr(1, 1, "SIP+D2T", "_sip._tcp.first."),
      naptr(2, 1, "SIP+D2T", "_sip._tcp.second.")
    )
    assertEquals(
      Rfc3263.naptrTransports(records, Set(Tcp)),
      List(Tcp -> "_sip._tcp.first.")
    )
  }

  test(
    "without NAPTR the preferred transport is probed first, then secure, then the rest"
  ) {
    assertEquals(
      Rfc3263.probeOrder(Udp, Set(Udp, Tcp, Tls)),
      List(Udp, Tls, Tcp)
    )
    assertEquals(
      Rfc3263.probeOrder(Tcp, Set(Udp, Tcp, Tls)),
      List(Tcp, Tls, Udp)
    )
    assertEquals(Rfc3263.probeOrder(Udp, Set(Udp, Tcp)), List(Udp, Tcp))
  }
}
