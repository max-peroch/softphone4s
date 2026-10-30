package softphone4s.dns

import cats.effect.IO
import cats.effect.std.Random
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import softphone4s.model.TransportProtocol
import softphone4s.runtime.Entropy

import TransportProtocol.*

class TargetResolverSuite extends CatsEffectSuite {

  private val all = Set(Udp, Tcp, Tls)

  private def host(s: String): Hostname =
    Hostname.fromString(s).getOrElse(fail(s))

  private def srv(priority: Int, weight: Int, port: Port, target: String) =
    SrvRecord(priority, weight, port, host(target))

  private def at(transport: TransportProtocol, ip: IpAddress, p: Port) =
    ResolvedTarget(transport, SocketAddress(ip, p))

  private def resolver(dns: FakeDns, supported: Set[TransportProtocol]) =
    Random.scalaUtilRandomSeedInt[IO](7).map { random =>
      new TargetResolver[IO](dns, Entropy.fromRandom(random), supported)
    }

  private def resolve(
      dns: FakeDns,
      uri: String,
      preferred: TransportProtocol = Udp,
      supported: Set[TransportProtocol] = all
  ) = resolver(dns, supported).flatMap(
    _.resolve(softphone4s.SipLiterals.uri(uri), preferred)
  )

  // ---- Addresses and explicit ports -----------------------------------------------

  test(
    "an IP address is the target as it is, with the transport's default port"
  ) {
    for {
      dns   <- FakeDns()
      udp   <- resolve(dns, "sip:bob@192.0.2.1")
      tls   <- resolve(dns, "sips:bob@192.0.2.1")
      asked <- dns.asked.get
    } yield {
      assertEquals(udp, List(at(Udp, ip"192.0.2.1", port"5060")))
      assertEquals(tls, List(at(Tls, ip"192.0.2.1", port"5061")))
      assertEquals(asked, Nil)
    }
  }

  test("an IP address with a port and a transport parameter keeps both") {
    FakeDns()
      .flatMap(resolve(_, "sip:bob@192.0.2.1:5555;transport=tcp"))
      .map(r => assertEquals(r, List(at(Tcp, ip"192.0.2.1", port"5555"))))
  }

  test(
    "a name with a port is looked up for its addresses only, with no SRV or NAPTR"
  ) {
    for {
      dns <- FakeDns(hosts =
        Map("sip.example.com" -> List(ip"192.0.2.7", ip"2001:db8::7"))
      )
      r     <- resolve(dns, "sip:bob@sip.example.com:5070", preferred = Tcp)
      asked <- dns.asked.get
    } yield {
      assertEquals(
        r,
        List(
          at(Tcp, ip"192.0.2.7", port"5070"),
          at(Tcp, ip"2001:db8::7", port"5070")
        )
      )
      assertEquals(asked, List("A sip.example.com"))
    }
  }

  // ---- A transport is named ------------------------------------------------------------

  test("a transport parameter goes straight to that transport's SRV records") {
    for {
      dns <- FakeDns(
        srvs = Map(
          "_sip._tcp.example.com." -> List(
            srv(20, 1, port"5080", "b.example.com"),
            srv(10, 1, port"5070", "a.example.com")
          )
        ),
        hosts = Map(
          "a.example.com" -> List(ip"192.0.2.1"),
          "b.example.com" -> List(ip"192.0.2.2")
        )
      )
      r     <- resolve(dns, "sip:bob@example.com;transport=tcp")
      asked <- dns.asked.get
    } yield {
      assertEquals(
        r,
        List(
          at(Tcp, ip"192.0.2.1", port"5070"),
          at(Tcp, ip"192.0.2.2", port"5080")
        )
      )
      assert(!asked.exists(_.startsWith("NAPTR")), asked.toString)
      assertEquals(asked.head, "SRV _sip._tcp.example.com.")
    }
  }

  test("a sips: URI looks for _sips._tcp records and means TLS") {
    for {
      dns <- FakeDns(
        srvs = Map(
          "_sips._tcp.example.com." -> List(
            srv(1, 1, port"5061", "t.example.com")
          )
        ),
        hosts = Map("t.example.com" -> List(ip"192.0.2.9"))
      )
      r <- resolve(dns, "sips:bob@example.com")
    } yield assertEquals(r, List(at(Tls, ip"192.0.2.9", port"5061")))
  }

  test(
    "with no SRV records the name's own addresses are used, at the default port"
  ) {
    for {
      dns <- FakeDns(hosts = Map("example.com" -> List(ip"192.0.2.3")))
      r   <- resolve(dns, "sip:bob@example.com;transport=tcp")
    } yield assertEquals(r, List(at(Tcp, ip"192.0.2.3", port"5060")))
  }

  test("an SRV target's addresses all become targets, keeping the SRV port") {
    for {
      dns <- FakeDns(
        srvs = Map(
          "_sip._udp.example.com." -> List(
            srv(1, 1, port"5099", "multi.example.com")
          )
        ),
        hosts = Map(
          "multi.example.com" -> List(
            ip"192.0.2.1",
            ip"192.0.2.2",
            ip"2001:db8::1"
          )
        )
      )
      r <- resolve(dns, "sip:bob@example.com;transport=udp")
    } yield {
      assertEquals(r.size, 3)
      assertEquals(r.map(_.address.port).distinct, List(port"5099"))
    }
  }

  // ---- No transport is named -------------------------------------------------------------

  test("NAPTR records choose the transports and their order") {
    for {
      dns <- FakeDns(
        naptrs = Map(
          "example.com" -> List(
            NaptrRecord(20, 1, "S", "SIP+D2U", "_sip._udp.example.com."),
            NaptrRecord(10, 1, "S", "SIPS+D2T", "_sips._tcp.example.com.")
          )
        ),
        srvs = Map(
          "_sips._tcp.example.com." -> List(
            srv(1, 1, port"5061", "secure.example.com")
          ),
          "_sip._udp.example.com." -> List(
            srv(1, 1, port"5060", "plain.example.com")
          )
        ),
        hosts = Map(
          "secure.example.com" -> List(ip"192.0.2.1"),
          "plain.example.com"  -> List(ip"192.0.2.2")
        )
      )
      r <- resolve(dns, "sip:bob@example.com")
    } yield assertEquals(
      r,
      List(
        at(Tls, ip"192.0.2.1", port"5061"),
        at(Udp, ip"192.0.2.2", port"5060")
      )
    )
  }

  test("NAPTR records for a transport we do not support are ignored") {
    for {
      dns <- FakeDns(
        naptrs = Map(
          "example.com" -> List(
            NaptrRecord(1, 1, "S", "SIPS+D2T", "_sips._tcp.example.com."),
            NaptrRecord(2, 1, "S", "SIP+D2U", "_sip._udp.example.com.")
          )
        ),
        srvs = Map(
          "_sips._tcp.example.com." -> List(
            srv(1, 1, port"5061", "secure.example.com")
          ),
          "_sip._udp.example.com." -> List(
            srv(1, 1, port"5060", "plain.example.com")
          )
        ),
        hosts = Map(
          "secure.example.com" -> List(ip"192.0.2.1"),
          "plain.example.com"  -> List(ip"192.0.2.2")
        )
      )
      r <- resolve(dns, "sip:bob@example.com", supported = Set(Udp))
    } yield assertEquals(r, List(at(Udp, ip"192.0.2.2", port"5060")))
  }

  test(
    "without NAPTR each supported transport's SRV name is tried, the preferred first"
  ) {
    for {
      dns <- FakeDns(
        srvs = Map(
          "_sip._tcp.example.com." -> List(
            srv(1, 1, port"5060", "t.example.com")
          ),
          "_sip._udp.example.com." -> List(
            srv(1, 1, port"5060", "u.example.com")
          )
        ),
        hosts = Map(
          "t.example.com" -> List(ip"192.0.2.1"),
          "u.example.com" -> List(ip"192.0.2.2")
        )
      )
      r     <- resolve(dns, "sip:bob@example.com", preferred = Tcp)
      asked <- dns.asked.get
    } yield {
      assertEquals(r.map(_.transport), List(Tcp, Udp))
      assertEquals(
        asked.take(3),
        List(
          "NAPTR example.com",
          "SRV _sip._tcp.example.com.",
          "A t.example.com"
        )
      )
    }
  }

  test(
    "with nothing but an address record the preferred transport is used at its default port"
  ) {
    for {
      dns <- FakeDns(hosts = Map("example.com" -> List(ip"192.0.2.5")))
      r   <- resolve(dns, "sip:bob@example.com", preferred = Tcp)
    } yield assertEquals(r, List(at(Tcp, ip"192.0.2.5", port"5060")))
  }

  test(
    "when NAPTR points to SRV records that are empty the name's addresses still serve"
  ) {
    for {
      dns <- FakeDns(
        naptrs = Map(
          "example.com" -> List(
            NaptrRecord(1, 1, "S", "SIP+D2U", "_sip._udp.example.com.")
          )
        ),
        hosts = Map("example.com" -> List(ip"192.0.2.5"))
      )
      r <- resolve(dns, "sip:bob@example.com")
    } yield assertEquals(r, List(at(Udp, ip"192.0.2.5", port"5060")))
  }

  test("a name that resolves to nothing gives no targets") {
    FakeDns()
      .flatMap(resolve(_, "sip:bob@nowhere.example"))
      .map(assertEquals(_, Nil))
  }
}
