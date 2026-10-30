package softphone4s.dns

import cats.effect.{IO, Ref, Resource}
import com.comcast.ip4s.*
import fs2.Chunk
import fs2.io.net.{Datagram, Network}
import munit.CatsEffectSuite
import org.xbill.DNS.{
  DClass,
  Flags,
  Message,
  NAPTRRecord,
  Name,
  Rcode,
  Record,
  SRVRecord as DnsSrv,
  Section,
  Type
}
import softphone4s.model.SoftphoneError

import scala.concurrent.duration.*

/** The dnsjava lookups against a DNS server of our own, so that what is on the
  * wire is under test.
  */
class DnsJavaRecordsSuite extends CatsEffectSuite {

  private def name(s: String): Name = Name.fromString(s, Name.root)

  private val zone: Map[(String, Int), List[Record]] = {
    val ttl = 300L
    def srv(owner: String, prio: Int, weight: Int, port: Int, target: String) =
      new DnsSrv(name(owner), DClass.IN, ttl, prio, weight, port, name(target))
    Map(
      ("_sip._tcp.example.com.", Type.SRV) -> List(
        srv("_sip._tcp.example.com.", 20, 5, 5080, "b.example.com."),
        srv("_sip._tcp.example.com.", 10, 60, 5070, "a.example.com.")
      ),
      ("example.com.", Type.NAPTR) -> List(
        new NAPTRRecord(
          name("example.com."),
          DClass.IN,
          ttl,
          10,
          1,
          "S",
          "SIP+D2T",
          "",
          name("_sip._tcp.example.com.")
        )
      )
    )
  }

  private val knownNames = zone.keySet.map(_._1)

  /** A DNS server on loopback answering from `zone`, counting what it is asked.
    */
  private def server: Resource[IO, (SocketAddress[IpAddress], Ref[IO, Int])] =
    for {
      queries <- Resource.eval(Ref.of[IO, Int](0))
      socket  <- Network[IO].bindDatagramSocket(
        SocketAddress(ip"127.0.0.1", port"0")
      )
      address = socket.address match {
        case a: SocketAddress[?] =>
          a.host match {
            case ip: IpAddress => SocketAddress(ip, a.port)
            case _             => fail("not an IP address")
          }
        case _ => fail("not an IP address")
      }
      _ <- socket.reads
        .evalMap { datagram =>
          queries.update(_ + 1) >> IO {
            val query    = new Message(datagram.bytes.toArray)
            val question = query.getQuestion
            val owner    = question.getName.toString
            val response = new Message(query.getHeader.getID)
            response.getHeader.setFlag(Flags.QR)
            response.getHeader.setFlag(Flags.RD)
            response.getHeader.setFlag(Flags.RA)
            response.addRecord(question, Section.QUESTION)
            zone.get((owner, question.getType)) match {
              case Some(records) =>
                records.foreach(response.addRecord(_, Section.ANSWER))
              // The name exists but has no records of this kind (NODATA) ...
              case None if knownNames.contains(owner) => ()
              // ... or it does not exist at all.
              case None => response.getHeader.setRcode(Rcode.NXDOMAIN)
            }
            Datagram(datagram.remote, Chunk.array(response.toWire))
          }.flatMap(socket.write)
        }
        .compile
        .drain
        .background
    } yield (address, queries)

  private def records(address: SocketAddress[IpAddress]) =
    DnsJavaRecords.against[IO](address, 1.second)

  test("SRV records come back with their priority, weight, port and target") {
    server.use { (address, _) =>
      records(address).flatMap(_.srv("_sip._tcp.example.com.")).map { found =>
        assertEquals(
          found
            .sortBy(_.priority)
            .map(r => (r.priority, r.weight, r.port.value, r.target.toString)),
          List((10, 60, 5070, "a.example.com"), (20, 5, 5080, "b.example.com"))
        )
      }
    }
  }

  test(
    "NAPTR records come back with their order, preference, flags, service and replacement"
  ) {
    server.use { (address, _) =>
      records(address)
        .flatMap(_.naptr("example.com"))
        .map(found =>
          assertEquals(
            found,
            List(NaptrRecord(10, 1, "S", "SIP+D2T", "_sip._tcp.example.com."))
          )
        )
    }
  }

  test("a name that does not exist has no SRV or NAPTR records") {
    server.use { (address, _) =>
      for {
        dns <- records(address)
        srv <- dns.srv("_sip._udp.nowhere.example.")
        n   <- dns.naptr("nowhere.example")
      } yield {
        assertEquals(srv, Nil)
        assertEquals(n, Nil)
      }
    }
  }

  test("host addresses come from the system resolver, which knows localhost") {
    server.use { (address, queries) =>
      for {
        found <- records(address).flatMap(
          _.addresses(Hostname.fromString("localhost").get)
        )
        asked <- queries.get
      } yield {
        assert(found.nonEmpty, "localhost resolves")
        assertEquals(asked, 0)
      }
    }
  }

  test("a host the system cannot resolve has no addresses") {
    server.use { (address, _) =>
      records(address)
        .flatMap(_.addresses(Hostname.fromString("no-such-host.invalid").get))
        .map(assertEquals(_, Nil))
    }
  }

  test("a name with no records of the kind asked for has none") {
    server.use { (address, _) =>
      records(address)
        .flatMap(_.srv("a.example.com."))
        .map(assertEquals(_, Nil))
    }
  }

  test("an IP address needs no lookup") {
    server.use { (address, queries) =>
      for {
        found <- records(address).flatMap(_.addresses(ip"192.0.2.99"))
        asked <- queries.get
      } yield {
        assertEquals(found, List(ip"192.0.2.99"))
        assertEquals(asked, 0)
      }
    }
  }

  test(
    "an answer is kept for its TTL, so a repeated question is not asked again"
  ) {
    server.use { (address, queries) =>
      for {
        dns   <- records(address)
        _     <- dns.srv("_sip._tcp.example.com.")
        _     <- dns.srv("_sip._tcp.example.com.")
        _     <- dns.srv("_sip._tcp.example.com.")
        asked <- queries.get
      } yield assertEquals(asked, 1)
    }
  }

  test("a server that does not answer is a failure, not an empty answer") {
    // Nothing listens here.
    val dead = SocketAddress(ip"127.0.0.1", port"1")
    records(dead).flatMap(_.srv("_sip._tcp.example.com.")).attempt.map {
      case Left(_: SoftphoneError.HostResolutionFailed) => ()
      case other => fail(s"unexpected $other")
    }
  }
}
