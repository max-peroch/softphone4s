package softphone4s

import cats.effect.std.Queue
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.{Network, Socket}
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.codec.SipCodec
import softphone4s.config.SoftphoneConfig
import softphone4s.dns.{FakeDns, NaptrRecord, SrvRecord}
import softphone4s.model.*
import softphone4s.transaction.TransactionMessages
import softphone4s.transport.StreamFraming

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** Calls whose destination comes from DNS: NAPTR and SRV records decide the
  * transport, the address and the port.
  */
class DnsCallSuite extends CatsEffectSuite {

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  /** A TCP SIP server on loopback; it queues what it is sent and answers INVITE
    * and BYE.
    */
  private def server(
      recordRoute: Option[String] = None
  ): Resource[IO, (Port, Queue[IO, SipRequest])] =
    for {
      requests  <- Resource.eval(Queue.unbounded[IO, SipRequest])
      bound     <- Network[IO].bind(SocketAddress(ip"127.0.0.1", port"0"))
      listening <- Resource.eval(bound.address match {
        case a: SocketAddress[?] => IO.pure(a.port)
        case _ => IO.raiseError(new RuntimeException("no port"))
      })
      _ <- bound.accept
        .map { (socket: Socket[IO]) =>
          socket.reads
            .through(StreamFraming.frames[IO]())
            .evalMap {
              case Right(bytes) =>
                SipCodec.decode(bytes) match {
                  case Right(request: SipRequest) =>
                    requests.offer(request) >>
                      Option
                        .when(
                          request.method == SipMethod.Invite || request.method == SipMethod.Bye
                        ) {
                          val base = TransactionMessages.responseTo(
                            request,
                            200,
                            "OK",
                            Some(tag("rtag"))
                          )
                          base.copy(
                            headers = base.headers.copy(
                              contacts = List(
                                nameAddr(
                                  s"<sip:bob@127.0.0.1:$listening;transport=tcp>"
                                )
                              ),
                              recordRoutes =
                                recordRoute.map(nameAddr(_)).toList,
                              contentType = Some(MediaType.Sdp)
                            ),
                            body = "v=0\r\nc=IN IP4 127.0.0.1\r\nt=0 0\r\nm=audio 40000 RTP/AVP 0\r\n"
                          )
                        }
                        .traverse_(r => socket.write(SipCodec.encode(r)))
                  case _ => IO.unit
                }
              case Left(_) => IO.unit
            }
            .handleErrorWith(_ => Stream.empty)
        }
        .parJoinUnbounded
        .compile
        .drain
        .background
    } yield (listening, requests)

  private def phone(dns: FakeDns, ours: Port) =
    SoftphoneStreaming.resourceWith[IO](
      SoftphoneConfig(
        user = "test",
        password = "test",
        realm = "example.org",
        localSipPort = ours,
        baseRtpPort = port"19700",
        bindAddress = ip"127.0.0.1",
        hangupTimeout = 2.seconds
      ),
      audio,
      NoOpLogger.impl[IO],
      IO.pure(dns)
    )

  private val bob = Callee("bob").fold(err => fail(err), identity)

  private def next(requests: Queue[IO, SipRequest], method: SipMethod) =
    Stream
      .fromQueueUnterminated(requests, limit = 1)
      .filter(_.method == method)
      .head
      .compile
      .lastOrError
      .timeout(10.seconds)

  test(
    "NAPTR and SRV records choose TCP and the address and port, though UDP is configured"
  ) {
    server().use { (listening, requests) =>
      for {
        dns <- FakeDns(
          naptrs = Map(
            "example.org" -> List(
              NaptrRecord(10, 1, "S", "SIP+D2T", "_sip._tcp.example.org.")
            )
          ),
          srvs = Map(
            "_sip._tcp.example.org." -> List(
              SrvRecord(
                10,
                1,
                listening,
                Hostname.fromString("sip1.example.org").get
              )
            )
          ),
          hosts = Map("sip1.example.org" -> List(ip"127.0.0.1"))
        )
        invite <- phone(dns, port"19130").use { p =>
          p.call(bob)
            .use(call => call.awaitPickup >> next(requests, SipMethod.Invite))
        }
        asked <- dns.asked.get
      } yield {
        assertEquals(invite.headers.topVia.transport, TransportProtocol.Tcp)
        assertEquals(invite.requestUri.host.toString, "example.org")
        assert(asked.contains("NAPTR example.org"), asked.toString)
        assert(asked.contains("SRV _sip._tcp.example.org."), asked.toString)
      }
    }
  }

  test("a domain that resolves to nothing cannot be called, and says so") {
    for {
      dns    <- FakeDns()
      result <- phone(dns, port"19131").use(_.call(bob).use_.attempt)
    } yield assertEquals(
      result,
      Left(SoftphoneError.HostResolutionFailed("example.org"))
    )
  }

  test("registering follows the same records") {
    server().use { (listening, requests) =>
      for {
        dns <- FakeDns(
          srvs = Map(
            "_sip._tcp.example.org." -> List(
              SrvRecord(
                10,
                1,
                listening,
                Hostname.fromString("sip1.example.org").get
              )
            )
          ),
          hosts = Map("sip1.example.org" -> List(ip"127.0.0.1"))
        )
        register <- phone(dns, port"19132").use { p =>
          // No NAPTR here: each transport's SRV name is tried in turn.
          p.register.use(_ => next(requests, SipMethod.Register))
        }
      } yield assertEquals(
        register.headers.topVia.transport,
        TransportProtocol.Tcp
      )
    }
  }

  private val naptrToTcp = Map(
    "example.org" -> List(
      NaptrRecord(10, 1, "S", "SIP+D2T", "_sip._tcp.example.org.")
    )
  )

  private def host(name: String) =
    Hostname.fromString(name).getOrElse(fail(name))

  test(
    "a Record-Route host is located through SRV records, and the BYE goes there"
  ) {
    server(recordRoute = Some("<sip:proxy.example.org;lr>")).use {
      (listening, requests) =>
        for {
          dns <- FakeDns(
            naptrs = naptrToTcp,
            srvs = Map(
              "_sip._tcp.example.org." -> List(
                SrvRecord(10, 1, listening, host("sip1.example.org"))
              ),
              "_sip._tcp.proxy.example.org." -> List(
                SrvRecord(10, 1, listening, host("edge.example.org"))
              )
            ),
            hosts = Map(
              "sip1.example.org" -> List(ip"127.0.0.1"),
              "edge.example.org" -> List(ip"127.0.0.1")
            )
          )
          bye <- phone(dns, port"19133").use { p =>
            p.call(bob).use(_.awaitPickup) >> next(requests, SipMethod.Bye)
          }
          asked <- dns.asked.get
        } yield {
          assertEquals(
            bye.headers.routes.map(_.render),
            List("<sip:proxy.example.org;lr>")
          )
          assert(
            asked.contains("SRV _sip._tcp.proxy.example.org."),
            asked.toString
          )
        }
    }
  }

  test(
    "a Record-Route host that resolves to nothing falls back to the server we dialled"
  ) {
    server(recordRoute = Some("<sip:unknown.example.org;lr>")).use {
      (listening, requests) =>
        for {
          dns <- FakeDns(
            naptrs = naptrToTcp,
            srvs = Map(
              "_sip._tcp.example.org." -> List(
                SrvRecord(10, 1, listening, host("sip1.example.org"))
              )
            ),
            hosts = Map("sip1.example.org" -> List(ip"127.0.0.1"))
          )
          bye <- phone(dns, port"19134").use { p =>
            p.call(bob).use(_.awaitPickup) >> next(requests, SipMethod.Bye)
          }
        } yield assertEquals(
          bye.headers.routes.map(_.render),
          List("<sip:unknown.example.org;lr>")
        )
    }
  }

  test("when the best SRV target refuses the connection the next one is used") {
    server().use { (listening, requests) =>
      for {
        dns <- FakeDns(
          naptrs = naptrToTcp,
          srvs = Map(
            "_sip._tcp.example.org." -> List(
              // Nothing listens on port 1: the connection is refused at once.
              SrvRecord(10, 1, port"1", host("down.example.org")),
              SrvRecord(20, 1, listening, host("up.example.org"))
            )
          ),
          hosts = Map(
            "down.example.org" -> List(ip"127.0.0.1"),
            "up.example.org"   -> List(ip"127.0.0.1")
          )
        )
        invite <- phone(dns, port"19135").use { p =>
          p.call(bob)
            .use(call => call.awaitPickup >> next(requests, SipMethod.Invite))
        }
      } yield assertEquals(
        invite.headers.topVia.transport,
        TransportProtocol.Tcp
      )
    }
  }

  test("when every SRV target is down the call fails, rather than hanging") {
    server().use { (_, _) =>
      for {
        dns <- FakeDns(
          naptrs = naptrToTcp,
          srvs = Map(
            "_sip._tcp.example.org." -> List(
              SrvRecord(10, 1, port"1", host("down.example.org")),
              SrvRecord(20, 1, port"2", host("down.example.org"))
            )
          ),
          hosts = Map("down.example.org" -> List(ip"127.0.0.1"))
        )
        result <- phone(dns, port"19136").use(p =>
          p.call(bob).use(_.awaitPickup).attempt.timeout(10.seconds)
        )
      } yield assertEquals(result, Left(SoftphoneError.TransportFailed))
    }
  }

  test(
    "a big INVITE finds the server's TCP port through its SRV records, not the UDP one"
  ) {
    server().use { (listening, requests) =>
      for {
        dns <- FakeDns(
          srvs =
            Map(
              // UDP is served on one port (nothing is there), TCP on another.
              "_sip._udp.example.org." -> List(
                SrvRecord(10, 1, port"19999", host("sip1.example.org"))
              ),
              "_sip._tcp.example.org." -> List(
                SrvRecord(10, 1, listening, host("sip1.example.org"))
              )
            ),
          hosts = Map("sip1.example.org" -> List(ip"127.0.0.1"))
        )
        invite <- phone(dns, port"19137").use { p =>
          p.call(bob, Map("X-Context" -> List("x" * 1500)))
            .use(call => call.awaitPickup >> next(requests, SipMethod.Invite))
        }
      } yield assertEquals(
        invite.headers.topVia.transport,
        TransportProtocol.Tcp
      )
    }
  }
}
