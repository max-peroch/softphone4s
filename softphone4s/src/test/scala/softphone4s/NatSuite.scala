package softphone4s

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.config.{
  NatConfig,
  ObservedAddresses,
  SoftphoneConfig,
  StunConfig
}
import softphone4s.dns.FakeDns
import softphone4s.sdp.SessionDescription
import softphone4s.stun.StunServer
import softphone4s.model.*
import softphone4s.transaction.TransactionMessages
import softphone4s.transport.{Inbound, SipTransport, Transport}

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** A softphone behind a NAT, against a server that tells it so: the server
  * stamps its answers with an address that is not the softphone's own.
  */
class NatSuite extends CatsEffectSuite {

  private val audio   = new AudioFormat(8000f, 16, 1, true, false)
  private val ours    = port"19160"
  private val theirs  = port"19161"
  private val target  = SocketAddress(ip"127.0.0.1", ours)
  private val outside = SocketAddress(ip"203.0.113.7", port"41000")

  private def config(mode: ObservedAddresses, stun: Option[StunConfig]) =
    SoftphoneConfig(
      user = sipUser("test"),
      password = Secret("test"),
      realm = host"127.0.0.1",
      localSipPort = ours,
      serverPort = theirs,
      baseRtpPort = port"19950",
      bindAddress = ip"127.0.0.1",
      hangupTimeout = 1.second,
      nat = NatConfig(mode, stun)
    )

  private def scenario(
      mode: ObservedAddresses,
      stun: Option[StunConfig] = None
  ): Resource[IO, (SoftphoneStreaming[IO], Transport[IO])] =
    (
      SoftphoneStreaming.resource[IO](
        config(mode, stun),
        audio,
        NoOpLogger.impl[IO]
      ),
      SipTransport.udp[IO](
        SocketAddress(ip"127.0.0.1", theirs),
        NoOpLogger.impl[IO]
      )
    ).tupled

  private def requestOf(peer: Transport[IO], method: SipMethod) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest) if r.method == method => r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  /** A 200 as a server that saw the request come from `outside` sends it. */
  private def okSeenAt(
      request: SipRequest,
      seenAt: SocketAddress[IpAddress]
  ): SipResponse = {
    val base =
      TransactionMessages.responseTo(request, 200, "OK", Some(tag("srv")))
    TransactionMessages.seenFrom(
      base.copy(headers =
        base.headers.copy(vias =
          NonEmptyList(
            base.headers.topVia.requestingRport,
            base.headers.vias.tail
          )
        )
      ),
      seenAt
    )
  }

  /** A 200 that says nothing about where the request came from. */
  private def plainOk(request: SipRequest): SipResponse =
    TransactionMessages.responseTo(request, 200, "OK", Some(tag("srv")))

  private val bob = Callee("bob").fold(err => fail(err), identity)

  test(
    "behind a NAT the public Contact is registered, and used by the calls that follow"
  ) {
    scenario(ObservedAddresses.Use).use { (phone, peer) =>
      phone.register.use { registration =>
        for {
          first <- requestOf(peer, SipMethod.Register)
          _     <- peer.send(target, okSeenAt(first, outside))
          // The server has said how we look from outside: register that.
          second <- requestOf(peer, SipMethod.Register).iterateUntil(
            _.headers.contacts.exists(_.uri.host == outside.host)
          )
          _      <- peer.send(target, okSeenAt(second, outside))
          _      <- registration.awaitRegistered
          invite <- phone.call(bob).use { _ =>
            requestOf(peer, SipMethod.Invite)
          }
        } yield {
          assertEquals(
            first.headers.contacts.map(_.uri.render),
            List("sip:test@127.0.0.1:19160")
          )
          assertEquals(
            second.headers.contacts.map(_.uri.render),
            List("sip:test@203.0.113.7:41000")
          )
          assertEquals(
            invite.headers.contacts.map(_.uri.render),
            List("sip:test@203.0.113.7:41000")
          )
          assert(invite.body.contains("c=IN IP4 203.0.113.7"), invite.body)
          // The Via is still our own, with rport asking to be answered where we are.
          assertEquals(invite.headers.topVia.host, ip"127.0.0.1")
          assert(invite.headers.topVia.rport.contains(RPort.Requested))
        }
      }
    }
  }

  test("told to ignore what servers say, it keeps its own address") {
    scenario(ObservedAddresses.Ignore).use { (phone, peer) =>
      phone.register.use { _ =>
        for {
          first  <- requestOf(peer, SipMethod.Register)
          _      <- peer.send(target, okSeenAt(first, outside))
          second <- requestOf(peer, SipMethod.Register)
            .timeout(1500.millis)
            .attempt
          invite <- phone.call(bob).use { _ =>
            requestOf(peer, SipMethod.Invite)
          }
        } yield {
          assert(
            second.isLeft || second.exists(
              _.headers.cseq == first.headers.cseq
            ),
            "no move is made"
          )
          assertEquals(
            invite.headers.contacts.map(_.uri.render),
            List("sip:test@127.0.0.1:19160")
          )
          assert(invite.body.contains("c=IN IP4 127.0.0.1"), invite.body)
        }
      }
    }
  }

  test("a server that says we are where we are changes nothing") {
    scenario(ObservedAddresses.Use).use { (phone, peer) =>
      phone.register.use { _ =>
        for {
          first <- requestOf(peer, SipMethod.Register)
          _     <- peer.send(
            target,
            okSeenAt(first, SocketAddress(ip"127.0.0.1", ours))
          )
          next <- requestOf(peer, SipMethod.Register)
            .timeout(1500.millis)
            .attempt
        } yield assert(
          next.isLeft || next.exists(_.headers.cseq == first.headers.cseq),
          "no move is made"
        )
      }
    }
  }

  private def stunAt(server: StunServer.Running) =
    StunConfig(host"127.0.0.1", server.address.port, refresh = 1.hour)

  test(
    "a STUN server's answer is registered as the Contact, though the registrar says nothing"
  ) {
    StunServer(says = Some(outside)).use(stun =>
      scenario(ObservedAddresses.Use, Some(stunAt(stun))).use { (phone, peer) =>
        phone.register.use { registration =>
          for {
            // The registrar says nothing of where we are; only STUN knows. The
            // first REGISTER may go out before its answer arrives, or after.
            second <- requestOf(peer, SipMethod.Register)
              .flatTap(r =>
                peer
                  .send(target, plainOk(r))
              )
              .iterateUntil(
                _.headers.contacts.exists(_.uri.host == outside.host)
              )
            _ <- registration.awaitRegistered
          } yield assertEquals(
            second.headers.contacts.map(_.uri.render),
            List("sip:test@203.0.113.7:41000")
          )
        }
      }
    )
  }

  private def sdpOf(request: SipRequest): SessionDescription =
    SessionDescription
      .parse(request.body)
      .fold(e => fail(e.toString), identity)

  test(
    "the RTP port is asked about too: the SDP advertises the port the NAT gives it, not ours"
  ) {
    StunServer(says = Some(outside)).use(stun =>
      scenario(ObservedAddresses.Use, Some(stunAt(stun))).use { (phone, peer) =>
        phone.call(bob).use { _ =>
          requestOf(peer, SipMethod.Invite).map { invite =>
            val media = sdpOf(invite).media.head
            // 19950 is the RTP port we bind; the NAT says 41000
            assertEquals(media.port.value, 41000)
            assert(invite.body.contains("c=IN IP4 203.0.113.7"), invite.body)
          }
        }
      }
    )
  }

  test(
    "of the addresses of the STUN server, the one of the family we send from is asked, not the first"
  ) {
    StunServer(says = Some(outside)).use(stun =>
      FakeDns(hosts = Map("stun.example.org" -> List(ip"::1", ip"127.0.0.1")))
        .flatMap(dns =>
          (
            SoftphoneStreaming.resourceWith[IO](
              config(
                ObservedAddresses.Use,
                Some(
                  StunConfig(
                    host"stun.example.org",
                    stun.address.port,
                    refresh = 1.hour
                  )
                )
              ),
              audio,
              NoOpLogger.impl[IO],
              IO.pure(dns)
            ),
            SipTransport.udp[IO](
              SocketAddress(ip"127.0.0.1", theirs),
              NoOpLogger.impl[IO]
            )
          ).tupled.use { (phone, peer) =>
            phone.call(bob).use { _ =>
              requestOf(peer, SipMethod.Invite).map(invite =>
                assertEquals(sdpOf(invite).media.head.port.value, 41000)
              )
            }
          }
        )
    )
  }

  test(
    "a STUN server that says nothing does not hold the call up for long, and the port we have is advertised"
  ) {
    StunServer(ignore = 1000).use(stun =>
      scenario(ObservedAddresses.Use, Some(stunAt(stun))).use { (phone, peer) =>
        phone.call(bob).use { _ =>
          requestOf(peer, SipMethod.Invite).timed.map { (took, invite) =>
            assertEquals(sdpOf(invite).media.head.port.value, 19950)
            // a request and one repeat, 500 ms and then 1 s, and a little more
            assert(took < 4.seconds, took.toString)
          }
        }
      }
    )
  }

  test(
    "with no STUN server the RTP port advertised is the one we bind"
  ) {
    scenario(ObservedAddresses.Use).use { (phone, peer) =>
      phone.call(bob).use { _ =>
        requestOf(peer, SipMethod.Invite).map(invite =>
          assertEquals(sdpOf(invite).media.head.port.value, 19950)
        )
      }
    }
  }

  test(
    "told to ignore what servers say, the RTP port is not asked about either"
  ) {
    StunServer(says = Some(outside)).use(stun =>
      scenario(ObservedAddresses.Ignore, Some(stunAt(stun))).use {
        (phone, peer) =>
          phone.call(bob).use { _ =>
            for {
              invite   <- requestOf(peer, SipMethod.Invite)
              requests <- stun.requests.get
            } yield {
              assertEquals(sdpOf(invite).media.head.port.value, 19950)
              assertEquals(requests, 0)
            }
          }
      }
    )
  }

  test(
    "a STUN server is not asked when the observed address is to be ignored"
  ) {
    StunServer(says = Some(outside)).use { stun =>
      scenario(ObservedAddresses.Ignore, Some(stunAt(stun))).use { (_, _) =>
        IO.sleep(500.millis) >> stun.requests.get.map(assertEquals(_, 0))
      }
    }
  }
}
