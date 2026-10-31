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
      user = "test",
      password = "test",
      realm = "127.0.0.1",
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
    StunConfig("127.0.0.1", server.address.port, refresh = 1.hour)

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
