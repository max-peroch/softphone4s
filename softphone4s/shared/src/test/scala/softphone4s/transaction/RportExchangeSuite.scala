package softphone4s.transaction

import cats.data.NonEmptyList
import cats.effect.IO
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.model.*
import softphone4s.transport.{InMemoryTransport, Inbound}

import scala.concurrent.duration.*

/** A server transaction tells the sender where its request came from. */
class RportExchangeSuite extends CatsEffectSuite {

  private def options(viaText: String) = SipRequest(
    SipMethod.Options,
    uri("sip:bob@example.com"),
    SipHeaders(
      callId = callId("r@h"),
      from = nameAddr("<sip:alice@example.com>;tag=a"),
      to = nameAddr("<sip:bob@example.com>"),
      vias = NonEmptyList.one(via(viaText)),
      cseq = cseq(1, SipMethod.Options)
    )
  )

  /** The 200 that comes back to `request` sent from the far end of the pair. */
  private def answerTo(request: SipRequest): IO[SipResponse] =
    InMemoryTransport.pair[IO].flatMap { (server, client) =>
      TransactionManager
        .resource(server, TransactionConfig(), NoOpLogger.impl[IO])
        .use { layer =>
          for {
            _  <- client.send(server.localAddress, request)
            tx <- layer.incoming.head.compile.lastOrError.timeout(5.seconds)
            _  <- tx.respond(
              TransactionMessages.responseTo(tx.request, 200, "OK")
            )
            reply <- client.messages
              .collectFirst { case Inbound(_, r: SipResponse) => r }
              .compile
              .lastOrError
              .timeout(5.seconds)
          } yield reply
        }
    }

  test(
    "a request that asked for rport is answered with rport and received filled in"
  ) {
    answerTo(options("SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKa;rport")).map {
      reply =>
        val top = reply.headers.topVia
        assertEquals(
          top.rport,
          Some(RPort.Assigned(InMemoryTransport.AddressB.port))
        )
        assertEquals(top.received, Some(InMemoryTransport.AddressB.host))
    }
  }

  test(
    "a request from behind a NAT is answered with received, though it asked for no rport"
  ) {
    answerTo(options("SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKb")).map {
      reply =>
        assertEquals(
          reply.headers.topVia.received,
          Some(InMemoryTransport.AddressB.host)
        )
        assertEquals(reply.headers.topVia.rport, None)
    }
  }

  test("a request whose sent-by is where it came from gets no received") {
    val sentBy = InMemoryTransport.AddressB
    answerTo(
      options(s"SIP/2.0/UDP ${sentBy.host}:${sentBy.port};branch=z9hG4bKc")
    ).map { reply =>
      assertEquals(reply.headers.topVia.received, None)
    }
  }

  // ---- Learning how we look from outside --------------------------------------------

  /** Sends `request` to `sendTo` from a layer that records what it observes,
    * has the far end answer it, and gives what was observed.
    */
  private def observedAfter(
      request: SipRequest,
      sendTo: SocketAddress[IpAddress]
  ): IO[List[ObservedAddress]] =
    for {
      pair <- InMemoryTransport.pair[IO]
      (ours, theirs) = pair
      seen     <- cats.effect.Ref.of[IO, List[ObservedAddress]](Nil)
      observed <- TransactionManager
        .resourceObserving(
          ours,
          TransactionConfig(),
          NoOpLogger.impl[IO],
          o => seen.update(_ :+ o),
          _ => IO.unit,
          TransactionTracing.noop[IO]
        )
        .use { layer =>
          for {
            tx <- layer
              .client(request, sendTo)
              .flatMap(
                _.fold(
                  e => IO.raiseError(new RuntimeException(e.message)),
                  IO.pure
                )
              )
            received <- theirs.messages
              .collectFirst { case Inbound(_, r: SipRequest) => r }
              .compile
              .lastOrError
              .timeout(5.seconds)
            // The far end answers as a server does: stamping where it saw us.
            _ <- theirs.send(
              ours.localAddress,
              TransactionMessages.seenFrom(
                TransactionMessages.responseTo(received, 200, "OK"),
                ours.localAddress
              )
            )
            _   <- tx.notices.head.compile.lastOrError.timeout(5.seconds)
            _   <- IO.sleep(100.millis)
            now <- seen.get
          } yield now
        }
    } yield observed

  test(
    "a response that says where it saw us from tells the layer, for a request sent over UDP"
  ) {
    observedAfter(
      options("SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKo1;rport"),
      InMemoryTransport.AddressB
    ).map(found =>
      assertEquals(
        found,
        List(ObservedAddress(TransportProtocol.Udp, InMemoryTransport.AddressA))
      )
    )
  }

  test(
    "a response from somewhere other than where the request went is not believed"
  ) {
    observedAfter(
      options("SIP/2.0/UDP 10.0.0.2:5060;branch=z9hG4bKo2;rport"),
      SocketAddress(ip"192.0.2.99", port"5060")
    ).map(found => assertEquals(found, Nil))
  }

  test(
    "nothing is learned from a stream transport, whose port is only a connection's"
  ) {
    observedAfter(
      options("SIP/2.0/TCP 10.0.0.2:5060;branch=z9hG4bKo3"),
      InMemoryTransport.AddressB
    ).map(found => assertEquals(found, Nil))
  }
}
