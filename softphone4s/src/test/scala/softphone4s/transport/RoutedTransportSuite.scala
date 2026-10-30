package softphone4s.transport

import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import com.comcast.ip4s.*
import fs2.Stream
import munit.CatsEffectSuite
import softphone4s.SipLiterals.*
import softphone4s.model.*

class RoutedTransportSuite extends CatsEffectSuite {

  private val to = SocketAddress(ip"127.0.0.1", port"5060")

  private def recording(
      name: String,
      log: Ref[IO, List[String]]
  ): Transport[IO] =
    new Transport[IO] {
      def localAddress: SocketAddress[IpAddress] = to
      def messages: Stream[IO, Inbound]          = Stream.empty
      def send(at: SocketAddress[IpAddress], message: SipMessage): IO[Unit] =
        log.update(_ :+ name)
    }

  private def request(protocol: String) = SipRequest(
    SipMethod.Options,
    uri("sip:bob@example.com"),
    SipHeaders(
      callId = callId("r@host"),
      from = nameAddr("<sip:alice@example.com>;tag=a"),
      to = nameAddr("<sip:bob@example.com>"),
      vias = NonEmptyList.one(
        via(s"SIP/2.0/$protocol 127.0.0.1:5060;branch=z9hG4bK1")
      ),
      cseq = cseq(1, SipMethod.Options)
    )
  )

  test("a request goes out on the transport its top Via names") {
    for {
      log <- Ref.of[IO, List[String]](Nil)
      routed = RoutedTransport(
        recording("udp", log),
        recording("tcp", log),
        recording("tls", log)
      )
      _    <- routed.send(to, request("UDP"))
      _    <- routed.send(to, request("TCP"))
      _    <- routed.send(to, request("udp"))
      _    <- routed.send(to, request("TLS"))
      seen <- log.get
    } yield assertEquals(seen, List("udp", "tcp", "udp", "tls"))
  }

  test("a response goes back on the transport its request came on") {
    for {
      log <- Ref.of[IO, List[String]](Nil)
      routed = RoutedTransport(
        recording("udp", log),
        recording("tcp", log),
        recording("tls", log)
      )
      req = request("TCP")
      _    <- routed.send(to, SipResponse(200, "OK", req.headers))
      seen <- log.get
    } yield assertEquals(seen, List("tcp"))
  }

  test("a transport we do not have is a typed failure") {
    for {
      log <- Ref.of[IO, List[String]](Nil)
      routed = RoutedTransport(
        recording("udp", log),
        recording("tcp", log),
        recording("tls", log)
      )
      result <- routed.send(to, request("SCTP")).attempt
      seen   <- log.get
    } yield {
      assertEquals(result, Left(SoftphoneError.TransportUnavailable("sctp")))
      assertEquals(seen, Nil)
    }
  }
}
