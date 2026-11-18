package softphone4s.transport

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Chunk
import fs2.io.net.{Datagram, Network}
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.codec.SipCodec
import softphone4s.model.*

import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

class TransportSuite extends CatsEffectSuite {

  private val options = SipRequest(
    SipMethod.Options,
    uri("sip:bob@example.com"),
    SipHeaders(
      callId = callId("t1@host"),
      from = nameAddr("<sip:alice@example.com>;tag=a"),
      to = nameAddr("<sip:bob@example.com>"),
      vias =
        NonEmptyList.one(via("SIP/2.0/UDP 127.0.0.1:5060;branch=z9hG4bK1")),
      cseq = cseq(1, SipMethod.Options)
    ),
    body = "hello"
  )

  private val loopback = SocketAddress(ip"127.0.0.1", port"0")

  private def udp = SipTransport.udp[IO](loopback, NoOpLogger.impl[IO])

  test("UDP binds an OS-chosen port and reports it as the local address") {
    udp.use(t => IO(assert(t.localAddress.port.value > 0)))
  }

  test("a message sent over UDP arrives intact, with its source address") {
    (udp, udp).tupled.use { (a, b) =>
      a.send(b.localAddress, options) >>
        b.messages.head.compile.lastOrError.map { inbound =>
          assertEquals(inbound.message, options)
          assertEquals(inbound.from, a.localAddress)
        }
    }
  }

  test("garbage and keep-alive datagrams are skipped, not fatal") {
    udp.use { b =>
      Network[IO].bindDatagramSocket(loopback).use { raw =>
        val send = (s: String) =>
          raw.write(Datagram(b.localAddress, Chunk.array(s.getBytes(UTF_8))))
        send("\r\n\r\n") >> send("this is not SIP") >>
          raw.write(Datagram(b.localAddress, SipCodec.encode(options))) >>
          b.messages.head.compile.lastOrError
            .timeout(5.seconds)
            .map(inbound => assertEquals(inbound.message, options))
      }
    }
  }

  test("the in-memory pair delivers both ways with the sender's address") {
    InMemoryTransport.pair[IO].flatMap { (a, b) =>
      for {
        _   <- a.send(b.localAddress, options)
        toB <- b.messages.head.compile.lastOrError
        _   <- b.send(a.localAddress, options)
        toA <- a.messages.head.compile.lastOrError
      } yield {
        assertEquals(toB, Inbound(a.localAddress, options))
        assertEquals(toA, Inbound(b.localAddress, options))
      }
    }
  }
}
