package softphone4s.ua

import cats.effect.std.Random
import cats.effect.{IO, Resource}
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.DigestVerifier
import softphone4s.SipLiterals.*
import softphone4s.config.SoftphoneConfig
import softphone4s.messaging.{MessageContent, MessageFailure}
import softphone4s.model.*
import softphone4s.runtime.Entropy
import softphone4s.transaction.{
  TransactionConfig,
  TransactionLayer,
  TransactionManager,
  TransactionMessages
}
import softphone4s.transport.{InMemoryTransport, Inbound, Transport}

import scala.concurrent.duration.*

/** A message sent against a real transaction layer, the test being the other
  * party: it reads what is sent and answers through the peer's transport.
  */
class MessageSenderSuite extends CatsEffectSuite {

  private val us      = InMemoryTransport.AddressA
  private val them    = InMemoryTransport.AddressB
  private val bob     = uri("sip:bob@test.local")
  private val content = MessageContent.plain("hello bob")

  private val config =
    SoftphoneConfig(user = "test", password = "test", realm = "test.local")

  private val endpoint =
    LocalEndpoint
      .from(config, ip"127.0.0.1", port"19060", port"19070")
      .fold(err => fail(err.getMessage), identity)

  private val uplink = Uplink(endpoint, them)

  private final case class Rig(
      sender: MessageSender[IO],
      peer: Transport[IO]
  )

  /** A sender, and the peer's end of the wire. Timers are short, so that
    * silence gives up in a moment.
    */
  private def rig: Resource[IO, Rig] =
    for {
      pair  <- Resource.eval(InMemoryTransport.pair[IO])
      layer <- TransactionManager.resource(
        pair._1,
        TransactionConfig(5.millis, 20.millis, 20.millis),
        NoOpLogger.impl[IO]
      )
      random <- Resource.eval(Random.scalaUtilRandom[IO])
    } yield Rig(
      new MessageSender[IO](
        layer: TransactionLayer[IO],
        Entropy.fromRandom(random)
      ),
      pair._2
    )

  private def nextRequest(peer: Transport[IO]) =
    requestWhere(peer)(_ => true)

  /** The next request that fits, passing over retransmissions of the others,
    * which a client sends until it is answered.
    */
  private def requestWhere(peer: Transport[IO])(wanted: SipRequest => Boolean) =
    peer.messages
      .collectFirst { case Inbound(_, r: SipRequest) if wanted(r) => r }
      .compile
      .lastOrError
      .timeout(5.seconds)

  /** The request that follows `first` in the same transaction user: the retry.
    */
  private def retryOf(peer: Transport[IO], first: SipRequest) =
    requestWhere(peer)(
      _.headers.cseq.seq.value == first.headers.cseq.seq.value + 1
    )

  private def answer(
      peer: Transport[IO],
      request: SipRequest,
      code: Int,
      reason: String
  ) =
    peer.send(us, TransactionMessages.responseTo(request, code, reason))

  private def challenged(request: SipRequest): SipResponse = {
    val base = TransactionMessages.responseTo(request, 401, "Unauthorized")
    base.copy(headers =
      base.headers.copy(wwwAuthenticate =
        List(
          challenge(
            """Digest realm="test.local", nonce="n0nce", algorithm=MD5, qop="auth""""
          )
        )
      )
    )
  }

  private def deliver(
      rig: Rig,
      candidates: List[IO[Uplink]] = List(IO.pure(uplink))
  ) =
    rig.sender.deliver(candidates, bob, content)

  test(
    "a message that is answered 200 is delivered, as a request outside any dialog"
  ) {
    rig.use { r =>
      for {
        sending <- deliver(r).start
        request <- nextRequest(r.peer)
        _       <- answer(r.peer, request, 200, "OK")
        result  <- sending.joinWithNever
      } yield {
        assertEquals(result, Right(()))
        assertEquals(request.method, SipMethod.Message)
        assertEquals(request.requestUri.render, "sip:bob@test.local")
        assertEquals(request.body, "hello bob")
        assertEquals(request.headers.contentType, Some(MediaType.Plain))
        assertEquals(request.headers.cseq, cseq(1, SipMethod.Message))
        assertEquals(request.headers.to.tag, None)
      }
    }
  }

  test(
    "a challenge is answered once, with the same Call-ID and the next CSeq, and the answer is delivery"
  ) {
    rig.use { r =>
      for {
        sending <- deliver(r).start
        first   <- nextRequest(r.peer)
        _       <- r.peer.send(us, challenged(first))
        retry   <- retryOf(r.peer, first)
        _       <- answer(r.peer, retry, 200, "OK")
        result  <- sending.joinWithNever
      } yield {
        assertEquals(result, Right(()))
        assertEquals(retry.headers.callId, first.headers.callId)
        assertEquals(retry.headers.cseq, cseq(2, SipMethod.Message))
        assertEquals(retry.headers.from.tag, first.headers.from.tag)
        assertEquals(retry.body, first.body)
        assertEquals(
          DigestVerifier.verify(retry, "test").map(_.valid),
          Some(true)
        )
        assertEquals(DigestVerifier.authorization(first), None)
      }
    }
  }

  test("a second challenge is not answered: the message is not authorised") {
    rig.use { r =>
      for {
        sending <- deliver(r).start
        first   <- nextRequest(r.peer)
        _       <- r.peer.send(us, challenged(first))
        retry   <- retryOf(r.peer, first)
        _       <- r.peer.send(us, challenged(retry))
        result  <- sending.joinWithNever
        third   <- retryOf(r.peer, retry).timeout(300.millis).attempt
      } yield {
        assertEquals(result, Left(MessageFailure.Unauthorized))
        assert(third.isLeft, "a third request was sent")
      }
    }
  }

  test(
    "a challenge with nothing we can answer is not authorised, with no retry"
  ) {
    rig.use { r =>
      for {
        sending <- deliver(r).start
        first   <- nextRequest(r.peer)
        bare = TransactionMessages.responseTo(first, 401, "Unauthorized")
        _      <- r.peer.send(us, bare)
        result <- sending.joinWithNever
      } yield assertEquals(result, Left(MessageFailure.Unauthorized))
    }
  }

  test("a refusal is final, with its status") {
    rig.use { r =>
      for {
        sending <- deliver(r, List(IO.pure(uplink), IO.pure(uplink))).start
        request <- nextRequest(r.peer)
        _       <- answer(r.peer, request, 403, "Forbidden")
        result  <- sending.joinWithNever
        another <- requestWhere(r.peer)(
          _.headers.callId != request.headers.callId
        ).timeout(300.millis).attempt
      } yield {
        assertEquals(result, Left(MessageFailure.Rejected(403, "Forbidden")))
        // The other way to reach them would be refused the same, so it is not tried.
        assert(another.isLeft, "another way was tried after a refusal")
      }
    }
  }

  test("a 415 says what the other side takes") {
    rig.use { r =>
      for {
        sending <- deliver(r).start
        request <- nextRequest(r.peer)
        refusal = TransactionMessages.responseTo(
          request,
          415,
          "Unsupported Media Type"
        )
        _ <- r.peer.send(
          us,
          refusal.copy(headers =
            refusal.headers.copy(others =
              List(ExtensionHeader(FieldName.Accept, "application/sdp"))
            )
          )
        )
        result <- sending.joinWithNever
      } yield assertEquals(
        result,
        Left(MessageFailure.UnsupportedContent(List(MediaType.Sdp)))
      )
    }
  }

  test("no answer in time is a timeout") {
    rig.use { r =>
      deliver(r).map(result =>
        assertEquals(result, Left(MessageFailure.TimedOut))
      )
    }
  }

  test(
    "when one way gets no answer the next is tried, with a message of its own"
  ) {
    rig.use { r =>
      for {
        sending <- deliver(r, List(IO.pure(uplink), IO.pure(uplink))).start
        first   <- nextRequest(r.peer) // left unanswered
        second <- requestWhere(r.peer)(_.headers.callId != first.headers.callId)
        _      <- answer(r.peer, second, 200, "OK")
        result <- sending.joinWithNever
      } yield {
        assertEquals(result, Right(()))
        assertNotEquals(second.headers.callId, first.headers.callId)
      }
    }
  }

  test("the ways are made only when their turn comes") {
    rig.use { r =>
      for {
        made <- cats.effect.Ref.of[IO, Int](0)
        second = made.update(_ + 1).as(uplink)
        sending <- deliver(r, List(IO.pure(uplink), second)).start
        request <- nextRequest(r.peer)
        _       <- answer(r.peer, request, 200, "OK")
        result  <- sending.joinWithNever
        count   <- made.get
      } yield {
        assertEquals(result, Right(()))
        assertEquals(count, 0)
      }
    }
  }

  test("with no way to reach them the message is not sent") {
    rig.use { r =>
      deliver(r, Nil).map(result =>
        assertEquals(result, Left(MessageFailure.TransportFailed))
      )
    }
  }

  test("every message has a Call-ID of its own") {
    rig.use { r =>
      for {
        one    <- deliver(r).start
        first  <- nextRequest(r.peer)
        _      <- answer(r.peer, first, 200, "OK")
        _      <- one.joinWithNever
        two    <- deliver(r).start
        second <- requestWhere(r.peer)(_.headers.callId != first.headers.callId)
        _      <- answer(r.peer, second, 200, "OK")
        _      <- two.joinWithNever
      } yield assertNotEquals(first.headers.callId, second.headers.callId)
    }
  }
}
