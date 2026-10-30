package softphone4s.ua

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.{Queue, Random, Supervisor}
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.Network
import munit.CatsEffectSuite
import softphone4s.SipLiterals.*
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*
import softphone4s.fsm.CallTimer
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.model.SoftphoneError

import scala.concurrent.duration.*

class OutboundCallSuite extends CatsEffectSuite {

  private val localIp    = ip"127.0.0.1"
  private val sipPort    = port"19060"
  private val rtpPort    = port"19000"
  private val testConfig =
    SoftphoneConfig(user = "test", password = "test", realm = "test.local")
  private val endpoint = LocalEndpoint
    .from(testConfig, localIp, sipPort, rtpPort)
    .fold(err => fail(err.getMessage), identity)

  private def mkHeaders(cseq: CSeq, to: String = "<sip:bob@test.local>") =
    SipHeaders(
      callId = callId("test@call"),
      from = nameAddr("<sip:test@test.local>;tag=ltag"),
      to = nameAddr(to),
      vias =
        NonEmptyList.one(via("SIP/2.0/UDP 127.0.0.1:19060;branch=z9hG4bKtest")),
      cseq = cseq,
      contacts = List(nameAddr("<sip:bob@test.local:5060>"))
    )

  private val ok200 = SipResponse(
    200,
    "OK",
    mkHeaders(cseq(1, SipMethod.Invite), to = "<sip:bob@test.local>;tag=rtag")
  )

  private val byeRequest = SipRequest(
    SipMethod.Bye,
    uri("sip:test@test.local"),
    mkHeaders(cseq(1, SipMethod.Bye))
  )

  private val activeCall
      : Resource[IO, (OutboundCall[IO], Queue[IO, SipMessage])] =
    for {
      queue <- Resource.eval(Queue.unbounded[IO, SipMessage])
      sipIn = Stream.fromQueueUnterminated(queue)
      supervisor <- Supervisor[IO]
      random     <- Resource.eval(Random.scalaUtilRandom[IO])
      timers     <- Timers.resource[IO, CallTimer]
      media <- Resource.eval(MediaSession.create[IO](160, localIp, rtpPort))
      call  <- Resource.eval(
        OutboundCall.create(
          callId("test@call"),
          uri("sip:bob@test.local"),
          _ => IO.unit,
          endpoint,
          supervisor,
          Entropy.fromRandom(random),
          media,
          timers,
          32.seconds,
          NoOpLogger.impl[IO]
        )
      )
      _ <- Resource.make(call.runEventLoop(sipIn).start)(_.cancel)
      _ <- Resource.eval(queue.offer(ok200) >> call.awaitPickup)
    } yield (call, queue)

  test("awaitHangup completes after remote BYE") {
    activeCall.use { (call, queue) =>
      queue.offer(byeRequest) >> call.awaitHangup
    }
  }

  test("source raises RemoteHangup after remote BYE") {
    activeCall.use { (call, queue) =>
      for {
        _      <- queue.offer(byeRequest)
        _      <- call.awaitHangup
        result <- call.source.compile.drain.attempt
      } yield assert(
        result.left.exists(_ == SoftphoneError.RemoteHangup),
        s"expected RemoteHangup, got $result"
      )
    }
  }

  test("dtmfEvents raises RemoteHangup after remote BYE") {
    activeCall.use { (call, queue) =>
      for {
        _      <- queue.offer(byeRequest)
        _      <- call.awaitHangup
        result <- call.dtmfEvents.compile.drain.attempt
      } yield assert(
        result.left.exists(_ == SoftphoneError.RemoteHangup),
        s"expected RemoteHangup, got $result"
      )
    }
  }

  test("awaitHangup does not complete on local hangup") {
    activeCall.use { (call, _) =>
      call.hangup >>
        call.awaitHangup.timeout(200.millis).attempt.map { result =>
          assert(
            result.isLeft,
            s"awaitHangup should not complete on local hangup, got $result"
          )
        }
    }
  }

  test("an unanswered call times out through the invite timer") {
    val unanswered: Resource[IO, OutboundCall[IO]] =
      for {
        supervisor <- Supervisor[IO]
        random     <- Resource.eval(Random.scalaUtilRandom[IO])
        timers     <- Timers.resource[IO, CallTimer]
        media      <- Resource.eval(
          MediaSession.create[IO](160, localIp, port"19002")
        )
        call <- Resource.eval(
          OutboundCall.create(
            callId("timeout@call"),
            uri("sip:bob@test.local"),
            _ => IO.unit,
            endpoint,
            supervisor,
            Entropy.fromRandom(random),
            media,
            timers,
            150.millis,
            NoOpLogger.impl[IO]
          )
        )
        _ <- Resource.make(call.runEventLoop(Stream.never[IO]).start)(_.cancel)
      } yield call
    unanswered.use(_.awaitPickup.attempt).map { result =>
      assertEquals(result, Left(SoftphoneError.CallTimedOut))
    }
  }

  test("wrong credentials: one retry, then the call is rejected, not looped") {
    val unauthorized = SipResponse(
      401,
      "Unauthorized",
      mkHeaders(cseq(1, SipMethod.Invite)).copy(
        wwwAuthenticate = List(
          AuthChallenge.parse("""Digest realm="test.local", nonce="n1"""")
        )
      )
    )
    val scenario = for {
      supervisor <- Supervisor[IO]
      random     <- Resource.eval(Random.scalaUtilRandom[IO])
      timers     <- Timers.resource[IO, CallTimer]
      media <- Resource.eval(MediaSession.create[IO](160, localIp, port"19003"))
      sent  <- Resource.eval(cats.effect.Ref.of[IO, List[SipMessage]](Nil))
      queue <- Resource.eval(Queue.unbounded[IO, SipMessage])
      call  <- Resource.eval(
        OutboundCall.create(
          callId("auth@call"),
          uri("sip:bob@test.local"),
          msg => sent.update(msg :: _),
          endpoint,
          supervisor,
          Entropy.fromRandom(random),
          media,
          timers,
          5.seconds,
          NoOpLogger.impl[IO]
        )
      )
      _ <- Resource.make(
        call.runEventLoop(Stream.fromQueueUnterminated(queue)).start
      )(_.cancel)
    } yield (call, sent, queue)

    scenario.use { (call, sent, queue) =>
      for {
        _      <- queue.offer(unauthorized) >> queue.offer(unauthorized)
        result <- call.awaitPickup.attempt.timeout(5.seconds)
        out    <- sent.get
      } yield {
        assertEquals(
          result,
          Left(SoftphoneError.CallRejected(401, "Unauthorized"))
        )
        val invites = out.collect {
          case r: SipRequest if r.method == SipMethod.Invite => r
        }
        assertEquals(invites.size, 2) // the first, and one authenticated retry
        assert(
          invites.head.headers.others.exists(_.name.matches("Authorization")),
          "the retry carries credentials"
        )
        assert(invites.last.headers.others.isEmpty, "the first INVITE has none")
      }
    }
  }
}
