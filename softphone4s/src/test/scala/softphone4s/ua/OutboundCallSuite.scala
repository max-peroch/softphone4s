package softphone4s.ua

import cats.effect.*
import cats.effect.std.{Queue, Random, Supervisor}
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.Network
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.config.SoftphoneConfig
import softphone4s.fsm.SipMessageBuilder
import softphone4s.model.*
import softphone4s.model.SoftphoneError

import scala.concurrent.duration.*

class OutboundCallSuite extends CatsEffectSuite {

  private val localIp    = ip"127.0.0.1"
  private val sipPort    = port"19060"
  private val rtpPort    = port"19000"
  private val testConfig =
    SoftphoneConfig(user = "test", password = "test", realm = "test.local")
  private val builder = SipMessageBuilder(testConfig, localIp, sipPort, rtpPort)

  private def mkHeaders(cseq: CSeq, to: String = "<sip:bob@test.local>") =
    SipHeaders(
      callId = "test@call",
      from = "<sip:test@test.local>;tag=ltag",
      to = to,
      via = "SIP/2.0/UDP 127.0.0.1:19060;branch=z9hG4bKtest",
      cseq = cseq,
      contact = "<sip:bob@test.local:5060>"
    )

  private val ok200 = SipResponse(
    200,
    "OK",
    mkHeaders(CSeq(1, SipMethod.Invite), to = "<sip:bob@test.local>;tag=rtag")
  )

  private val byeRequest = SipRequest(
    SipMethod.Bye,
    "sip:test@test.local",
    mkHeaders(CSeq(1, SipMethod.Bye))
  )

  private val activeCall
      : Resource[IO, (OutboundCall[IO], Queue[IO, SipMessage])] =
    for {
      queue <- Resource.eval(Queue.unbounded[IO, SipMessage])
      sipIn = Stream.fromQueueUnterminated(queue)
      supervisor <- Supervisor[IO]
      random     <- Resource.eval(Random.scalaUtilRandom[IO])
      media <- Resource.eval(MediaSession.create[IO](160, localIp, rtpPort))
      call  <- Resource.eval(
        OutboundCall.create(
          "test@call",
          "sip:bob@test.local",
          _ => IO.unit,
          builder,
          supervisor,
          random,
          media,
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
}
