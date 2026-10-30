package softphone4s.registration

import cats.effect.testkit.TestControl
import cats.effect.std.Random
import cats.effect.{IO, Resource}
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.DigestVerifier
import softphone4s.SipLiterals.*
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*
import softphone4s.runtime.Entropy
import softphone4s.transaction.*
import softphone4s.transport.{Inbound, InMemoryTransport, Transport}

import scala.concurrent.duration.*

/** The registration agent against a fake registrar (the test's peer), over the
  * in-memory transport and under virtual time.
  */
class RegistrationSuite extends CatsEffectSuite {

  private val us   = InMemoryTransport.AddressA
  private val them = InMemoryTransport.AddressB

  private val config =
    SoftphoneConfig(user = "test", password = "secret", realm = "test.local")

  private val endpoint = LocalEndpoint
    .from(config, ip"127.0.0.1", port"19060", port"19000")
    .fold(err => fail(err.getMessage), identity)

  private def expires(n: Int): Expires =
    Expires(n).fold(err => fail(err), identity)

  private def agent(
      requested: Int = 3600,
      stopTimeout: FiniteDuration = 5.seconds
  ): Resource[IO, (Registration[IO], Transport[IO])] =
    for {
      pair  <- Resource.eval(InMemoryTransport.pair[IO])
      layer <- TransactionManager.resource(
        pair._1,
        Reliability.Unreliable,
        TransactionConfig(),
        NoOpLogger.impl[IO]
      )
      random <- Resource.eval(
        Random.javaUtilRandom[IO](new java.util.Random(7))
      )
      reg <- Registration.resource(
        layer,
        them,
        uri("sip:test.local"),
        endpoint,
        Entropy.fromRandom(random),
        expires(requested),
        stopTimeout,
        NoOpLogger.impl[IO]
      )
    } yield (reg, pair._2)

  private def nextRegister(peer: Transport[IO]) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest) if r.method == SipMethod.Register => r
      }
      .compile
      .lastOrError

  /** The registrar's answer, echoing our Contact with the lifetime granted. */
  private def answer(
      peer: Transport[IO],
      register: SipRequest,
      code: Int,
      reason: String = "OK",
      granted: Option[Int] = None,
      minExpires: Option[Int] = None,
      challenge: Option[String] = None
  ) = {
    val base =
      TransactionMessages.responseTo(register, code, reason, Some(tag("srv")))
    val response = base.copy(
      headers = base.headers.copy(
        contacts = granted.toList.map(g =>
          NameAddr(endpoint.contact).withExpires(expires(g))
        ),
        minExpires = minExpires.map(expires),
        wwwAuthenticate = challenge.toList.map(challengeOf)
      )
    )
    peer.send(us, response)
  }

  private def challengeOf(s: String) = AuthChallenge.parse(s)

  private val digest =
    """Digest realm="test.local", nonce="abc123", qop="auth""""

  private def within(actual: FiniteDuration, base: FiniteDuration) = {
    val max = base * 1.1 + 1.millis
    assert(actual >= base && actual <= max, s"$actual not within [$base, $max]")
  }

  test(
    "it registers, reports Registered with the granted lifetime, and refreshes at about half of it"
  ) {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          t0    <- IO.monotonic
          _     <- answer(peer, first, 200, granted = Some(600))
          _     <- reg.awaitRegistered
          state <- reg.state.get
          again <- nextRegister(peer)
          t1    <- IO.monotonic
        } yield {
          assertEquals(first.headers.expires, Some(expires(3600)))
          assertEquals(
            first.headers.contacts.flatMap(_.expires),
            List(expires(3600))
          )
          assertEquals(first.headers.to.uri, endpoint.accountUri)
          assertEquals(first.headers.to.tag, None)
          assertEquals(state, RegistrationState.Registered(600.seconds))
          within(t1 - t0, 300.seconds)
          assertEquals(again.headers.callId, first.headers.callId)
          assertEquals(again.headers.cseq.seq, first.headers.cseq.seq.next)
          assertNotEquals(
            again.headers.topVia.branch,
            first.headers.topVia.branch
          )
        }
      }
    )
  }

  test("a 401 is answered with credentials and a higher CSeq") {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          _     <- answer(
            peer,
            first,
            401,
            "Unauthorized",
            challenge = Some(digest)
          )
          retry <- nextRegister(peer)
          _     <- answer(peer, retry, 200, granted = Some(3600))
          _     <- reg.awaitRegistered
        } yield {
          assert(first.headers.others.isEmpty)
          assert(retry.headers.others.exists(_.name.matches("Authorization")))
          assertEquals(retry.headers.cseq.seq, first.headers.cseq.seq.next)
          assertEquals(retry.headers.callId, first.headers.callId)
        }
      }
    )
  }

  test(
    "refused credentials fail the registration, then it tries again after the backoff"
  ) {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          _     <- answer(
            peer,
            first,
            401,
            "Unauthorized",
            challenge = Some(digest)
          )
          second <- nextRegister(peer)
          t0     <- IO.monotonic
          _      <- answer(
            peer,
            second,
            401,
            "Unauthorized",
            challenge = Some(digest)
          )
          third <- nextRegister(peer)
          t1    <- IO.monotonic
          state <- reg.state.get
        } yield {
          assertEquals(state, RegistrationState.Registering)
          within(t1 - t0, 5.seconds)
          assert(
            third.headers.others.isEmpty,
            "a new attempt starts without credentials"
          )
        }
      }
    )
  }

  test("the state signal reports each step") {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          seen <- cats.effect.Ref.of[IO, Vector[RegistrationState]](
            Vector.empty
          )
          watch <- reg.state.discrete
            .evalMap(s => seen.update(_ :+ s))
            .compile
            .drain
            .start
          first  <- nextRegister(peer)
          _      <- answer(peer, first, 500, "Server Error")
          retry  <- nextRegister(peer)
          _      <- answer(peer, retry, 200, granted = Some(3600))
          _      <- reg.awaitRegistered
          _      <- IO.sleep(1.second)
          states <- seen.get
          _      <- watch.cancel
        } yield assertEquals(
          states.collect {
            case RegistrationState.Registering => "registering"
            case RegistrationState
                  .Failed(RegistrationFailure.Rejected(500, _), d) =>
              s"failed:${d.toSeconds}"
            case RegistrationState.Registered(l) => s"registered:${l.toSeconds}"
          }.toList,
          List("registering", "failed:5", "registering", "registered:3600")
        )
      }
    )
  }

  test(
    "a registrar that never answers times out at 32 s and is retried 5 s later"
  ) {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          _     <- reg.state.discrete
            .collectFirst {
              case RegistrationState.Failed(RegistrationFailure.TimedOut, _) =>
                ()
            }
            .compile
            .drain
          failedAt <- IO.monotonic
          // Skip the transaction layer's retransmissions of the first REGISTER:
          // the retry is the one with a higher CSeq.
          retry <- peer.messages
            .collectFirst {
              case Inbound(_, r: SipRequest)
                  if r.method == SipMethod.Register &&
                    r.headers.cseq.seq.value > first.headers.cseq.seq.value =>
                r
            }
            .compile
            .lastOrError
          retriedAt <- IO.monotonic
        } yield {
          assertEquals(failedAt, 32.seconds)
          within(retriedAt - failedAt, 5.seconds)
          assertEquals(retry.headers.callId, first.headers.callId)
        }
      }
    )
  }

  private val sha256 =
    """Digest realm="test.local", nonce="n-sha", algorithm=SHA-256, qop="auth""""

  test(
    "a SHA-256 challenge is answered with a response the registrar can verify"
  ) {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          _     <- answer(
            peer,
            first,
            401,
            "Unauthorized",
            challenge = Some(sha256)
          )
          retry <- nextRegister(peer)
          _     <- answer(peer, retry, 200, granted = Some(3600))
          _     <- reg.awaitRegistered
        } yield {
          val verdict = DigestVerifier
            .verify(retry, "secret")
            .getOrElse(fail("no Authorization"))
          assert(verdict.valid, verdict.toString)
          assertEquals(verdict.algorithm, "SHA-256")
        }
      }
    )
  }

  test("SHA-256 is used when the registrar offers MD5 as well") {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          base = TransactionMessages.responseTo(
            first,
            401,
            "Unauthorized",
            Some(tag("srv"))
          )
          _ <- peer.send(
            us,
            base.copy(headers =
              base.headers.copy(wwwAuthenticate =
                List(
                  challengeOf(
                    """Digest realm="test.local", nonce="n-md5", algorithm=MD5, qop="auth""""
                  ),
                  challengeOf(sha256)
                )
              )
            )
          )
          retry <- nextRegister(peer)
          _     <- answer(peer, retry, 200, granted = Some(3600))
          _     <- reg.awaitRegistered
        } yield assertEquals(
          DigestVerifier
            .verify(retry, "secret")
            .map(v => (v.valid, v.algorithm)),
          Some((true, "SHA-256"))
        )
      }
    )
  }

  test("a refresh reuses the credentials at once, with a higher nonce count") {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          _     <- answer(
            peer,
            first,
            401,
            "Unauthorized",
            challenge = Some(sha256)
          )
          retry   <- nextRegister(peer)
          _       <- answer(peer, retry, 200, granted = Some(600))
          _       <- reg.awaitRegistered
          refresh <- nextRegister(
            peer
          ) // after about 300 s, with no 401 in between
          _     <- answer(peer, refresh, 200, granted = Some(600))
          later <- nextRegister(peer)
        } yield {
          val verdicts =
            List(retry, refresh, later).map(DigestVerifier.verify(_, "secret"))
          assert(verdicts.forall(_.exists(_.valid)), verdicts.toString)
          assertEquals(
            verdicts.map(_.map(_.nonce)),
            List.fill(3)(Some("n-sha"))
          )
          assertEquals(
            verdicts.map(_.flatMap(_.nc)),
            List(Some("00000001"), Some("00000002"), Some("00000003"))
          )
          assertEquals(refresh.headers.cseq.seq, retry.headers.cseq.seq.next)
        }
      }
    )
  }

  test(
    "when the registrar finds the cached nonce stale, the fresh one is used"
  ) {
    TestControl.executeEmbed(
      agent().use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          _     <- answer(
            peer,
            first,
            401,
            "Unauthorized",
            challenge = Some(sha256)
          )
          retry   <- nextRegister(peer)
          _       <- answer(peer, retry, 200, granted = Some(600))
          _       <- reg.awaitRegistered
          refresh <- nextRegister(peer)
          _       <- answer(
            peer,
            refresh,
            401,
            "Unauthorized",
            challenge = Some(
              """Digest realm="test.local", nonce="n-new", algorithm=SHA-256, qop="auth", stale=true"""
            )
          )
          again <- nextRegister(peer)
          _     <- answer(peer, again, 200, granted = Some(600))
        } yield {
          val verdict = DigestVerifier
            .verify(again, "secret")
            .getOrElse(fail("no Authorization"))
          assert(verdict.valid)
          assertEquals((verdict.nonce, verdict.nc), ("n-new", Some("00000001")))
        }
      }
    )
  }

  test("a 423 raises the lifetime to Min-Expires") {
    TestControl.executeEmbed(
      agent(requested = 60).use { (reg, peer) =>
        for {
          first <- nextRegister(peer)
          _     <- answer(
            peer,
            first,
            423,
            "Interval Too Brief",
            minExpires = Some(600)
          )
          retry <- nextRegister(peer)
          _     <- answer(peer, retry, 200, granted = Some(600))
          _     <- reg.awaitRegistered
        } yield {
          assertEquals(first.headers.expires, Some(expires(60)))
          assertEquals(retry.headers.expires, Some(expires(600)))
        }
      }
    )
  }

  test(
    "releasing removes the binding with Expires: 0 and waits for the answer"
  ) {
    TestControl.executeEmbed(
      agent().allocated.flatMap { case ((reg, peer), release) =>
        for {
          first     <- nextRegister(peer)
          _         <- answer(peer, first, 200, granted = Some(3600))
          _         <- reg.awaitRegistered
          releasing <- release.start
          removal   <- nextRegister(peer)
          _         <- answer(peer, removal, 200, granted = Some(0))
          _         <- releasing.join
          state     <- reg.state.get
        } yield {
          assertEquals(removal.headers.expires, Some(Expires.Zero))
          assertEquals(
            removal.headers.contacts.flatMap(_.expires),
            List(Expires.Zero)
          )
          assertEquals(removal.headers.callId, first.headers.callId)
          assertEquals(state, RegistrationState.Unregistered)
        }
      }
    )
  }

  test(
    "a release is not held up for ever by a registrar that does not answer"
  ) {
    TestControl.executeEmbed(
      agent(stopTimeout = 2.seconds).allocated.flatMap {
        case ((reg, peer), release) =>
          for {
            first <- nextRegister(peer)
            _     <- answer(peer, first, 200, granted = Some(3600))
            _     <- reg.awaitRegistered
            t0    <- IO.monotonic
            _     <- release
            t1    <- IO.monotonic
          } yield assertEquals(t1 - t0, 2.seconds)
      }
    )
  }
}
