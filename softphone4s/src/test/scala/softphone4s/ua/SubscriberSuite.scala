package softphone4s.ua

import cats.data.NonEmptyList
import cats.effect.std.Random
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.concurrent.Channel
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.DigestVerifier
import softphone4s.SipLiterals.*
import softphone4s.Subscription
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*
import softphone4s.runtime.Entropy
import softphone4s.subscription.*
import softphone4s.transaction.{
  TransactionConfig,
  TransactionManager,
  TransactionMessages
}
import softphone4s.transport.{InMemoryTransport, Inbound, Transport}

import scala.concurrent.duration.*
import scala.util.chaining.*

/** A subscription against a real transaction layer, the test being the
  * notifier: it reads the SUBSCRIBEs, answers them and sends the NOTIFYs. Time
  * is virtual, so refreshes and waits take no time.
  */
class SubscriberSuite extends CatsEffectSuite {

  private val us      = InMemoryTransport.AddressA
  private val them    = InMemoryTransport.AddressB
  private val target  = uri("sip:bob@test.local")
  private val contact = nameAddr("<sip:bob@198.51.100.7:5070>")

  private val config =
    SoftphoneConfig(user = "test", password = "test", realm = "test.local")

  private val endpoint =
    LocalEndpoint
      .from(config, ip"127.0.0.1", port"19060", port"19070")
      .fold(err => fail(err.getMessage), identity)

  private val presence = EventHeader("presence").fold(e => fail(e), identity)
  private val pidf     =
    MediaType.parse("application/pidf+xml").fold(e => fail(e), identity)

  private def expires(n: Int) = Expires(n).fold(e => fail(e), identity)

  private final case class Rig(
      subscription: Subscription[IO],
      peer: Transport[IO],
      routes: Ref[IO, Map[CallId, Channel[IO, DialogInput[IO]]]]
  )

  /** The subscription, the notifier's end of the wire, and a router that hands
    * a request to the subscription listed under its Call-ID, as the phone's
    * does.
    */
  private def rig(requested: Int = 120): Resource[IO, Rig] =
    for {
      pair  <- Resource.eval(InMemoryTransport.pair[IO])
      layer <- TransactionManager.resource(
        pair._1,
        TransactionConfig(),
        NoOpLogger.impl[IO]
      )
      random <- Resource.eval(Random.scalaUtilRandom[IO])
      routes <- Resource.eval(
        Ref.of[IO, Map[CallId, Channel[IO, DialogInput[IO]]]](Map.empty)
      )
      _ <- layer.incoming
        .evalMap(tx =>
          routes.get.flatMap(
            _.get(tx.request.headers.callId)
              .fold(
                tx.respond(
                  TransactionMessages
                    .responseTo(tx.request, 481, "Does Not Exist")
                )
              )(_.send(DialogInput.Request(tx)).void)
          )
        )
        .compile
        .drain
        .background
      subscription <- Subscriber.resource[IO](
        layer,
        NonEmptyList.one(Uplink(endpoint, them)),
        routes,
        target,
        presence,
        List(pidf),
        expires(requested),
        (_, _) => IO.pure(them),
        Entropy.fromRandom(random),
        stopTimeout = 5.seconds,
        NoOpLogger.impl[IO]
      )
    } yield Rig(subscription, pair._2, routes)

  // ---- The notifier's side ---------------------------------------------------------

  private def subscribeWhere(
      peer: Transport[IO]
  )(wanted: SipRequest => Boolean) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest)
            if r.method == SipMethod.Subscribe && wanted(r) =>
          r
      }
      .compile
      .lastOrError
      .timeout(5.minutes)

  private def nextSubscribe(peer: Transport[IO]) =
    subscribeWhere(peer)(_ => true)

  private def acceptSubscribe(
      peer: Transport[IO],
      request: SipRequest,
      lifetime: Int = 120
  ) = {
    val base =
      TransactionMessages.responseTo(request, 200, "OK", Some(tag("srv")))
    peer.send(
      us,
      base.copy(headers =
        base.headers.copy(
          contacts = List(contact),
          expires = Some(expires(lifetime))
        )
      )
    )
  }

  private def refuseSubscribe(
      peer: Transport[IO],
      request: SipRequest,
      code: Int,
      reason: String
  ) = peer.send(us, TransactionMessages.responseTo(request, code, reason))

  private def header(name: String, value: String) =
    ExtensionHeader(FieldName(name).fold(e => fail(e), identity), value)

  /** A NOTIFY in the subscription's dialog, as the notifier sends it. */
  private def notifyIn(
      subscribe: SipRequest,
      number: Int,
      state: String,
      body: String = "",
      toTag: Option[Tag] = None,
      events: String = "presence"
  ): SipRequest =
    SipRequest(
      SipMethod.Notify,
      uri("sip:test@127.0.0.1:19060"),
      SipHeaders(
        callId = subscribe.headers.callId,
        from = nameAddr("<sip:bob@test.local>;tag=srv"),
        to = subscribe.headers.from
          .copy(tag = toTag.orElse(subscribe.headers.from.tag)),
        vias = NonEmptyList.one(
          via(s"SIP/2.0/UDP 127.0.0.1:5061;branch=z9hG4bKn$number")
        ),
        cseq = cseq(number, SipMethod.Notify),
        contacts = List(contact),
        contentType = Option.when(body.nonEmpty)(pidf),
        others =
          List(header("Event", events), header("Subscription-State", state))
      ),
      body
    )

  /** Sends the NOTIFY and gives the answer to it. */
  private def notifyAnswered(peer: Transport[IO], request: SipRequest) =
    peer.send(us, request) >>
      peer.messages
        .collectFirst {
          case Inbound(_, r: SipResponse)
              if r.headers.cseq == request.headers.cseq && r.statusCode >= 200 =>
            r
        }
        .compile
        .lastOrError
        .timeout(5.minutes)

  private def virtual[A](program: IO[A]): IO[A] =
    TestControl.executeEmbed(program)

  private def statusNow(s: Subscription[IO]) = s.status.get

  // ---- Subscribing -------------------------------------------------------------------

  test(
    "it sends a SUBSCRIBE with the event, the lifetime and the types, and lists the dialog"
  ) {
    virtual {
      rig().use { r =>
        for {
          request <- nextSubscribe(r.peer)
          listed  <- r.routes.get
        } yield {
          assertEquals(request.requestUri.render, "sip:bob@test.local")
          assertEquals(request.headers.expires.map(_.value), Some(120))
          assertEquals(request.headers.cseq, cseq(1, SipMethod.Subscribe))
          assertEquals(request.headers.to.tag, None)
          assert(request.headers.contacts.nonEmpty)
          assertEquals(
            request.headers.others.find(_.name.matches("Event")).map(_.value),
            Some("presence")
          )
          assertEquals(
            request.headers.others.find(_.name.matches("Accept")).map(_.value),
            Some("application/pidf+xml")
          )
          assert(
            listed.contains(request.headers.callId),
            "the dialog is not listed for its NOTIFYs"
          )
        }
      }
    }
  }

  test(
    "a 2xx and the first NOTIFY make it active: answered, delivered, and awaitActive completes"
  ) {
    virtual {
      rig().use { r =>
        for {
          request <- nextSubscribe(r.peer)
          _       <- acceptSubscribe(r.peer, request)
          answer  <- notifyAnswered(
            r.peer,
            notifyIn(request, 1, "active;expires=120", "<presence/>")
          )
          got    <- r.subscription.notifications.head.compile.lastOrError
          active <- r.subscription.awaitActive
          status <- statusNow(r.subscription)
        } yield {
          assertEquals(answer.statusCode, 200)
          assertEquals(got.body, "<presence/>")
          assertEquals(got.contentType, Some(pidf))
          assertEquals(active, Right(()))
          assertEquals(status, SubscriptionStatus.Active(120.seconds))
        }
      }
    }
  }

  test(
    "a NOTIFY that comes before the 2xx is taken, and the 2xx then finishes it"
  ) {
    virtual {
      rig().use { r =>
        for {
          request <- nextSubscribe(r.peer)
          answer  <- notifyAnswered(
            r.peer,
            notifyIn(request, 1, "active;expires=120", "<presence/>")
          )
          _      <- acceptSubscribe(r.peer, request)
          got    <- r.subscription.notifications.head.compile.lastOrError
          active <- r.subscription.awaitActive
        } yield {
          assertEquals(answer.statusCode, 200)
          assertEquals(got.body, "<presence/>")
          assertEquals(active, Right(()))
        }
      }
    }
  }

  test("every NOTIFY is delivered in order") {
    virtual {
      rig().use { r =>
        for {
          request <- nextSubscribe(r.peer)
          _       <- acceptSubscribe(r.peer, request)
          _ <- List("one", "two", "three").zipWithIndex.traverse_((text, i) =>
            notifyAnswered(
              r.peer,
              notifyIn(request, i + 1, "active;expires=120", text)
            )
          )
          got <- r.subscription.notifications.take(3).compile.toList
        } yield assertEquals(got.map(_.body), List("one", "two", "three"))
      }
    }
  }

  // ---- Refreshing ----------------------------------------------------------------------

  test(
    "at half the lifetime it refreshes in the dialog, with the next CSeq, to the notifier's Contact"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextSubscribe(r.peer)
          _     <- acceptSubscribe(r.peer, first)
          _ <- notifyAnswered(r.peer, notifyIn(first, 1, "active;expires=120"))
          refresh <- nextSubscribe(r.peer)
          _       <- acceptSubscribe(r.peer, refresh)
          status  <- statusNow(r.subscription)
        } yield {
          assertEquals(refresh.headers.callId, first.headers.callId)
          assertEquals(refresh.headers.cseq, cseq(2, SipMethod.Subscribe))
          assertEquals(refresh.headers.to.tag, Some(tag("srv")))
          assertEquals(refresh.requestUri.render, "sip:bob@198.51.100.7:5070")
          assert(status.isInstanceOf[SubscriptionStatus.Active], status)
        }
      }
    }
  }

  test(
    "it refreshes again and again, each time later by the lifetime granted"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextSubscribe(r.peer)
          _     <- acceptSubscribe(r.peer, first)
          _ <- notifyAnswered(r.peer, notifyIn(first, 1, "active;expires=120"))
          start  <- IO.monotonic
          second <- nextSubscribe(r.peer)
          _      <- acceptSubscribe(r.peer, second)
          mid    <- IO.monotonic
          third  <- nextSubscribe(r.peer)
          end    <- IO.monotonic
        } yield {
          assertEquals(second.headers.cseq, cseq(2, SipMethod.Subscribe))
          assertEquals(third.headers.cseq, cseq(3, SipMethod.Subscribe))
          assertEquals((mid - start).toSeconds, 60L)
          assertEquals((end - mid).toSeconds, 60L)
        }
      }
    }
  }

  // ---- Challenges and refusals ------------------------------------------------------------

  test(
    "a challenge is answered once, with the credentials, the same Call-ID and the next CSeq"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextSubscribe(r.peer)
          base = TransactionMessages.responseTo(first, 401, "Unauthorized")
          _ <- r.peer.send(
            us,
            base.copy(headers =
              base.headers.copy(wwwAuthenticate =
                List(
                  challenge(
                    """Digest realm="test.local", nonce="n1", algorithm=MD5, qop="auth""""
                  )
                )
              )
            )
          )
          retry <- subscribeWhere(r.peer)(_.headers.cseq.seq.value == 2)
        } yield {
          assertEquals(retry.headers.callId, first.headers.callId)
          assertEquals(
            DigestVerifier.verify(retry, "test").map(_.valid),
            Some(true)
          )
          assertEquals(DigestVerifier.authorization(first), None)
        }
      }
    }
  }

  test(
    "a refusal that is final ends it for good: the status, the stream and awaitActive say so"
  ) {
    virtual {
      rig().use { r =>
        for {
          request <- nextSubscribe(r.peer)
          _       <- refuseSubscribe(r.peer, request, 403, "Forbidden")
          active  <- r.subscription.awaitActive
          got     <- r.subscription.notifications.compile.toList
          status  <- statusNow(r.subscription)
          listed  <- r.routes.get
        } yield {
          assertEquals(active, Left(Ending.Rejected(403, "Forbidden")))
          assertEquals(got, Nil)
          assertEquals(
            status,
            SubscriptionStatus.Ended(Ending.Rejected(403, "Forbidden"))
          )
          assert(
            !listed.contains(request.headers.callId),
            "an ended dialog is still listed"
          )
        }
      }
    }
  }

  test("a refusal that may pass is tried again after a wait, in a new dialog") {
    virtual {
      rig().use { r =>
        for {
          first   <- nextSubscribe(r.peer)
          _       <- refuseSubscribe(r.peer, first, 503, "Service Unavailable")
          _       <- IO.sleep(1.second)
          waiting <- statusNow(r.subscription)
          whileWaiting <- r.routes.get
          second       <- subscribeWhere(r.peer)(
            _.headers.callId != first.headers.callId
          )
          _ <- acceptSubscribe(r.peer, second)
          _ <- notifyAnswered(r.peer, notifyIn(second, 1, "active;expires=120"))
          active <- r.subscription.awaitActive
        } yield {
          waiting match {
            case SubscriptionStatus.Retrying(
                  Ending.Failed(SubscriptionFailure.Refused(503, _)),
                  in
                ) =>
              assertEquals(in, 5.seconds)
            case other => fail(s"expected Retrying: $other")
          }
          assertNotEquals(second.headers.callId, first.headers.callId)
          assertNotEquals(second.headers.from.tag, first.headers.from.tag)
          // While it waits, the dialog that ended is not listed for NOTIFYs.
          assert(
            !whileWaiting.contains(first.headers.callId),
            whileWaiting.keys.toString
          )
          assertEquals(second.headers.cseq, cseq(1, SipMethod.Subscribe))
          assertEquals(active, Right(()))
        }
      }
    }
  }

  test(
    "no NOTIFY after the 2xx is a failure to try again after, in a new dialog"
  ) {
    virtual {
      rig().use { r =>
        for {
          first  <- nextSubscribe(r.peer)
          _      <- acceptSubscribe(r.peer, first)
          second <- subscribeWhere(r.peer)(
            _.headers.callId != first.headers.callId
          )
          status <- statusNow(r.subscription)
        } yield {
          assertNotEquals(second.headers.callId, first.headers.callId)
          assertEquals(status, SubscriptionStatus.Subscribing)
        }
      }
    }
  }

  // ---- The notifier ends it --------------------------------------------------------------

  test(
    "a NOTIFY that says terminated, deactivated, is followed at once by a new subscription"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextSubscribe(r.peer)
          _     <- acceptSubscribe(r.peer, first)
          _ <- notifyAnswered(r.peer, notifyIn(first, 1, "active;expires=120"))
          answer <- notifyAnswered(
            r.peer,
            notifyIn(first, 2, "terminated;reason=deactivated")
          )
          second <- subscribeWhere(r.peer)(
            _.headers.callId != first.headers.callId
          )
          listed <- r.routes.get
        } yield {
          assertEquals(answer.statusCode, 200)
          assertNotEquals(second.headers.callId, first.headers.callId)
          assert(!listed.contains(first.headers.callId))
          assert(listed.contains(second.headers.callId))
        }
      }
    }
  }

  test(
    "a NOTIFY that says terminated, rejected, ends it for good and is not followed"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextSubscribe(r.peer)
          _     <- acceptSubscribe(r.peer, first)
          _     <- notifyAnswered(
            r.peer,
            notifyIn(first, 1, "terminated;reason=rejected")
          )
          active <- r.subscription.awaitActive
          more <- subscribeWhere(r.peer)(_ => true).timeout(10.minutes).attempt
          status <- statusNow(r.subscription)
        } yield {
          assertEquals(
            active,
            Left(Ending.Terminated(EndReason.Rejected, None))
          )
          assert(more.isLeft, "it subscribed again after being rejected")
          assertEquals(
            status,
            SubscriptionStatus.Ended(
              Ending.Terminated(EndReason.Rejected, None)
            )
          )
        }
      }
    }
  }

  test(
    "a NOTIFY that says probation waits as long as it says before a new subscription"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextSubscribe(r.peer)
          _     <- acceptSubscribe(r.peer, first)
          _     <- notifyAnswered(
            r.peer,
            notifyIn(first, 1, "terminated;reason=probation;retry-after=90")
          )
          start  <- IO.monotonic
          second <- subscribeWhere(r.peer)(
            _.headers.callId != first.headers.callId
          )
          waited <- IO.monotonic.map(_ - start)
        } yield {
          assertNotEquals(second.headers.callId, first.headers.callId)
          assertEquals(waited.toSeconds, 90L)
        }
      }
    }
  }

  // ---- Requests that do not belong -----------------------------------------------------------

  test(
    "a NOTIFY for another event is answered 481, one with the wrong tag 481, one with no state 400"
  ) {
    virtual {
      rig().use { r =>
        for {
          request <- nextSubscribe(r.peer)
          _       <- acceptSubscribe(r.peer, request)
          other   <- notifyAnswered(
            r.peer,
            notifyIn(request, 1, "active", events = "dialog")
          )
          wrong <- notifyAnswered(
            r.peer,
            notifyIn(request, 2, "active", toTag = Some(tag("not-ours")))
          )
          noState <- notifyAnswered(
            r.peer,
            notifyIn(request, 3, "active").copy(headers =
              notifyIn(request, 3, "active").headers
                .copy(others = List(header("Event", "presence")))
            )
          )
        } yield {
          assertEquals(other.statusCode, 481)
          assertEquals(wrong.statusCode, 481)
          assertEquals(noState.statusCode, 400)
        }
      }
    }
  }

  test("any other request to the dialog is answered 405") {
    virtual {
      rig().use { r =>
        for {
          request <- nextSubscribe(r.peer)
          _       <- acceptSubscribe(r.peer, request)
          bye = notifyIn(request, 1, "active")
            .copy(method = SipMethod.Bye)
            .pipe(b =>
              b.copy(headers = b.headers.copy(cseq = cseq(1, SipMethod.Bye)))
            )
          answer <- r.peer.send(us, bye) >> r.peer.messages
            .collectFirst {
              case Inbound(_, resp: SipResponse)
                  if resp.headers.cseq.method == SipMethod.Bye =>
                resp
            }
            .compile
            .lastOrError
        } yield assertEquals(answer.statusCode, 405)
      }
    }
  }

  // ---- Releasing it -------------------------------------------------------------------------------

  // ---- Time runs out -------------------------------------------------------------------------

  test(
    "a lifetime that runs out with no refresh answered ends the dialog, and a new one begins at once"
  ) {
    virtual {
      rig(requested = 10).use { r =>
        for {
          first <- nextSubscribe(r.peer)
          _     <- acceptSubscribe(r.peer, first, lifetime = 10)
          _ <- notifyAnswered(r.peer, notifyIn(first, 1, "active;expires=10"))
          start <- IO.monotonic
          // The refresh at five seconds goes unanswered, and is passed over here.
          second <- subscribeWhere(r.peer)(
            _.headers.callId != first.headers.callId
          )
          waited <- IO.monotonic.map(_ - start)
        } yield {
          assertNotEquals(second.headers.callId, first.headers.callId)
          assertEquals(waited.toSeconds, 10L)
        }
      }
    }
  }

  // ---- Releasing it -------------------------------------------------------------------------------

  test(
    "releasing it sends a SUBSCRIBE for nothing in the dialog and finishes when the last NOTIFY comes"
  ) {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          first <- nextSubscribe(r.peer)
          _     <- acceptSubscribe(r.peer, first)
          _ <- notifyAnswered(r.peer, notifyIn(first, 1, "active;expires=120"))
          releasing <- release.start
          leave <- subscribeWhere(r.peer)(_.headers.expires.exists(_.isZero))
          _     <- acceptSubscribe(r.peer, leave, lifetime = 0)
          last  <- notifyAnswered(
            r.peer,
            notifyIn(first, 2, "terminated;reason=timeout")
          )
          _ <- releasing.joinWithNever
        } yield {
          assertEquals(leave.headers.callId, first.headers.callId)
          assertEquals(leave.headers.to.tag, Some(tag("srv")))
          assertEquals(leave.headers.cseq, cseq(2, SipMethod.Subscribe))
          assertEquals(last.statusCode, 200)
        }
      }
    }
  }

  test(
    "releasing it while the notifier does not answer gives up after the time allowed"
  ) {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          first <- nextSubscribe(r.peer)
          _     <- acceptSubscribe(r.peer, first)
          _ <- notifyAnswered(r.peer, notifyIn(first, 1, "active;expires=120"))
          start <- IO.monotonic
          _     <- release
          took  <- IO.monotonic.map(_ - start)
        } yield assertEquals(took.toSeconds, 5L)
      }
    }
  }

  test(
    "releasing it before it is answered ends it without waiting on a dialog that never was"
  ) {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          _      <- nextSubscribe(r.peer)
          start  <- IO.monotonic
          _      <- release
          took   <- IO.monotonic.map(_ - start)
          listed <- r.routes.get
        } yield {
          assertEquals(took.toSeconds, 0L)
          assertEquals(listed, Map.empty)
        }
      }
    }
  }

  test(
    "releasing it while it waits to begin again ends it at once, and the stream ends"
  ) {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          first   <- nextSubscribe(r.peer)
          _       <- refuseSubscribe(r.peer, first, 503, "Later")
          _       <- IO.sleep(1.second)
          waiting <- r.subscription.status.get
          _       <- release
          got     <- r.subscription.notifications.compile.toList
        } yield {
          assert(waiting.isInstanceOf[SubscriptionStatus.Retrying], waiting)
          assertEquals(got, Nil)
        }
      }
    }
  }

  test("released while it waits to try again, the status says it ended") {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          request <- nextSubscribe(r.peer)
          _ <- refuseSubscribe(r.peer, request, 503, "Service Unavailable")
          _ <- r.subscription.status.discrete
            .collectFirst { case s: SubscriptionStatus.Retrying => s }
            .compile
            .lastOrError
          _    <- release
          now  <- r.subscription.status.get
          done <- r.subscription.awaitActive
        } yield {
          assertEquals(now, SubscriptionStatus.Ended(Ending.Unsubscribed))
          assertEquals(done, Left(Ending.Unsubscribed))
        }
      }
    }
  }

  test(
    "the end of the stream stays: a reader that comes after it ends at once"
  ) {
    virtual {
      rig().use { r =>
        for {
          request <- nextSubscribe(r.peer)
          _       <- refuseSubscribe(r.peer, request, 403, "Forbidden")
          first   <- r.subscription.notifications.compile.toList
          second  <- r.subscription.notifications.compile.toList
            .timeout(1.minute)
          third <- r.subscription.notifications.compile.toList.timeout(1.minute)
        } yield assertEquals((first, second, third), (Nil, Nil, Nil))
      }
    }
  }

  test(
    "releasing it at once, before anything has been sent, sends no SUBSCRIBE afterwards"
  ) {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          _ <- release
          // The notifier's end of the wire stays open after the subscription is
          // released; nothing may arrive on it.
          sent <- r.peer.messages
            .collectFirst {
              case Inbound(_, q: SipRequest)
                  if q.method == SipMethod.Subscribe =>
                q
            }
            .compile
            .last
            .timeout(10.minutes)
            .attempt
          listed <- r.routes.get
        } yield {
          assert(
            sent.isLeft || sent.exists(_.isEmpty),
            s"a SUBSCRIBE was sent after the release: $sent"
          )
          assertEquals(listed, Map.empty)
        }
      }
    }
  }
}
