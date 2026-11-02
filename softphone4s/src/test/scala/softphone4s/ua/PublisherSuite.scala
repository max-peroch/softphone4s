package softphone4s.ua

import cats.effect.std.Random
import cats.effect.testkit.TestControl
import cats.effect.{IO, Resource}
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.DigestVerifier
import softphone4s.Publication
import softphone4s.SipLiterals.*
import softphone4s.config.SoftphoneConfig
import softphone4s.model.*
import softphone4s.publication.*
import softphone4s.runtime.Entropy
import softphone4s.subscription.EventHeader
import softphone4s.transaction.{
  TransactionConfig,
  TransactionManager,
  TransactionMessages
}
import softphone4s.transport.{InMemoryTransport, Inbound, Transport}

import scala.concurrent.duration.*

/** A publication against a real transaction layer, the test being the notifier:
  * it reads the PUBLISHes and answers them. Time is virtual, so refreshes and
  * waits take no time.
  */
class PublisherSuite extends CatsEffectSuite {

  private val us         = InMemoryTransport.AddressA
  private val them       = InMemoryTransport.AddressB
  private val presentity = uri("sip:alice@test.local")

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

  private def doc(text: String) = PublicationBody(pidf, text)

  private final case class Rig(
      publication: Publication[IO],
      peer: Transport[IO]
  )

  private def rig(
      requested: Int = 120,
      document: PublicationBody = doc("one")
  ): Resource[IO, Rig] =
    for {
      pair  <- Resource.eval(InMemoryTransport.pair[IO])
      layer <- TransactionManager.resource(
        pair._1,
        TransactionConfig(),
        NoOpLogger.impl[IO]
      )
      random      <- Resource.eval(Random.scalaUtilRandom[IO])
      publication <- Publisher.resource[IO](
        layer,
        cats.data.NonEmptyList.one(Uplink(endpoint, them)),
        presentity,
        presence,
        document,
        expires(requested),
        Entropy.fromRandom(random),
        stopTimeout = 5.seconds,
        NoOpLogger.impl[IO]
      )
    } yield Rig(publication, pair._2)

  // ---- The notifier's side ---------------------------------------------------------

  private def publishWhere(
      peer: Transport[IO]
  )(wanted: SipRequest => Boolean) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest)
            if r.method == SipMethod.Publish && wanted(r) =>
          r
      }
      .compile
      .lastOrError
      .timeout(5.minutes)

  private def nextPublish(peer: Transport[IO]) = publishWhere(peer)(_ => true)

  private def publishNumber(peer: Transport[IO], n: Int) =
    publishWhere(peer)(_.headers.cseq.seq.value == n)

  private def accept(
      peer: Transport[IO],
      request: SipRequest,
      tag: String,
      lifetime: Int = 120
  ) = {
    val base = TransactionMessages.responseTo(request, 200, "OK")
    peer.send(
      us,
      base.copy(headers =
        base.headers.copy(
          expires = Some(expires(lifetime)),
          others =
            ExtensionHeader(FieldName.SipETag, tag) :: base.headers.others
        )
      )
    )
  }

  private def refuse(
      peer: Transport[IO],
      request: SipRequest,
      code: Int,
      reason: String
  ) = peer.send(us, TransactionMessages.responseTo(request, code, reason))

  private def extra(request: SipRequest, name: String) =
    request.headers.others.find(_.name.matches(name)).map(_.value)

  private def virtual[A](program: IO[A]): IO[A] =
    TestControl.executeEmbed(program)

  // ---- Publishing --------------------------------------------------------------------

  test(
    "the first PUBLISH carries the state, the event and the lifetime, and no tag"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextPublish(r.peer)
          _     <- accept(r.peer, first, "t1")
          done  <- r.publication.awaitPublished
          now   <- r.publication.status.get
        } yield {
          assertEquals(first.requestUri.render, "sip:alice@test.local")
          assertEquals(first.body, "one")
          assertEquals(first.headers.contentType, Some(pidf))
          assertEquals(extra(first, "Event"), Some("presence"))
          assertEquals(extra(first, "SIP-If-Match"), None)
          assertEquals(first.headers.expires.map(_.value), Some(120))
          assertEquals(first.headers.cseq.seq.value, 1)
          assertEquals(done, Right(()))
          assertEquals(now, PublicationStatus.Published(120.seconds))
        }
      }
    }
  }

  test(
    "it is refreshed at half the lifetime under the tag, with no body, and the tag the 2xx gives is used next"
  ) {
    virtual {
      rig().use { r =>
        for {
          first  <- nextPublish(r.peer)
          _      <- accept(r.peer, first, "t1")
          start  <- IO.monotonic
          second <- publishNumber(r.peer, 2)
          at     <- IO.monotonic.map(_ - start)
          _      <- accept(r.peer, second, "t2")
          third  <- publishNumber(r.peer, 3)
        } yield {
          assertEquals(at, 60.seconds)
          assertEquals(extra(second, "SIP-If-Match"), Some("t1"))
          assertEquals(second.body, "")
          assertEquals(second.headers.callId, first.headers.callId)
          assertEquals(extra(third, "SIP-If-Match"), Some("t2"))
        }
      }
    }
  }

  // ---- Changing -----------------------------------------------------------------------

  test(
    "a new document is sent at once under the tag, and the tag it gets is used next"
  ) {
    virtual {
      rig().use { r =>
        for {
          first  <- nextPublish(r.peer)
          _      <- accept(r.peer, first, "t1")
          _      <- r.publication.awaitPublished
          _      <- r.publication.update(doc("two"))
          change <- publishNumber(r.peer, 2)
          _      <- accept(r.peer, change, "t2")
          next   <- publishNumber(r.peer, 3)
        } yield {
          assertEquals(change.body, "two")
          assertEquals(extra(change, "SIP-If-Match"), Some("t1"))
          assertEquals(next.body, "")
          assertEquals(extra(next, "SIP-If-Match"), Some("t2"))
        }
      }
    }
  }

  test(
    "a document given while the first PUBLISH is in flight is sent when it is answered"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextPublish(r.peer)
          _     <- r.publication.update(doc("two"))
          _     <- r.publication.update(doc("three"))
          _     <- accept(r.peer, first, "t1")
          next  <- publishNumber(r.peer, 2)
        } yield {
          assertEquals(first.body, "one")
          assertEquals(next.body, "three")
          assertEquals(extra(next, "SIP-If-Match"), Some("t1"))
        }
      }
    }
  }

  // ---- Losing the state -----------------------------------------------------------------

  test(
    "a 412 to a refresh says the state is lost: it is published again, all of it, with no tag"
  ) {
    virtual {
      rig().use { r =>
        for {
          first  <- nextPublish(r.peer)
          _      <- accept(r.peer, first, "t1")
          _      <- r.publication.update(doc("two"))
          change <- publishNumber(r.peer, 2)
          _      <- refuse(r.peer, change, 412, "Conditional Request Failed")
          again  <- publishNumber(r.peer, 3)
        } yield {
          assertEquals(extra(again, "SIP-If-Match"), None)
          assertEquals(again.body, "two")
          assertEquals(again.headers.callId, first.headers.callId)
        }
      }
    }
  }

  // ---- Challenges ---------------------------------------------------------------------------

  test(
    "a challenge is answered once, with the credentials, the same Call-ID and the next CSeq"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextPublish(r.peer)
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
          retry <- publishNumber(r.peer, 2)
        } yield {
          assertEquals(retry.headers.callId, first.headers.callId)
          assertEquals(retry.body, "one")
          assertEquals(
            DigestVerifier.verify(retry, "test").map(_.valid),
            Some(true)
          )
          assertEquals(DigestVerifier.authorization(first), None)
        }
      }
    }
  }

  // ---- Refusals and failures -------------------------------------------------------------------

  test(
    "a refusal that is final ends it: nothing is sent again, and the status and awaitPublished say so"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextPublish(r.peer)
          _     <- refuse(r.peer, first, 403, "Forbidden")
          done  <- r.publication.awaitPublished
          now   <- r.publication.status.get
          more  <- r.peer.messages
            .collectFirst {
              case Inbound(_, q: SipRequest)
                  if q.method == SipMethod.Publish && q.headers.cseq.seq.value != 1 =>
                q
            }
            .compile
            .last
            .timeout(10.minutes)
            .attempt
        } yield {
          assertEquals(done, Left(PublicationEnding.Rejected(403, "Forbidden")))
          assertEquals(
            now,
            PublicationStatus.Ended(
              PublicationEnding.Rejected(403, "Forbidden")
            )
          )
          assert(more.isLeft, s"another PUBLISH was sent: $more")
        }
      }
    }
  }

  test(
    "a failure that may pass is tried again after a wait, as a new run with the latest document"
  ) {
    virtual {
      rig().use { r =>
        for {
          first <- nextPublish(r.peer)
          _     <- refuse(r.peer, first, 503, "Service Unavailable")
          // The program changes its mind while the agent waits.
          _ <- r.publication.status.discrete
            .collectFirst { case s: PublicationStatus.Retrying => s }
            .compile
            .lastOrError
          _    <- r.publication.update(doc("two"))
          anew <- publishWhere(r.peer)(_.headers.callId != first.headers.callId)
          waiting <- r.publication.status.get
        } yield {
          assertEquals(anew.body, "two")
          assertEquals(extra(anew, "SIP-If-Match"), None)
          assertEquals(anew.headers.cseq.seq.value, 1)
          assert(
            waiting != PublicationStatus.Retrying(
              PublicationEnding.Removed,
              0.seconds
            )
          )
        }
      }
    }
  }

  // ---- Releasing it ------------------------------------------------------------------------------

  test(
    "releasing it removes the state: Expires 0 under the tag, with no body"
  ) {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          first     <- nextPublish(r.peer)
          _         <- accept(r.peer, first, "t1")
          _         <- r.publication.awaitPublished
          releasing <- release.start
          removal   <- publishWhere(r.peer)(_.headers.expires.exists(_.isZero))
          _         <- accept(r.peer, removal, "t1", 0)
          _         <- releasing.joinWithNever
          done      <- r.publication.awaitPublished
          now       <- r.publication.status.get
        } yield {
          assertEquals(extra(removal, "SIP-If-Match"), Some("t1"))
          assertEquals(removal.body, "")
          assertEquals(done, Right(()))
          assertEquals(now, PublicationStatus.Ended(PublicationEnding.Removed))
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
          first <- nextPublish(r.peer)
          _     <- accept(r.peer, first, "t1")
          _     <- r.publication.awaitPublished
          start <- IO.monotonic
          _     <- release
          took  <- IO.monotonic.map(_ - start)
        } yield assertEquals(took.toSeconds, 5L)
      }
    }
  }

  test(
    "releasing it at once, before anything is sent, sends no PUBLISH afterwards"
  ) {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          _    <- release
          sent <- r.peer.messages
            .collectFirst {
              case Inbound(_, q: SipRequest) if q.method == SipMethod.Publish =>
                q
            }
            .compile
            .last
            .timeout(10.minutes)
            .attempt
        } yield assert(sent.isLeft || sent.exists(_.isEmpty), s"sent: $sent")
      }
    }
  }

  test("released while it waits to try again, the status says it ended") {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          first <- nextPublish(r.peer)
          _     <- refuse(r.peer, first, 503, "Service Unavailable")
          _     <- r.publication.status.discrete
            .collectFirst { case s: PublicationStatus.Retrying => s }
            .compile
            .lastOrError
          _    <- release
          now  <- r.publication.status.get
          done <- r.publication.awaitPublished
        } yield {
          assertEquals(now, PublicationStatus.Ended(PublicationEnding.Removed))
          assertEquals(done, Left(PublicationEnding.Removed))
        }
      }
    }
  }

  test("a release after it ended for good changes nothing") {
    virtual {
      rig().allocated.flatMap { (r, release) =>
        for {
          first  <- nextPublish(r.peer)
          _      <- refuse(r.peer, first, 403, "Forbidden")
          before <- r.publication.awaitPublished
          _      <- release
          after  <- r.publication.status.get
        } yield {
          assertEquals(
            before,
            Left(PublicationEnding.Rejected(403, "Forbidden"))
          )
          assertEquals(
            after,
            PublicationStatus.Ended(
              PublicationEnding.Rejected(403, "Forbidden")
            )
          )
        }
      }
    }
  }
}
