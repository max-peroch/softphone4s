package softphone4s

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.config.{CodecChoice, SoftphoneConfig}
import softphone4s.media.HalfByteCodec
import softphone4s.metrics.RecordingMetrics
import softphone4s.registration.RegistrationState
import softphone4s.messaging.{MessageContent, MessageFailure}
import softphone4s.dialog.{DialogInfo, DialogState}
import softphone4s.presence.{Availability, Presence, PresenceTuple}
import softphone4s.subscription.{Ending, EventHeader, MessageSummary}
import softphone4s.model.*
import softphone4s.transaction.TransactionMessages
import softphone4s.transport.{Inbound, SipTransport, Transport}

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** The whole stack over real loopback UDP: a `SoftphoneStreaming` places a call
  * to a bare transport that plays the remote party.
  */
class SoftphoneStreamingSuite extends CatsEffectSuite {

  private val ours   = port"19070"
  private val theirs = port"19071"

  private val config = SoftphoneConfig(
    user = sipUser("test"),
    password = Secret("test"),
    realm = host"127.0.0.1",
    localSipPort = ours,
    serverPort = theirs,
    baseRtpPort = port"19100",
    bindAddress = ip"127.0.0.1",
    hangupTimeout = 2.seconds.positive
  )

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  private val phone =
    SoftphoneStreaming.resource[IO](config, audio, NoOpLogger.impl[IO])

  private val remote: cats.effect.Resource[IO, Transport[IO]] =
    SipTransport
      .udp[IO](SocketAddress(ip"127.0.0.1", theirs), NoOpLogger.impl[IO])

  private val callee = Callee("bob@127.0.0.1")
    .fold(err => fail(err), identity)

  private val us = SocketAddress(ip"127.0.0.1", ours)

  private def requestOf(peer: Transport[IO], method: SipMethod) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest) if r.method == method => r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  private def okFor(invite: SipRequest): SipResponse = {
    val base =
      TransactionMessages.responseTo(invite, 200, "OK", Some(tag("rtag")))
    base.copy(
      headers = base.headers.copy(
        contacts = List(nameAddr("<sip:bob@127.0.0.1:19071>")),
        contentType = Some(MediaType.Sdp)
      ),
      body = "v=0\r\nc=IN IP4 127.0.0.1\r\nm=audio 40000 RTP/AVP 0\r\n"
    )
  }

  private def byeFor(
      invite: SipRequest,
      ok: SipResponse,
      callId: CallId
  ): SipRequest =
    SipRequest(
      SipMethod.Bye,
      uri("sip:test@127.0.0.1:19070"),
      SipHeaders(
        callId = callId,
        from = ok.headers.to,
        to = invite.headers.from,
        vias = NonEmptyList.one(
          via("SIP/2.0/UDP 127.0.0.1:19071;branch=z9hG4bKbye1")
        ),
        cseq = cseq(1, SipMethod.Bye)
      )
    )

  test("a call over UDP is answered, ACKed, and ended by the remote BYE") {
    (phone, remote).tupled.use { (softphone, peer) =>
      softphone.call(callee).use { call =>
        for {
          invite <- requestOf(peer, SipMethod.Invite)
          ok = okFor(invite)
          _      <- peer.send(us, ok)
          _      <- call.awaitPickup.timeout(5.seconds)
          ack    <- requestOf(peer, SipMethod.Ack)
          _      <- peer.send(us, byeFor(invite, ok, invite.headers.callId))
          _      <- call.awaitHangup.timeout(5.seconds)
          answer <- peer.messages
            .collectFirst {
              case Inbound(_, r: SipResponse)
                  if r.headers.cseq.method == SipMethod.Bye =>
                r
            }
            .compile
            .lastOrError
            .timeout(5.seconds)
        } yield {
          assertEquals(ack.headers.callId, invite.headers.callId)
          assertEquals(answer.statusCode, 200)
        }
      }
    }
  }

  /** `io`, again after a moment while it fails for want of a pair of ports. */
  private def retrying[A](io: IO[A], times: Int): IO[A] =
    io.handleErrorWith {
      case _: SoftphoneError.RtpPortsExhausted if times > 0 =>
        IO.sleep(100.millis) >> retrying(io, times - 1)
      case other => IO.raiseError(other)
    }

  test("the RTP ports of a call go back to the pool when it ends") {
    // One pair of ports for the whole phone.
    val small = SoftphoneStreaming.resource[IO](
      config.copy(
        // A pair no other suite binds: they run side by side.
        baseRtpPort = port"19180",
        maxRtpPort = Some(port"19181")
      ),
      audio,
      NoOpLogger.impl[IO]
    )
    (small, remote).tupled.use { (softphone, peer) =>
      for {
        first <- softphone.call(callee).use { call =>
          for {
            invite <- requestOf(peer, SipMethod.Invite)
            _      <- peer.send(us, okFor(invite))
            _      <- call.awaitPickup.timeout(5.seconds)
            // The pair is held, so another call finds none.
            blocked <- softphone.call(callee).use_.attempt
          } yield (invite, blocked)
        }
        // The first call is over, and its pair is free again.
        // The system may take a moment to let go of the sockets just closed.
        second <- retrying(
          softphone
            .call(callee)
            .use(_ => requestOf(peer, SipMethod.Invite)),
          20
        )
      } yield {
        assertEquals(
          first._2.left.toOption,
          Some(SoftphoneError.RtpPortsExhausted(19180, 19181))
        )
        assert(first._1.body.contains("m=audio 19180"), first._1.body)
        assert(second.body.contains("m=audio 19180"), second.body)
      }
    }
  }

  test(
    "a range of RTP ports with no pair in it is refused when the phone starts"
  ) {
    SoftphoneStreaming
      .resource[IO](
        config.copy(
          baseRtpPort = port"19130",
          maxRtpPort = Some(port"19130")
        ),
        audio,
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map(result =>
        assert(
          result.left.exists(_.isInstanceOf[SoftphoneError.RtpPortsInvalid]),
          result
        )
      )
  }

  test(
    "a message is sent to the callee over UDP and delivered when answered 200"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      for {
        sending <- softphone.messages
          .send(callee, MessageContent.plain("hi bob, é ✓"))
          .start
        message <- requestOf(peer, SipMethod.Message)
        _ <- peer.send(us, TransactionMessages.responseTo(message, 200, "OK"))
        result <- sending.joinWithNever.timeout(5.seconds)
      } yield {
        assertEquals(result, Right(()))
        assertEquals(message.body, "hi bob, é ✓")
        assertEquals(message.requestUri.render, "sip:bob@127.0.0.1")
        assertEquals(message.headers.contentType, Some(MediaType.Plain))
        // From us, as the account, with a tag: outside a dialog the To has none.
        assertEquals(message.headers.to.tag, None)
        assert(message.headers.from.tag.isDefined)
      }
    }
  }

  test("a message that is refused says why") {
    (phone, remote).tupled.use { (softphone, peer) =>
      for {
        sending <- softphone.messages
          .send(callee, MessageContent.plain("hi"))
          .start
        message <- requestOf(peer, SipMethod.Message)
        _       <- peer.send(
          us,
          TransactionMessages.responseTo(message, 403, "Forbidden")
        )
        result <- sending.joinWithNever.timeout(5.seconds)
      } yield assertEquals(
        result,
        Left(MessageFailure.Rejected(403, "Forbidden"))
      )
    }
  }

  // ---- Messages that arrive -------------------------------------------------------------

  private def messageRequest(
      id: String,
      body: String,
      contentType: Option[MediaType] = Some(MediaType.Plain),
      user: String = "test"
  ): SipRequest =
    SipRequest(
      SipMethod.Message,
      uri(s"sip:$user@127.0.0.1:19070"),
      SipHeaders(
        callId = callId(s"$id@host"),
        from = nameAddr("<sip:bob@127.0.0.1>;tag=ft"),
        to = nameAddr(s"<sip:$user@127.0.0.1>"),
        vias = NonEmptyList.one(
          via(s"SIP/2.0/UDP 127.0.0.1:19071;branch=z9hG4bK$id")
        ),
        cseq = cseq(1, SipMethod.Message),
        contentType = contentType
      ),
      body
    )

  private def finalResponse(peer: Transport[IO]) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipResponse) if r.statusCode >= 200 => r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  test(
    "a message that arrives is answered 200 and read from messages.incoming"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      for {
        _        <- peer.send(us, messageRequest("m1", "hi there, é ✓"))
        answer   <- finalResponse(peer)
        received <- softphone.messages.incoming.head.compile.lastOrError
          .timeout(5.seconds)
      } yield {
        assertEquals(answer.statusCode, 200)
        assertEquals(answer.headers.callId, callId("m1@host"))
        assertEquals(received.content, MessageContent.plain("hi there, é ✓"))
        assertEquals(received.from.uri.render, "sip:bob@127.0.0.1")
        assertEquals(received.account.render, "sip:test@127.0.0.1")
      }
    }
  }

  test("messages are read in the order they came") {
    (phone, remote).tupled.use { (softphone, peer) =>
      for {
        _ <- List("one", "two", "three").zipWithIndex.traverse_((text, i) =>
          peer.send(us, messageRequest(s"o$i", text)) >> finalResponse(peer)
        )
        got <- softphone.messages.incoming
          .take(3)
          .compile
          .toList
          .timeout(5.seconds)
      } yield assertEquals(got.map(_.content.text), List("one", "two", "three"))
    }
  }

  test(
    "a message of a type we cannot read is answered 415 with what we take, and does not appear"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      val html = MediaType.parse("text/html").fold(e => fail(e), identity)
      for {
        _      <- peer.send(us, messageRequest("h1", "<b>hi</b>", Some(html)))
        answer <- finalResponse(peer)
        none   <- softphone.messages.incoming.head.compile.last
          .timeout(300.millis)
          .attempt
      } yield {
        assertEquals(answer.statusCode, 415)
        assertEquals(
          answer.headers.others.map(h => (h.name.value, h.value)),
          List(("Accept", "text/plain"))
        )
        assert(none.isLeft, "a message that was refused appeared")
      }
    }
  }

  test("text in a character set that would be garbled is answered 415") {
    (phone, remote).tupled.use { (_, peer) =>
      val latin = MediaType
        .parse("text/plain;charset=iso-8859-1")
        .fold(e => fail(e), identity)
      peer.send(us, messageRequest("l1", "caf\u00e9", Some(latin))) >>
        finalResponse(peer).map(a => assertEquals(a.statusCode, 415))
    }
  }

  test("a body that does not say what it is is answered 400") {
    (phone, remote).tupled.use { (_, peer) =>
      peer.send(us, messageRequest("n1", "no type", None)) >>
        finalResponse(peer).map(a => assertEquals(a.statusCode, 400))
    }
  }

  test("a retransmitted message is answered again and appears once") {
    (phone, remote).tupled.use { (softphone, peer) =>
      val request = messageRequest("r1", "once")
      for {
        _     <- peer.send(us, request)
        _     <- finalResponse(peer)
        _     <- peer.send(us, request)
        again <- finalResponse(peer)
        got   <- softphone.messages.incoming
          .take(2)
          .compile
          .toList
          .timeout(500.millis)
          .attempt
      } yield {
        assertEquals(again.statusCode, 200)
        // Only one arrived, so waiting for a second times out.
        assert(got.isLeft, got)
      }
    }
  }

  test(
    "when nobody reads, the messages that wait are limited, and the next is refused 503"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      val n = SoftphoneStreaming.MessageBacklog + 1
      for {
        collected <- peer.messages
          .collect {
            case Inbound(_, r: SipResponse) if r.statusCode >= 200 => r
          }
          .take(n.toLong)
          .compile
          .toList
          .timeout(10.seconds)
          .start
        _ <- (1 to n).toList.traverse_(i =>
          peer.send(us, messageRequest(s"b$i", s"message $i"))
        )
        statuses <- collected.joinWithNever.map(_.map(_.statusCode))
        // A message read makes room for the next.
        _     <- softphone.messages.incoming.head.compile.lastOrError
        _     <- peer.send(us, messageRequest("b-after", "after"))
        after <- finalResponse(peer)
      } yield {
        assertEquals(
          statuses.count(_ == 200),
          SoftphoneStreaming.MessageBacklog
        )
        assertEquals(statuses.count(_ == 503), 1)
        assertEquals(after.statusCode, 200)
      }
    }
  }

  // ---- Subscriptions ---------------------------------------------------------------------

  private val presence = EventHeader("presence").fold(e => fail(e), identity)
  private val pidf     =
    MediaType.parse("application/pidf+xml").fold(e => fail(e), identity)

  private def subscribeRequest(
      peer: Transport[IO],
      wanted: SipRequest => Boolean = _ => true
  ) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest)
            if r.method == SipMethod.Subscribe && wanted(r) =>
          r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  private def acceptedSubscribe(subscribe: SipRequest, lifetime: Int) = {
    val base =
      TransactionMessages.responseTo(subscribe, 200, "OK", Some(tag("srv")))
    base.copy(headers =
      base.headers.copy(
        contacts = List(nameAddr("<sip:bob@127.0.0.1:19071>")),
        expires = Some(Expires(lifetime).fold(e => fail(e), identity))
      )
    )
  }

  private def notifyFor(
      subscribe: SipRequest,
      number: Int,
      state: String,
      body: String = "",
      event: String = "presence",
      contentType: MediaType = pidf
  ): SipRequest =
    SipRequest(
      SipMethod.Notify,
      uri("sip:test@127.0.0.1:19070"),
      SipHeaders(
        callId = subscribe.headers.callId,
        from = nameAddr("<sip:bob@127.0.0.1>;tag=srv"),
        to = subscribe.headers.from,
        vias = NonEmptyList.one(
          via(s"SIP/2.0/UDP 127.0.0.1:19071;branch=z9hG4bKn$number")
        ),
        cseq = cseq(number, SipMethod.Notify),
        contacts = List(nameAddr("<sip:bob@127.0.0.1:19071>")),
        contentType = Option.when(body.nonEmpty)(contentType),
        others = List(
          ExtensionHeader(
            FieldName("Event").fold(e => fail(e), identity),
            event
          ),
          ExtensionHeader(
            FieldName("Subscription-State").fold(e => fail(e), identity),
            state
          )
        )
      ),
      body
    )

  private def answerTo(peer: Transport[IO], request: SipRequest) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipResponse)
            if r.headers.cseq == request.headers.cseq && r.statusCode >= 200 =>
          r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  test(
    "a subscription over UDP: the SUBSCRIBE, the NOTIFY, a refresh in the dialog, and the end on release"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      softphone.subscriptions
        .subscribe(callee, presence, List(pidf))
        .allocated
        .flatMap { (subscription, release) =>
          for {
            first <- subscribeRequest(peer)
            _     <- peer.send(us, acceptedSubscribe(first, 3600))
            notice = notifyFor(first, 1, "active;expires=3600", "<presence/>")
            _      <- peer.send(us, notice)
            answer <- answerTo(peer, notice)
            got    <- subscription.notifications.head.compile.lastOrError
              .timeout(5.seconds)
            active <- subscription.awaitActive.timeout(5.seconds)
            // Releasing it ends the subscription in its dialog.
            releasing <- release.start
            leave <- subscribeRequest(peer, _.headers.expires.exists(_.isZero))
            _     <- peer.send(us, acceptedSubscribe(leave, 0))
            _ <- peer.send(us, notifyFor(first, 2, "terminated;reason=timeout"))
            _ <- releasing.joinWithNever.timeout(10.seconds)
          } yield {
            // The resource is addressed as given; the port only decides where it goes.
            assertEquals(first.requestUri.render, "sip:bob@127.0.0.1")
            assertEquals(first.headers.from.uri.render, "sip:test@127.0.0.1")
            assertEquals(
              first.headers.others.find(_.name.matches("Event")).map(_.value),
              Some("presence")
            )
            assertEquals(answer.statusCode, 200)
            assertEquals(got.body, "<presence/>")
            assertEquals(active, Right(()))
            assertEquals(leave.headers.callId, first.headers.callId)
            assertEquals(leave.headers.to.tag, Some(tag("srv")))
            assertEquals(leave.requestUri.render, "sip:bob@127.0.0.1:19071")
          }
        }
    }
  }

  // ---- Publishing --------------------------------------------------------------------

  private def publishRequest(
      peer: Transport[IO],
      wanted: SipRequest => Boolean = _ => true
  ) =
    peer.messages
      .collectFirst {
        case Inbound(_, r: SipRequest)
            if r.method == SipMethod.Publish && wanted(r) =>
          r
      }
      .compile
      .lastOrError
      .timeout(5.seconds)

  private def acceptedPublish(
      publish: SipRequest,
      tag: String,
      lifetime: Int
  ) = {
    val base = TransactionMessages.responseTo(publish, 200, "OK")
    base.copy(headers =
      base.headers.copy(
        expires = Some(Expires(lifetime).fold(e => fail(e), identity)),
        others = ExtensionHeader(FieldName.SipETag, tag) :: base.headers.others
      )
    )
  }

  private val mine = Presence(
    "pres:test@127.0.0.1",
    List(
      PresenceTuple("t1", Some(Availability.Open), None, List("at my desk"))
    ),
    Nil
  )

  test(
    "presence is published over UDP: the PUBLISH, a change under the tag, and the removal on release"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      softphone.publications.publishPresence(mine).allocated.flatMap {
        (publication, release) =>
          for {
            first <- publishRequest(peer)
            _     <- peer.send(us, acceptedPublish(first, "tag1", 3600))
            done  <- publication.awaitPublished.timeout(5.seconds)
            busy = mine.copy(tuples =
              List(PresenceTuple("t1", Some(Availability.Closed), None, Nil))
            )
            updated   <- publication.updatePresence(busy)
            change    <- publishRequest(peer, _.headers.cseq.seq.value == 2)
            _         <- peer.send(us, acceptedPublish(change, "tag2", 3600))
            releasing <- release.start
            removal <- publishRequest(peer, _.headers.expires.exists(_.isZero))
            _       <- peer.send(us, acceptedPublish(removal, "tag2", 0))
            _       <- releasing.joinWithNever.timeout(10.seconds)
          } yield {
            // The state is that of our own address.
            assertEquals(first.requestUri.render, "sip:test@127.0.0.1")
            assertEquals(first.headers.from.uri.render, "sip:test@127.0.0.1")
            assertEquals(
              first.headers.others.find(_.name.matches("Event")).map(_.value),
              Some("presence")
            )
            assertEquals(first.headers.contentType, Some(MediaType.PidfXml))
            assertEquals(Presence.parse(first.body), Right(mine))
            assertEquals(
              first.headers.others.find(_.name.matches("SIP-If-Match")),
              None
            )
            assertEquals(done, Right(()))
            assertEquals(updated, Right(()))
            assertEquals(Presence.parse(change.body), Right(busy))
            assertEquals(
              change.headers.others
                .find(_.name.matches("SIP-If-Match"))
                .map(_.value),
              Some("tag1")
            )
            assertEquals(
              removal.headers.others
                .find(_.name.matches("SIP-If-Match"))
                .map(_.value),
              Some("tag2")
            )
            assertEquals(removal.body, "")
          }
      }
    }
  }

  test(
    "a presence that cannot be written is refused and nothing is sent, and a change to one is refused the same way"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      val broken = mine.copy(notes = List("bell \u0007"))
      for {
        refused <- softphone.publications.publishPresence(broken).use_.attempt
        sent    <- peer.messages
          .collectFirst {
            case Inbound(_, r: SipRequest) if r.method == SipMethod.Publish => r
          }
          .compile
          .last
          .timeout(500.millis)
          .attempt
        changed <- softphone.publications.publishPresence(mine).use {
          publication =>
            publication.updatePresence(broken)
        }
      } yield {
        assert(
          refused.left.exists {
            case SoftphoneError.InvalidPublication(_) => true
            case _                                    => false
          },
          refused.toString
        )
        assert(
          sent.isLeft,
          "a PUBLISH was sent for a presence that cannot be written"
        )
        assert(changed.isLeft)
      }
    }
  }

  test(
    "a package is subscribed to by its Event and Accept, and its states are read"
  ) {
    def summary(n: Int, state: String, body: String) =
      (first: SipRequest) =>
        notifyFor(
          first,
          n,
          state,
          body,
          "message-summary",
          MediaType.SimpleMessageSummary
        )
    (phone, remote).tupled.use { (softphone, peer) =>
      softphone.subscriptions
        .watch(callee, MessageSummary.Package)
        .use { watched =>
          for {
            first <- subscribeRequest(peer)
            _     <- peer.send(us, acceptedSubscribe(first, 3600))
            good = summary(
              1,
              "active;expires=3600",
              "Messages-Waiting: yes\r\nVoice-Message: 2/8\r\n"
            )(first)
            _ <- peer.send(us, good)
            _ <- answerTo(peer, good)
            _ <- peer.send(
              us,
              summary(2, "active;expires=3000", "Messages-Waiting: maybe\r\n")(
                first
              )
            )
            states <- watched.states.take(2).compile.toList.timeout(5.seconds)
          } yield {
            def header(name: String) =
              first.headers.others.find(_.name.matches(name)).map(_.value)
            assertEquals(header("Event"), Some("message-summary"))
            assertEquals(
              header("Accept"),
              Some("application/simple-message-summary")
            )
            assertEquals(
              states.headOption.flatMap(_.toOption).map(_.waiting),
              Some(true)
            )
            // A body that does not follow the format is a Left, and the stream
            // goes on.
            assert(states.lift(1).exists(_.isLeft), states.toString)
          }
        }
    }
  }

  test(
    "the dialog package is subscribed to as dialog, and a full state then a partial one are kept track of"
  ) {
    val ns = """xmlns="urn:ietf:params:xml:ns:dialog-info""""
    def dialogs(version: Int, state: String, inner: String) =
      s"""<dialog-info $ns version="$version" state="$state" entity="sip:bob@127.0.0.1">$inner</dialog-info>"""
    def dialogNotify(first: SipRequest, n: Int, state: String, body: String) =
      notifyFor(first, n, state, body, "dialog", MediaType.DialogInfoXml)
    (phone, remote).tupled.use { (softphone, peer) =>
      softphone.subscriptions
        .watch(callee, DialogInfo.Package)
        .use { watched =>
          for {
            first <- subscribeRequest(peer)
            _     <- peer.send(us, acceptedSubscribe(first, 3600))
            full = dialogNotify(
              first,
              1,
              "active;expires=3600",
              dialogs(
                1,
                "full",
                """<dialog id="c1"><state>early</state></dialog>"""
              )
            )
            _ <- peer.send(us, full)
            _ <- answerTo(peer, full)
            partial = dialogNotify(
              first,
              2,
              "active;expires=3000",
              dialogs(
                2,
                "partial",
                """<dialog id="c1"><state>confirmed</state></dialog><dialog id="c2"><state>trying</state></dialog>"""
              )
            )
            _      <- peer.send(us, partial)
            states <- watched.states.take(2).compile.toList.timeout(5.seconds)
          } yield {
            def header(name: String) =
              first.headers.others.find(_.name.matches(name)).map(_.value)
            assertEquals(header("Event"), Some("dialog"))
            assertEquals(header("Accept"), Some("application/dialog-info+xml"))
            val kept = states.foldLeft(Option.empty[DialogInfo]) {
              (known, state) =>
                state.toOption
                  .flatMap(update => DialogInfo.track(known, update).toOption)
            }
            assertEquals(
              kept.map(_.dialogs.map(d => d.id -> d.state)),
              Some(
                List("c1" -> DialogState.Confirmed, "c2" -> DialogState.Trying)
              )
            )
          }
        }
    }
  }

  test(
    "a subscription that is refused for good says so, and a NOTIFY for no subscription is answered 481"
  ) {
    (phone, remote).tupled.use { (softphone, peer) =>
      softphone.subscriptions.subscribe(callee, presence).use { subscription =>
        for {
          first <- subscribeRequest(peer)
          _     <- peer.send(
            us,
            TransactionMessages.responseTo(first, 403, "Forbidden")
          )
          ended <- subscription.awaitActive.timeout(5.seconds)
          stray = notifyFor(first, 1, "active").copy(headers =
            notifyFor(first, 1, "active").headers
              .copy(callId = callId("nobody@nowhere"))
          )
          _      <- peer.send(us, stray)
          answer <- answerTo(peer, stray)
        } yield {
          assertEquals(ended, Left(Ending.Rejected(403, "Forbidden")))
          assertEquals(answer.statusCode, 481)
        }
      }
    }
  }

  test("a BYE for a call that does not exist is answered 481") {
    (phone, remote).tupled.use { (_, peer) =>
      val stray = SipRequest(
        SipMethod.Bye,
        uri("sip:test@127.0.0.1:19070"),
        SipHeaders(
          callId = callId("nobody@nowhere"),
          from = nameAddr("<sip:bob@127.0.0.1>;tag=r1"),
          to = nameAddr("<sip:test@127.0.0.1>;tag=l1"),
          vias = NonEmptyList.one(
            via("SIP/2.0/UDP 127.0.0.1:19071;branch=z9hG4bKstray")
          ),
          cseq = cseq(1, SipMethod.Bye)
        )
      )
      for {
        _      <- peer.send(us, stray)
        answer <- peer.messages
          .collectFirst { case Inbound(_, r: SipResponse) => r }
          .compile
          .lastOrError
          .timeout(5.seconds)
      } yield assertEquals(answer.statusCode, 481)
    }
  }

  test("registering over UDP binds the account and unregistering removes it") {
    (phone, remote).tupled.use { (softphone, peer) =>
      def registerOf(peer: Transport[IO]) = requestOf(peer, SipMethod.Register)
      def answer(request: SipRequest, granted: Int) = {
        val base =
          TransactionMessages.responseTo(request, 200, "OK", Some(tag("srv")))
        peer.send(
          us,
          base.copy(headers =
            base.headers.copy(contacts =
              request.headers.contacts.map(c =>
                c.withExpires(Expires(granted).fold(err => fail(err), identity))
              )
            )
          )
        )
      }
      softphone.register.allocated.flatMap { case (registration, release) =>
        for {
          first     <- registerOf(peer)
          _         <- answer(first, 3600)
          _         <- registration.awaitRegistered.timeout(5.seconds)
          state     <- registration.state.get
          releasing <- release.start
          removal   <- registerOf(peer)
          _         <- answer(removal, 0)
          _         <- releasing.join
        } yield {
          assertEquals(first.requestUri.render, "sip:127.0.0.1")
          assertEquals(
            first.headers.expires,
            Some(Expires(3600).fold(err => fail(err), identity))
          )
          assertEquals(
            state,
            softphone4s.registration.RegistrationState.Registered(3600.seconds)
          )
          assertEquals(removal.headers.expires, Some(Expires.Zero))
          assertEquals(removal.headers.callId, first.headers.callId)
        }
      }
    }
  }

  test("a registration's states, and its end, are reported to the metrics") {
    RecordingMetrics().flatMap { metrics =>
      val measured =
        SoftphoneStreaming.resource[IO](
          config,
          audio,
          NoOpLogger.impl[IO],
          metrics = Some(metrics)
        )
      (measured, remote).tupled.use { (softphone, peer) =>
        def answer(request: SipRequest, granted: Int) = {
          val base =
            TransactionMessages.responseTo(request, 200, "OK", Some(tag("srv")))
          peer.send(
            us,
            base.copy(headers =
              base.headers.copy(contacts =
                request.headers.contacts.map(c =>
                  c.withExpires(
                    Expires(granted).fold(err => fail(err), identity)
                  )
                )
              )
            )
          )
        }
        softphone.register.allocated.flatMap { case (registration, release) =>
          for {
            first     <- requestOf(peer, SipMethod.Register)
            _         <- answer(first, 3600)
            _         <- registration.awaitRegistered.timeout(5.seconds)
            releasing <- release.start
            removal   <- requestOf(peer, SipMethod.Register)
            _         <- answer(removal, 0)
            _         <- releasing.join
            seen      <- metrics.recorded
          } yield {
            val states = seen.collect {
              case RecordingMetrics.Recorded.Registration(account, state) =>
                (account, state)
            }
            val account = softphone.accounts.head
            assert(states.forall(_._1 == account), states.toString)
            assertEquals(
              states.map(_._2).filterNot(_ == RegistrationState.Unregistering),
              Vector(
                RegistrationState.Unregistered,
                RegistrationState.Registering,
                RegistrationState.Registered(3600.seconds),
                RegistrationState.Unregistered
              )
            )
          }
        }
      }
    }
  }

  test("no codec configured is an error before anything is bound") {
    SoftphoneStreaming
      .resource[IO](config.copy(codecs = Nil), audio, NoOpLogger.impl[IO])
      .use_
      .attempt
      .map(r =>
        assertEquals(r.left.toOption, Some(SoftphoneError.NoCodecsConfigured))
      )
  }

  test("a codec nobody supplied is refused when the softphone starts") {
    SoftphoneStreaming
      .resource[IO](
        config.copy(codecs = List(CodecChoice.Named("opus"))),
        audio,
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map(r =>
        assertEquals(r.left.toOption, Some(SoftphoneError.UnknownCodec("opus")))
      )
  }

  test("a codec the program supplies is accepted, and listed as it chooses") {
    SoftphoneStreaming
      .resource[IO](
        config
          .copy(codecs = List(CodecChoice.Named("x-half"), CodecChoice.Pcmu)),
        audio,
        NoOpLogger.impl[IO],
        List(HalfByteCodec)
      )
      .use_
  }

  test("G.722 beside G.711 is refused: one audio format cannot serve both") {
    SoftphoneStreaming
      .resource[IO](
        config.copy(codecs = List(CodecChoice.G722, CodecChoice.Pcmu)),
        audio,
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map(r =>
        assertEquals(r.left.toOption, Some(SoftphoneError.MixedCodecRates))
      )
  }

  test("G.722 alone starts on a 16 kHz audio format") {
    SoftphoneStreaming
      .resource[IO](
        config.copy(codecs = List(CodecChoice.G722)),
        new AudioFormat(16000f, 16, 1, true, false),
        NoOpLogger.impl[IO]
      )
      .use_
  }

  test("codecs that work at another rate than the audio format are refused") {
    SoftphoneStreaming
      .resource[IO](
        config,
        new AudioFormat(16000f, 16, 1, true, false),
        NoOpLogger.impl[IO]
      )
      .use_
      .attempt
      .map(r =>
        assertEquals(
          r.left.toOption,
          Some(SoftphoneError.AudioFormatMismatch(8000, 16000))
        )
      )
  }
}
