package softphone4s.transaction

import cats.effect.IO
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SipLiterals.*
import softphone4s.model.*
import softphone4s.transaction.TransactionExamples.*
import softphone4s.transport.InMemoryTransport

import scala.concurrent.duration.*

class TransactionManagerSuite extends CatsEffectSuite {

  private val a = InMemoryTransport.AddressA
  private val b = InMemoryTransport.AddressB

  private def isRequest(m: SipMessage, method: SipMethod): Boolean =
    m match {
      case r: SipRequest => r.method == method
      case _             => false
    }

  private def isStatus(m: SipMessage, code: Int): Boolean =
    m match {
      case r: SipResponse => r.statusCode == code
      case _              => false
    }

  /** Two layers over a connected in-memory pair; `dropA` and `dropB` lose
    * messages sent by A and B. `withB = false` leaves B without a layer, so
    * nothing ever answers A.
    */
  private def peers(
      dropA: (Int, SipMessage) => Boolean = (_, _) => false,
      dropB: (Int, SipMessage) => Boolean = (_, _) => false,
      withB: Boolean = true
  ) =
    for {
      pair <- cats.effect.Resource.eval(InMemoryTransport.pair[IO])
      ta   <- cats.effect.Resource.eval(RecordingTransport(pair._1, dropA))
      tb   <- cats.effect.Resource.eval(RecordingTransport(pair._2, dropB))
      la   <- TransactionManager.resource(
        ta,
        TransactionConfig(),
        NoOpLogger.impl[IO]
      )
      lb <-
        if withB then
          TransactionManager
            .resource(
              tb,
              TransactionConfig(),
              NoOpLogger.impl[IO]
            )
            .map(Some(_))
        else cats.effect.Resource.pure[IO, Option[TransactionLayer[IO]]](None)
    } yield (la, lb, ta, tb)

  private def client(
      layer: TransactionLayer[IO],
      request: SipRequest,
      to: com.comcast.ip4s.SocketAddress[com.comcast.ip4s.IpAddress]
  ) =
    layer.client(request, to).flatMap {
      case Right(t)  => IO.pure(t)
      case Left(err) => IO.raiseError(new AssertionError(err.message))
    }

  private def reply(st: ServerTransaction[IO], code: Int) =
    st.respond(
      TransactionMessages.responseTo(
        st.request,
        code,
        "Reason",
        Some(tag("b1"))
      )
    )

  private def secondsOf(
      log: Vector[(FiniteDuration, SipMessage)],
      p: SipMessage => Boolean
  ) =
    log.collect { case (t, m) if p(m) => t.toMillis / 1000.0 }.toList

  // ---- INVITE ------------------------------------------------------------------

  test(
    "INVITE answered with a failure: ACKed by the transaction, both sides finish"
  ) {
    TestControl.executeEmbed(
      peers().use { (la, lb, ta, _) =>
        for {
          c    <- client(la, invite, b)
          st   <- lb.get.incoming.head.compile.lastOrError
          _    <- reply(st, 486)
          cn   <- c.notices.compile.toList
          sn   <- st.notices.compile.toList
          sent <- ta.log
        } yield {
          assertEquals(
            cn.map {
              case ClientNotice.Response(r) => r.statusCode; case _ => -1
            },
            List(100, 486)
          )
          assertEquals(sn, Nil)
          assertEquals(
            sent.map(_._2).collect { case r: SipRequest => r.method },
            Vector(SipMethod.Invite, SipMethod.Ack)
          )
        }
      }
    )
  }

  test(
    "100 Trying, 180 and 200: all reach the client and the 2xx has no ACK from the transaction"
  ) {
    TestControl.executeEmbed(
      peers().use { (la, lb, ta, _) =>
        for {
          c    <- client(la, invite, b)
          st   <- lb.get.incoming.head.compile.lastOrError
          _    <- reply(st, 180) >> reply(st, 200)
          cn   <- c.notices.compile.toList
          sent <- ta.log
        } yield {
          assertEquals(
            cn.collect { case ClientNotice.Response(r) => r.statusCode },
            List(100, 180, 200)
          )
          assertEquals(
            sent.map(_._2).collect { case r: SipRequest => r.method },
            Vector(SipMethod.Invite)
          )
        }
      }
    )
  }

  test("a lost INVITE is retransmitted after T1 and then answered") {
    TestControl.executeEmbed(
      peers(dropA = (n, m) => n == 0 && isRequest(m, SipMethod.Invite)).use {
        (la, lb, ta, _) =>
          for {
            _    <- client(la, invite, b)
            st   <- lb.get.incoming.head.compile.lastOrError
            now  <- IO.monotonic
            sent <- ta.log
          } yield {
            assertEquals(now, 500.millis)
            assertEquals(
              secondsOf(sent, isRequest(_, SipMethod.Invite)),
              List(0.0, 0.5)
            )
            assertEquals(st.request.method, SipMethod.Invite)
          }
      }
    )
  }

  test(
    "an INVITE nobody answers is retransmitted at 0.5, 1.5, 3.5, 7.5, 15.5 and 31.5 s, then times out at 32 s"
  ) {
    TestControl.executeEmbed(
      peers(withB = false).use { (la, _, ta, _) =>
        for {
          c    <- client(la, invite, b)
          cn   <- c.notices.compile.toList
          now  <- IO.monotonic
          sent <- ta.log
        } yield {
          assertEquals(cn, List(ClientNotice.TimedOut))
          assertEquals(now, 32.seconds)
          assertEquals(
            secondsOf(sent, isRequest(_, SipMethod.Invite)),
            List(0.0, 0.5, 1.5, 3.5, 7.5, 15.5, 31.5)
          )
        }
      }
    )
  }

  test(
    "a lost failure response is sent again when the INVITE is retransmitted"
  ) {
    TestControl.executeEmbed(
      // B's sends are 0: the 100 Trying, 1: the 486, which is lost.
      peers(dropB = (n, m) => n == 1 && isStatus(m, 486)).use {
        (la, lb, _, tb) =>
          for {
            c    <- client(la, invite, b)
            st   <- lb.get.incoming.head.compile.lastOrError
            _    <- reply(st, 486)
            cn   <- c.notices.compile.toList
            sent <- tb.log
          } yield {
            assertEquals(
              cn.collect { case ClientNotice.Response(r) => r.statusCode },
              List(100, 486)
            )
            assertEquals(sent.count((_, m) => isStatus(m, 486)), 2)
          }
      }
    )
  }

  test(
    "a failure response is retransmitted until its ACK; with every ACK lost the server gives up at 32 s"
  ) {
    TestControl.executeEmbed(
      peers(dropA = (_, m) => isRequest(m, SipMethod.Ack)).use {
        (la, lb, _, tb) =>
          for {
            _    <- client(la, invite, b)
            st   <- lb.get.incoming.head.compile.lastOrError
            _    <- reply(st, 486)
            sn   <- st.notices.compile.toList
            sent <- tb.log
          } yield {
            assertEquals(sn, List(ServerNotice.AckTimedOut))
            assertEquals(
              secondsOf(sent, isStatus(_, 486)),
              List(0.0, 0.5, 1.5, 3.5, 7.5, 11.5, 15.5, 19.5, 23.5, 27.5, 31.5)
            )
          }
      }
    )
  }

  test(
    "a retransmitted INVITE gets the last response again and reaches the user once"
  ) {
    TestControl.executeEmbed(
      peers().use { (la, lb, _, tb) =>
        for {
          _     <- client(la, invite, b)
          st    <- lb.get.incoming.head.compile.lastOrError
          _     <- la.transmit(invite, b) // the same branch again
          _     <- IO.sleep(100.millis)
          sent  <- tb.log
          extra <- lb.get.incoming.interruptAfter(1.second).compile.toList
        } yield {
          assertEquals(extra, Nil)
          assertEquals(sent.count((_, m) => isStatus(m, 100)), 2)
          assertEquals(st.request, invite)
        }
      }
    )
  }

  test(
    "the ACK of a 2xx and a retransmitted 2xx are handed to the user agent"
  ) {
    TestControl.executeEmbed(
      peers().use { (la, lb, _, _) =>
        val ok = response(invite, 200)
        for {
          c     <- client(la, invite, b)
          st    <- lb.get.incoming.head.compile.lastOrError
          _     <- reply(st, 200)
          _     <- c.notices.compile.toList // the transaction has ended
          _     <- la.transmit(request(SipMethod.Ack), b)
          ack   <- lb.get.unmatched.head.compile.lastOrError
          _     <- lb.get.transmit(ok, a)   // the server's 2xx retransmission
          again <- la.unmatched.head.compile.lastOrError
        } yield {
          assert(isRequest(ack.message, SipMethod.Ack))
          assert(isStatus(again.message, 200))
        }
      }
    )
  }

  // ---- non-INVITE ----------------------------------------------------------------

  test("a non-INVITE request is answered and both sides clean up") {
    TestControl.executeEmbed(
      peers().use { (la, lb, _, _) =>
        for {
          c   <- client(la, options, b)
          st  <- lb.get.incoming.head.compile.lastOrError
          _   <- reply(st, 200)
          cn  <- c.notices.compile.toList
          now <- IO.monotonic
        } yield {
          assertEquals(
            cn.collect { case ClientNotice.Response(r) => r.statusCode },
            List(200)
          )
          assertEquals(now, 5.seconds) // Timer K
        }
      }
    )
  }

  test(
    "a non-INVITE nobody answers is retransmitted at the T2 cap and times out at 32 s"
  ) {
    TestControl.executeEmbed(
      peers(withB = false).use { (la, _, ta, _) =>
        for {
          c    <- client(la, options, b)
          cn   <- c.notices.compile.toList
          sent <- ta.log
        } yield {
          assertEquals(cn, List(ClientNotice.TimedOut))
          assertEquals(
            secondsOf(sent, isRequest(_, SipMethod.Options)),
            List(0.0, 0.5, 1.5, 3.5, 7.5, 11.5, 15.5, 19.5, 23.5, 27.5, 31.5)
          )
        }
      }
    )
  }

  test(
    "a lost final response to a non-INVITE is repeated when the request is retransmitted"
  ) {
    TestControl.executeEmbed(
      peers(dropB = (n, m) => isStatus(m, 200) && n == 0).use {
        (la, lb, _, _) =>
          for {
            c   <- client(la, options, b)
            st  <- lb.get.incoming.head.compile.lastOrError
            _   <- reply(st, 200)
            cn  <- c.notices.compile.toList
            now <- IO.monotonic
          } yield {
            assertEquals(
              cn.collect { case ClientNotice.Response(r) => r.statusCode },
              List(200)
            )
            assert(now >= 500.millis)
          }
      }
    )
  }

  // ---- errors and edges -------------------------------------------------------------

  test("starting a second client transaction with the same key is refused") {
    TestControl.executeEmbed(
      peers(withB = false).use { (la, _, _, _) =>
        for {
          _      <- client(la, invite, b)
          second <- la.client(invite, b)
        } yield second match {
          case Left(TransactionError.Duplicate(key)) =>
            assertEquals(
              key,
              ClientKey(branch("z9hG4bKtxn1"), SipMethod.Invite)
            )
          case other => fail(s"unexpected: $other")
        }
      }
    )
  }

  test(
    "an ACK, or a request without an RFC 3261 branch, cannot start a transaction"
  ) {
    TestControl.executeEmbed(
      peers(withB = false).use { (la, _, _, _) =>
        for {
          ack <- la.client(request(SipMethod.Ack), b)
          old <- la.client(request(SipMethod.Invite, "oldstyle"), b)
        } yield {
          assertEquals(
            ack,
            Left(TransactionError.NoTransactionFor(SipMethod.Ack))
          )
          old match {
            case Left(
                  TransactionError.InvalidKey(
                    TransactionKeyError.Rfc2543Branch(_)
                  )
                ) =>
              ()
            case other => fail(s"unexpected: $other")
          }
        }
      }
    )
  }

  test(
    "a response to a transaction that has already ended is dropped quietly"
  ) {
    TestControl.executeEmbed(
      peers().use { (la, lb, _, _) =>
        for {
          _  <- client(la, options, b)
          st <- lb.get.incoming.head.compile.lastOrError
          _  <- reply(st, 200)
          _  <- IO.sleep(40.seconds) // Timer J has ended the server transaction
          _  <- reply(st, 500)
          _  <- IO.sleep(1.second)
        } yield ()
      }
    )
  }

  test("a started transaction can be reused after it ends") {
    TestControl.executeEmbed(
      peers().use { (la, lb, _, _) =>
        for {
          c1  <- client(la, options, b)
          st1 <- lb.get.incoming.head.compile.lastOrError
          _   <- reply(st1, 200)
          _   <- c1.notices.compile.toList
          _   <- IO.sleep(40.seconds)
          _   <- client(la, options, b) // same branch, after both ended
          st2 <- lb.get.incoming.head.compile.lastOrError
        } yield assertEquals(st2.request, options)
      }
    )
  }
}
