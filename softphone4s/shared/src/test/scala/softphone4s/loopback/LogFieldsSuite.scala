package softphone4s.loopback

import cats.effect.IO
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite
import org.typelevel.log4cats.testing.StructuredTestingLogger
import org.typelevel.log4cats.testing.StructuredTestingLogger.LogMessage
import softphone4s.logging.LogFields

import scala.concurrent.duration.*

/** What the stack's log lines say about the call they are about. */
class LogFieldsSuite extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 2.minutes

  private val frame: Chunk[Byte] =
    Chunk.array(
      Array.tabulate[Byte](320)(i => if i % 2 == 0 then 0x40 else 0x1f)
    )

  private def recording: IO[StructuredTestingLogger[IO]] =
    StructuredTestingLogger.ref[IO](true, true, true, true, true)

  /** A call that is answered, spoken in for a moment, and hung up by the
    * caller; and what each phone logged.
    */
  private def oneCall: IO[(Vector[LogMessage], Vector[LogMessage])] =
    for {
      alice <- recording
      bob   <- recording
      _     <- Loopback.pair(aliceLogger = alice, bobLogger = bob).use { p =>
        p.alice.call(p.callee()).use { call =>
          for {
            incoming <- p.bob.incoming.head.compile.lastOrError
              .timeout(10.seconds)
            _ <- incoming.accept.use { bobCall =>
              (call.awaitPickup, bobCall.awaitPickup).parTupled
                .timeout(10.seconds) >>
                Stream
                  .fixedRate[IO](20.millis)
                  .flatMap(_ => Stream.chunk(frame))
                  .through(call.sink)
                  .interruptAfter(500.millis)
                  .compile
                  .drain
            }
          } yield ()
        }
      }
      _ <- IO.sleep(500.millis)
      a <- alice.logged
      b <- bob.logged
    } yield (a, b)

  private def callIdsOf(lines: Vector[LogMessage]): Set[String] =
    lines.flatMap(_.ctx.get(LogFields.CallId)).toSet

  test(
    "every line a call logs says which call it is, which way, and for which account"
  ) {
    oneCall.map { (alice, bob) =>
      val outgoing =
        alice.filter(_.ctx.get(LogFields.Direction).contains("outgoing"))
      val incoming =
        bob.filter(_.ctx.get(LogFields.Direction).contains("incoming"))
      assert(outgoing.nonEmpty, "alice logged nothing about the call")
      assert(incoming.nonEmpty, "bob logged nothing about the call")
      // a line of a call has all three, never one without the others
      (outgoing ++ incoming).foreach { line =>
        assert(line.ctx.contains(LogFields.CallId), line.toString)
        assert(line.ctx.contains(LogFields.Account), line.toString)
      }
      assert(
        outgoing.forall(_.ctx(LogFields.Account) == "sip:alice@127.0.0.1"),
        outgoing.toString
      )
      assert(
        incoming.forall(_.ctx(LogFields.Account) == "sip:bob@127.0.0.1"),
        incoming.toString
      )
    }
  }

  test(
    "both phones log the same call-id for the same call, so their logs can be joined"
  ) {
    oneCall.map { (alice, bob) =>
      val aliceIds = callIdsOf(
        alice.filter(_.ctx.get(LogFields.Direction).contains("outgoing"))
      )
      val bobIds = callIdsOf(
        bob.filter(_.ctx.get(LogFields.Direction).contains("incoming"))
      )
      assertEquals(aliceIds.size, 1)
      assertEquals(aliceIds, bobIds)
    }
  }

  test(
    "the fields are named as documented: no old-style keys, no call-id in two spellings"
  ) {
    oneCall.map { (alice, bob) =>
      val keys = (alice ++ bob).flatMap(_.ctx.keys).toSet
      assert(!keys.contains("callId"), keys.toString)
      assert(!keys.contains("statusCode"), keys.toString)
      assert(keys.contains(LogFields.CallId), keys.toString)
      assert(keys.contains(LogFields.Direction), keys.toString)
    }
  }
}
