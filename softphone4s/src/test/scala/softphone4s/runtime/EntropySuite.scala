package softphone4s.runtime

import cats.effect.IO
import cats.effect.std.Random
import cats.syntax.all.*
import com.comcast.ip4s.*
import munit.CatsEffectSuite

class EntropySuite extends CatsEffectSuite {

  private def seeded(seed: Long): IO[Entropy[IO]] =
    Random
      .javaUtilRandom[IO](new java.util.Random(seed))
      .map(Entropy.fromRandom(_))

  test("the same seed gives the same identifiers") {
    for {
      a  <- seeded(7)
      b  <- seeded(7)
      xs <- (a.tag, a.branch, a.callId(ip"10.0.0.1"), a.cnonce).tupled
      ys <- (b.tag, b.branch, b.callId(ip"10.0.0.1"), b.cnonce).tupled
    } yield assertEquals(xs, ys)
  }

  test("identifiers have the expected shape") {
    seeded(1).flatMap { e =>
      (e.tag, e.branch, e.callId(host"example.com"), e.cnonce).tupled.map {
        (tag, branch, callId, cnonce) =>
          assertEquals(tag.value.length, 8)
          assert(branch.isRfc3261)
          assertEquals(branch.value.length, "z9hG4bK".length + 16)
          assert(callId.value.endsWith("@example.com"), callId.value)
          assertEquals(cnonce.value.length, 16)
      }
    }
  }

  test("successive identifiers differ") {
    seeded(1).flatMap { e =>
      e.branch.replicateA(50).map(bs => assertEquals(bs.distinct.size, 50))
    }
  }

  test("the secure instance produces valid identifiers") {
    Entropy.secure[IO].flatMap(_.branch).map(b => assert(b.isRfc3261))
  }

  test("fractions are in [0, 1), differ, and repeat for the same seed") {
    for {
      a  <- seeded(3)
      b  <- seeded(3)
      xs <- a.fraction.replicateA(20)
      ys <- b.fraction.replicateA(20)
    } yield {
      assert(xs.forall(f => f >= 0.0 && f < 1.0))
      assert(xs.distinct.size > 10)
      assertEquals(xs, ys)
    }
  }
}
