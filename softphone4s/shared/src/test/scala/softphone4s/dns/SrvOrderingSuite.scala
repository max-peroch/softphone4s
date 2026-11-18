package softphone4s.dns

import com.comcast.ip4s.*
import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll

class SrvOrderingSuite extends ScalaCheckSuite {

  private def srv(priority: Int, weight: Int, target: String): SrvRecord =
    SrvRecord(
      priority,
      weight,
      port"5060",
      Hostname.fromString(target).getOrElse(fail(s"bad host $target"))
    )

  private def names(records: List[SrvRecord]) = records.map(_.target.toString)

  test("lower priority numbers come first") {
    val records =
      List(srv(20, 1, "c"), srv(10, 1, "a"), srv(30, 1, "d"), srv(10, 1, "b"))
    val ordered = SrvOrdering.order(records, List.fill(4)(0.0))
    assertEquals(ordered.map(_.priority), List(10, 10, 20, 30))
    assertEquals(names(ordered).take(2).toSet, Set("a", "b"))
  }

  test("a lone record is returned as it is") {
    val only = srv(5, 0, "only")
    assertEquals(SrvOrdering.order(List(only), Nil), List(only))
  }

  test("nothing in, nothing out") {
    assertEquals(SrvOrdering.order(Nil, List(0.5)), Nil)
  }

  test("the same random numbers give the same order") {
    val records = List(srv(1, 10, "a"), srv(1, 20, "b"), srv(1, 30, "c"))
    val draws   = List(0.7, 0.2, 0.9)
    assertEquals(
      SrvOrdering.order(records, draws),
      SrvOrdering.order(records, draws)
    )
  }

  test(
    "a draw of 0 picks in listed order, with weight-0 records first (RFC 2782)"
  ) {
    val records = List(srv(1, 5, "heavy"), srv(1, 0, "zero"))
    assertEquals(
      names(SrvOrdering.order(records, List(0.0, 0.0))),
      List("zero", "heavy")
    )
  }

  test("a draw near 1 picks the last of the weighted records first") {
    val records = List(srv(1, 1, "a"), srv(1, 1, "b"), srv(1, 1, "c"))
    assertEquals(
      names(SrvOrdering.order(records, List(0.999, 0.0, 0.0))).head,
      "c"
    )
  }

  test("too few draws are treated as 0, not an error") {
    val records = List(srv(1, 1, "a"), srv(1, 1, "b"))
    assertEquals(SrvOrdering.order(records, Nil).size, 2)
  }

  test("over many draws the first choice follows the weights") {
    val records  = List(srv(1, 90, "big"), srv(1, 10, "small"))
    val n        = 4000
    val bigFirst = (0 until n).count { i =>
      names(SrvOrdering.order(records, List((i + 0.5) / n, 0.0))).head == "big"
    }
    val share = bigFirst.toDouble / n
    assert(
      share > 0.85 && share < 0.95,
      s"big came first in $share of the draws"
    )
  }

  private val recordGen: Gen[SrvRecord] = for {
    priority <- Gen.choose(0, 5)
    weight   <- Gen.choose(0, 100)
    port     <- Gen.choose(1, 65535)
    target   <- Gen.alphaLowerStr
      .suchThat(_.nonEmpty)
      .map(s => s"${s.take(8)}.example.com")
  } yield SrvRecord(
    priority,
    weight,
    Port.fromInt(port).get,
    Hostname.fromString(target).get
  )

  property("every record comes back exactly once, in non-decreasing priority") {
    forAll(Gen.listOf(recordGen), Gen.listOf(Gen.choose(0.0, 1.0))) {
      (records, draws) =>
        val ordered = SrvOrdering.order(records, draws)
        Prop(ordered.sortBy(_.toString) == records.sortBy(_.toString)) &&
        Prop(ordered.map(_.priority) == ordered.map(_.priority).sorted)
    }
  }
}
