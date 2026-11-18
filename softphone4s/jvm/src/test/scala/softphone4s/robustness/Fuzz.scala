package softphone4s.robustness

import org.scalacheck.{Arbitrary, Gen, Prop}

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.util.concurrent.{
  Callable,
  ExecutorService,
  Executors,
  TimeUnit,
  TimeoutException
}

/** What the fuzz suites are made of: inputs that are random or are real ones
  * damaged, and the check that a decoder survives them.
  */
object Fuzz {

  type Bytes = Array[Byte]

  /** The longest a decoder may take on one input before it counts as hung. */
  val Budget: Long = 500L

  /** Bytes of every value, from none to a few hundred. */
  val randomBytes: Gen[Bytes] =
    for {
      size <- Gen.frequency(
        1 -> Gen.const(0),
        8 -> Gen.choose(1, 64),
        4 -> Gen.choose(65, 400)
      )
      bytes <- Gen.listOfN(size, Arbitrary.arbitrary[Byte])
    } yield bytes.toArray

  /** Text made of the characters protocols are made of, which is where parsers
    * go wrong, with a few that are not.
    */
  val protocolText: Gen[String] =
    Gen
      .listOf(
        Gen.frequency(
          6 -> Gen.alphaNumChar,
          4 -> Gen.oneOf("<>;:=\"@[]()/,\\ \t\r\n.-_+%*!~'&$?#{}|^`".toList),
          1 -> Gen.oneOf('\u0000', '\u0001', '\u007f', '\u0080', 'ÿ', 'Ā', ' ',
            '\ud800', '\udfff', '￿')
        )
      )
      .map(_.mkString)

  /** Any text there is, surrogates included. */
  val anyText: Gen[String] = Arbitrary.arbitrary[String]

  /** One damage done to `bytes`. */
  private def damage(bytes: Bytes): Gen[Bytes] = {
    val n             = bytes.length
    def pos: Gen[Int] = if n == 0 then Gen.const(0) else Gen.choose(0, n - 1)
    Gen.oneOf(
      // flip a bit
      pos.flatMap(i =>
        Gen
          .choose(0, 7)
          .map(b =>
            if n == 0 then bytes
            else bytes.updated(i, (bytes(i) ^ (1 << b)).toByte)
          )
      ),
      // another value
      pos.flatMap(i =>
        Arbitrary
          .arbitrary[Byte]
          .map(v => if n == 0 then bytes else bytes.updated(i, v))
      ),
      // an extreme value, the kind that overflows a length
      pos.flatMap(i =>
        Gen
          .oneOf[Byte](0, -1, 127, -128, 1)
          .map(v => if n == 0 then bytes else bytes.updated(i, v))
      ),
      // cut off
      pos.map(i => bytes.take(i)),
      // a range removed
      for { a <- pos; b <- pos } yield bytes.take(math.min(a, b)) ++ bytes.drop(
        math.max(a, b)
      ),
      // a range repeated
      for { a <- pos; b <- pos; times <- Gen.choose(2, 40) } yield {
        val (from, to) = (math.min(a, b), math.max(a, b))
        bytes
          .take(to) ++ Array.fill(times)(bytes.slice(from, to)).flatten ++ bytes
          .drop(to)
      },
      // bytes inserted
      for {
        i <- pos; extra <- Gen.listOfN(8, Arbitrary.arbitrary[Byte])
      } yield bytes.take(i) ++ extra.toArray ++ bytes.drop(i),
      // two halves swapped
      pos.map(i => bytes.drop(i) ++ bytes.take(i))
    )
  }

  /** A real input, damaged once to a few times. */
  def mutated(seeds: List[Bytes]): Gen[Bytes] =
    for {
      seed   <- Gen.oneOf(seeds)
      rounds <- Gen.choose(1, 5)
      out    <- (1 to rounds).foldLeft(Gen.const(seed))((g, _) =>
        g.flatMap(damage)
      )
    } yield out

  /** Text from `seeds`, damaged. */
  def mutatedText(seeds: List[String]): Gen[String] =
    mutated(seeds.map(_.getBytes(ISO_8859_1))).map(new String(_, ISO_8859_1))

  def hex(bytes: Bytes, limit: Int = 160): String = {
    val shown = bytes.take(limit).map(b => f"${b & 0xff}%02x").mkString
    if bytes.length > limit then s"$shown... (${bytes.length} bytes)" else shown
  }

  /** Decodes run on threads of their own, so that one that never returns can be
    * told from one that is slow. A thread that never returns keeps spinning
    * until the JVM ends: that is the bug being reported.
    */
  private val pool: ExecutorService =
    Executors.newCachedThreadPool { task =>
      val thread = new Thread(task, "fuzz-decoder")
      thread.setDaemon(true)
      thread
    }

  /** Whether `decode` of `input` returns, and soon: no exception of any kind (a
    * stack overflow included), and not longer than the budget.
    */
  def survives(name: String, shown: String, budget: Long = Budget)(
      decode: => Any
  ): Prop = {
    val start = System.nanoTime
    val task  = pool.submit(new Callable[Option[Throwable]] {
      def call(): Option[Throwable] =
        try { decode; None }
        catch { case t: Throwable => Some(t) }
    })
    try {
      // Twice the budget to return, then it is hung; between the two it is slow.
      val outcome = task.get(budget * 2, TimeUnit.MILLISECONDS)
      val ms      = (System.nanoTime - start) / 1000000
      outcome match {
        case Some(t) =>
          Prop.falsified :| s"$name threw ${t.getClass.getName}: ${t.getMessage} on $shown"
        case None if ms > budget =>
          Prop.falsified :| s"$name took $ms ms (budget $budget) on $shown"
        case None => Prop.passed
      }
    } catch {
      case _: TimeoutException =>
        task.cancel(true)
        Prop.falsified :| s"$name did not return within ${budget * 2} ms on $shown"
    }
  }
}
