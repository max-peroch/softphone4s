package softphone4s.fsm

import munit.FunSuite
import softphone4s.transaction.TransactionConfig

import scala.concurrent.duration.*

class OkRetransmissionSuite extends FunSuite {

  test("a 2xx is repeated after T1, then at doubling intervals capped at T2") {
    val delays = OkRetransmission.delays(TransactionConfig())
    assertEquals(
      delays.take(6),
      List(500.millis, 1.second, 2.seconds, 4.seconds, 4.seconds, 4.seconds)
    )
  }

  test("the repeats stop within 64 * T1") {
    val config = TransactionConfig()
    val delays = OkRetransmission.delays(config)
    assertEquals(delays.size, 10)
    assert(
      delays.foldLeft(Duration.Zero: FiniteDuration)(
        _ + _
      ) <= config.transactionTimeout
    )
    assert(delays.last <= config.t2)
  }

  test("other timer values give another schedule") {
    val config = TransactionConfig(t1 = 100.millis, t2 = 400.millis)
    val delays = OkRetransmission.delays(config)
    assertEquals(delays.take(3), List(100.millis, 200.millis, 400.millis))
    assert(
      delays.foldLeft(Duration.Zero: FiniteDuration)(
        _ + _
      ) <= config.transactionTimeout
    )
  }
}
