package softphone4s.fsm

import softphone4s.transaction.TransactionConfig

import scala.annotation.tailrec
import scala.concurrent.duration.*

/** How a 2xx to an INVITE is repeated until its ACK arrives (RFC 3261
  * §13.3.1.4): after T1, then at doubling intervals capped at T2, for no longer
  * than 64 * T1 in all.
  */
object OkRetransmission {

  /** The waits before each repeat; they add up to at most `64 * T1`. */
  def delays(config: TransactionConfig): List[FiniteDuration] = {
    @tailrec
    def loop(
        next: FiniteDuration,
        elapsed: FiniteDuration,
        acc: List[FiniteDuration]
    ): List[FiniteDuration] =
      if elapsed + next > config.transactionTimeout then acc.reverse
      else loop(config.nextCapped(next), elapsed + next, next :: acc)
    loop(config.t1, Duration.Zero, Nil)
  }
}
