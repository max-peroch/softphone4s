package softphone4s.transaction

import scala.concurrent.duration.*

/** The timer base values of RFC 3261 §17 (T1 is the round-trip estimate, T2 the
  * retransmit cap, T4 how long a message may linger in the network) and the
  * durations derived from them.
  */
final case class TransactionConfig(
    t1: FiniteDuration = 500.millis,
    t2: FiniteDuration = 4.seconds,
    t4: FiniteDuration = 5.seconds
) {

  /** Timers B, F, H and J: 64 * T1. */
  def transactionTimeout: FiniteDuration = t1 * 64

  /** Timer D: how long a client keeps absorbing retransmitted final responses;
    * at least 32 s over an unreliable transport.
    */
  def timerD: FiniteDuration = transactionTimeout.max(32.seconds)

  /** The next retransmit interval after `current`, doubling and capped at T2
    * (Timers E and G; Timer A doubles without a cap).
    */
  def nextCapped(current: FiniteDuration): FiniteDuration =
    (current * 2).min(t2)
}
