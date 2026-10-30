package softphone4s.fsm

import scala.concurrent.duration.FiniteDuration

/** A timer request emitted by a pure state machine and run by
  * [[softphone4s.runtime.Timers]]. Starting a timer that is already running
  * restarts it.
  */
enum TimerAction[+Id] {
  case Start(id: Id, after: FiniteDuration)
  case Cancel(id: Id)
}
