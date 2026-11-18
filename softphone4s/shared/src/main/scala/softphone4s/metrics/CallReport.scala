package softphone4s.metrics

import softphone4s.media.JitterStats

import scala.concurrent.duration.FiniteDuration

/** What is known of a call when it ends.
  *
  * @param talkTime
  *   from the answer to the hang-up, for a call that was answered.
  * @param media
  *   what the jitter buffer did over the call: packets received, played, lost,
  *   late, duplicated, reordered, dropped, and how often it ran dry.
  */
final case class CallReport(
    direction: CallDirection,
    outcome: CallOutcome,
    talkTime: Option[FiniteDuration],
    media: JitterStats
)
