package softphone4s.metrics

/** How a call ended. */
enum CallOutcome {

  /** It was answered, and it lasted until one side hung up. */
  case Completed

  /** The other side refused it with a final status. */
  case Rejected(status: Int)

  /** We turned an incoming call away. */
  case Declined

  /** Nobody answered: it was cancelled, or it timed out. */
  case Unanswered

  /** It could not be had: the transport failed, or something else went wrong.
    */
  case Failed
}
