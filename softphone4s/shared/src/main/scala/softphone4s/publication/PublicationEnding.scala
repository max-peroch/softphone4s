package softphone4s.publication

/** How a publication ended. */
enum PublicationEnding {

  /** The program removed it, or it was removed because the program asked. */
  case Removed

  /** Refused for good: trying again will not help. */
  case Rejected(status: Int, reason: String)

  /** Could not be had, where trying again later may help. */
  case Failed(failure: PublicationFailure)
}
