package softphone4s.publication

/** Why a publication could not be had, where trying again may help. */
enum PublicationFailure {
  case TimedOut
  case TransportFailed

  /** A PUBLISH answered with a status that may pass, such as 503 or 408. */
  case Refused(status: Int, reason: String)

  /** A 2xx the publisher cannot go on from: no entity tag, or one that is not a
    * token.
    */
  case BadResponse(reason: String)
}
