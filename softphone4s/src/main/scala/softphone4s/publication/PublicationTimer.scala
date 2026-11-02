package softphone4s.publication

enum PublicationTimer {

  /** Time to refresh the state, or to send the change that is owed. */
  case Refresh

  /** The state has lapsed at the notifier. */
  case Expiry
}
