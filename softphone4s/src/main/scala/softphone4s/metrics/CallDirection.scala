package softphone4s.metrics

/** Which way a call was made. */
enum CallDirection {

  /** We dialled. */
  case Outgoing

  /** It was made to us. */
  case Incoming
}
