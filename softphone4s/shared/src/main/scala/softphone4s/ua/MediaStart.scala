package softphone4s.ua

/** What starting media came to. */
private[softphone4s] enum MediaStart {

  /** It was not running and is now. */
  case Started

  /** It was running already, from early media, and now follows the new answer's
    * address and direction.
    */
  case Followed

  /** As [[Followed]], but the new answer chose another codec, which cannot be
    * switched to in mid-stream: the early one is kept.
    */
  case CodecChanged
}
