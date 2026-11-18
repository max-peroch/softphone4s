package softphone4s.srtp

import softphone4s.model.Ssrc

/** Why a key could not be made, or a packet could not be protected or opened.
  */
enum SrtpError {
  case BadKeyLength(expected: Int, actual: Int)
  case BadSaltLength(expected: Int, actual: Int)

  /** Not an RTP packet: too short, or not version 2. */
  case NotRtp

  /** Shorter than its header and authentication tag. */
  case TooShort(size: Int)

  /** The tag is not the one the key and the packet make. */
  case AuthenticationFailed

  /** A packet with this index has been opened already. */
  case Replayed

  /** Further behind the newest than the replay window reaches. */
  case TooOld

  /** From another source than the one this context keeps the count of. */
  case SourceChanged(ssrc: Ssrc)

  def message: String = this match {
    case BadKeyLength(expected, actual) =>
      s"a master key is $expected bytes, not $actual"
    case BadSaltLength(expected, actual) =>
      s"a master salt is $expected bytes, not $actual"
    case NotRtp                => "not an RTP packet"
    case TooShort(size)        => s"a packet of $size bytes is too short"
    case AuthenticationFailed  => "authentication failed"
    case Replayed              => "packet replayed"
    case TooOld                => "packet too old for the replay window"
    case SourceChanged(source) => s"packet from another source, $source"
  }
}
