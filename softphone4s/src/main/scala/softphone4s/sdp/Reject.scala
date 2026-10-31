package softphone4s.sdp

/** Why a description cannot be accepted. */
enum Reject {

  /** No audio stream we can address: none at all, or declined, or one that is
    * not plain RTP.
    */
  case NoAudio

  /** Audio, but none of the codecs we can use. */
  case NoCommonCodec

  /** The stream has no address to send to. */
  case NoAddress

  def message: String = this match {
    case NoAudio       => "no audio stream we can use"
    case NoCommonCodec => "no codec in common"
    case NoAddress     => "no address to send media to"
  }
}
