package softphone4s.media

/** What to play for one frame of 20 ms. */
enum Playout {

  /** The next packet. */
  case Frame(packet: JitterPacket)

  /** The next packet did not come, and later ones did: it is lost. Play
    * concealment for it.
    */
  case Lost

  /** Nothing is held: playing has not begun, or the buffer ran dry. Silence, or
    * comfort noise.
    */
  case Waiting
}
