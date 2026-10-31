package softphone4s.media

import fs2.Chunk

/** What to play for one frame of 20 ms. */
enum Playout {

  /** The payload of the next packet. */
  case Frame(payload: Chunk[Byte])

  /** The next packet did not come, and later ones did: it is lost. Play
    * concealment for it.
    */
  case Lost

  /** Nothing is held: playing has not begun, or the buffer ran dry. Silence, or
    * comfort noise.
    */
  case Waiting
}
