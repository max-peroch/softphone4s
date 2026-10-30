package softphone4s.fsm

import softphone4s.model.MediaDirection

/** A change to the session that we ask for with a re-INVITE. */
enum SessionChange {

  /** Put the call on hold: we offer `sendonly`. */
  case Hold

  /** Take the call off hold: we offer `sendrecv`. */
  case Resume

  def direction: MediaDirection = this match {
    case Hold   => MediaDirection.SendOnly
    case Resume => MediaDirection.SendRecv
  }
}
