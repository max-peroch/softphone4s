package softphone4s.model

/** Which side of the INVITE a dialog's owner is. */
enum DialogRole {

  /** Placed the INVITE. */
  case Uac

  /** Answered it. */
  case Uas
}
