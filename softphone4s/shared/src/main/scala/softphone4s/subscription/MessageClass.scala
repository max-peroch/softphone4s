package softphone4s.subscription

/** The kinds of message a mailbox holds (RFC 3842 §5.2). */
enum MessageClass(val field: String) {
  case Voice      extends MessageClass("voice-message")
  case Fax        extends MessageClass("fax-message")
  case Pager      extends MessageClass("pager-message")
  case Multimedia extends MessageClass("multimedia-message")
  case Text       extends MessageClass("text-message")
}

object MessageClass {

  /** The class named by a field of the body, without regard to case. */
  def named(field: String): Option[MessageClass] =
    values.find(_.field.equalsIgnoreCase(field))
}
