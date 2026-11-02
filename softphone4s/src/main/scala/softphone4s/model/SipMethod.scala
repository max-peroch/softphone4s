package softphone4s.model

enum SipMethod {
  case Invite, Ack, Bye, Cancel, Options, Info, Register, Update, Message,
    Subscribe, Notify, Publish

  /** The method as it is written on the wire: `INVITE`, `OPTIONS`... */
  def token: String = toString.toUpperCase
}
