package softphone4s.model

enum SipScheme(val value: String) {
  case Sip  extends SipScheme("sip")
  case Sips extends SipScheme("sips")
}
