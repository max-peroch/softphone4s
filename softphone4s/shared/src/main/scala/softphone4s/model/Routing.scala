package softphone4s.model

/** Whether a SIP URI asks for loose routing (the `lr` parameter, RFC 3261
  * §19.1.1).
  */
enum Routing {
  case Strict, Loose
}
