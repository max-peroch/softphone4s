package softphone4s.srtp

/** The keys a call's media is encrypted with. SDES gives each side its own: the
  * one we send with, which we made and told the other side, and the one we
  * receive with, which they made and told us.
  */
final case class SrtpPlan(suite: SrtpSuite, send: MasterKey, receive: MasterKey)
