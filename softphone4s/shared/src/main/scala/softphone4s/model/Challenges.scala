package softphone4s.model

/** What a 401 or 407 asks of us, and when we may answer (RFC 3261 §22.2, RFC
  * 7616 §3.3). One place for the rule that the call, the registration and the
  * requests outside a dialog all follow.
  */
object Challenges {

  /** A challenge is answered once. A second one means the credentials were
    * refused, unless the server says the nonce only went stale, which earns one
    * more try; never more than this many answers of one kind.
    */
  val MaxAttempts: Int = 2

  /** Who challenged and with what: a 401 is the server (`WWW-Authenticate`), a
    * 407 a proxy (`Proxy-Authenticate`). None for any other response.
    */
  def of(response: SipResponse): Option[(AuthKind, List[AuthChallenge])] =
    response.statusCode match {
      case 401 => Some((AuthKind.Origin, response.headers.wwwAuthenticate))
      case 407 => Some((AuthKind.Proxy, response.headers.proxyAuthenticate))
      case _   => None
    }

  /** `Challenges.of` as a pattern:
    * `case response @ Challenged(kind, challenges)`.
    */
  object Challenged {
    def unapply(
        response: SipResponse
    ): Option[(AuthKind, List[AuthChallenge])] = of(response)
  }

  /** Whether `challenge` may be answered after `made` answers of its kind: the
    * first always, a later one only for a stale nonce.
    */
  def mayAnswer(made: Int, challenge: DigestChallenge): Boolean =
    made == 0 ||
      (made < MaxAttempts && challenge.staleness == Staleness.Stale)

  /** The challenge to answer: the best of those we can, if we may. */
  def toAnswer(
      challenges: List[AuthChallenge],
      made: Int
  ): Option[DigestChallenge] =
    AuthChallenge.best(challenges).filter(mayAnswer(made, _))

  /** The challenge to answer when the request is answered once at most, as the
    * removal of a binding is: the best of those we can, if none was answered.
    */
  def once(
      challenges: List[AuthChallenge],
      made: Int
  ): Option[DigestChallenge] =
    AuthChallenge.best(challenges).filter(_ => made == 0)
}
