package softphone4s.model

/** A Digest challenge from WWW-Authenticate or Proxy-Authenticate.
  *
  * @param qop
  *   the qualities of protection the server offers; empty for RFC 2069.
  */
final case class DigestChallenge(
    realm: String,
    nonce: Nonce,
    opaque: Option[String],
    algorithm: DigestAlgorithm,
    qop: Set[Qop],
    staleness: Staleness = Staleness.Fresh
) {

  /** The quality of protection we answer with: `auth` if offered, else
    * `auth-int`, else none (the RFC 2069 form, when the server offers none).
    */
  def chosenQop: Option[Qop] =
    if qop.contains(Qop.Auth) then Some(Qop.Auth)
    else if qop.contains(Qop.AuthInt) then Some(Qop.AuthInt)
    else None

  /** Whether we can answer: no qop offered, or one we know. */
  def isAnswerable: Boolean = qop.isEmpty || chosenQop.isDefined

  def render: String = {
    val opaqueText =
      opaque.fold("")(o => s", opaque=${DigestChallenge.quoted(o)}")
    val staleText =
      if staleness == Staleness.Stale then ", stale=true" else ""
    val qopText =
      if qop.isEmpty then ""
      else s""", qop="${qop.toList.map(_.token).sorted.mkString(",")}""""
    s"Digest realm=${DigestChallenge.quoted(realm)}, nonce=${DigestChallenge.quoted(nonce.value)}" +
      opaqueText + s", algorithm=${algorithm.token}" + qopText + staleText
  }
}

object DigestChallenge {

  private[model] def quoted(s: String): String =
    "\"" + s.flatMap {
      case c @ ('"' | '\\') => s"\\$c"
      case c                => c.toString
    } + "\""
}
