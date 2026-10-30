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

  /** Whether we can answer: no qop, or `auth` among them (`auth-int` would need
    * the body hash).
    */
  def isAnswerable: Boolean = qop.isEmpty || qop.contains(Qop.Auth)

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
