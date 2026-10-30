package softphone4s.model

/** The answer to a [[DigestChallenge]], for Authorization or
  * Proxy-Authorization.
  *
  * @param uri
  *   the digest-uri: the request-URI the response was computed over.
  * @param qop
  *   present when the challenge offered `auth` or `auth-int`.
  */
final case class DigestCredentials(
    username: String,
    realm: String,
    nonce: Nonce,
    uri: String,
    response: String,
    opaque: Option[String],
    algorithm: DigestAlgorithm,
    qop: Option[DigestCredentials.QopResponse]
) {

  def render: String = {
    val q             = DigestChallenge.quoted
    val algorithmText =
      if algorithm == DigestAlgorithm.Md5 then ""
      else s", algorithm=${algorithm.token}"
    val opaqueText = opaque.fold("")(o => s", opaque=${q(o)}")
    val qopText    = qop.fold("") { r =>
      s", qop=${r.qop.token}, nc=${r.nonceCount.render}, cnonce=${q(r.cnonce.value)}"
    }
    s"Digest username=${q(username)}, realm=${q(realm)}, nonce=${q(nonce.value)}, uri=${q(uri)}, response=${q(response)}" +
      algorithmText + opaqueText + qopText
  }
}

object DigestCredentials {
  final case class QopResponse(
      qop: Qop,
      nonceCount: NonceCount,
      cnonce: Cnonce
  )
}
