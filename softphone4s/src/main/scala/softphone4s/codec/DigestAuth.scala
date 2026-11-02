package softphone4s.codec

import softphone4s.model.*

import java.security.MessageDigest

/** HTTP Digest authentication (RFC 2617, RFC 7616, RFC 8760) — pure, no
  * external deps.
  */
object DigestAuth {

  /** Answers `challenge` for a request with `method`, `uri` (the digest-uri)
    * and `body`. The response uses `qop` and `nc`/`cnonce` when the challenge
    * offers one (`auth`, else `auth-int`, which also hashes the body), and the
    * RFC 2069 form otherwise.
    */
  def respond(
      challenge: DigestChallenge,
      method: String,
      uri: String,
      body: String,
      credentials: Credentials,
      cnonce: Cnonce,
      nonceCount: NonceCount
  ): DigestCredentials = {
    val Credentials(username, password) = credentials
    val algorithm                       = challenge.algorithm
    val realm                           = challenge.realm
    val nonce                           = challenge.nonce.value
    val hash: String => String          = hex(algorithm.javaName, _)

    val a1  = hash(s"$username:$realm:${password.reveal}")
    val ha1 =
      if algorithm.session then hash(s"$a1:$nonce:${cnonce.value}") else a1

    val qop = challenge.chosenQop
    val ha2 = qop match {
      case Some(Qop.AuthInt) => hash(s"$method:$uri:${hash(body)}")
      case _                 => hash(s"$method:$uri")
    }

    val qopResponse =
      qop.map(DigestCredentials.QopResponse(_, nonceCount, cnonce))
    val response = qopResponse match {
      case Some(q) =>
        hash(
          s"$ha1:$nonce:${q.nonceCount.render}:${q.cnonce.value}:${q.qop.token}:$ha2"
        )
      case None => hash(s"$ha1:$nonce:$ha2")
    }

    DigestCredentials(
      username,
      realm,
      challenge.nonce,
      uri,
      response,
      challenge.opaque,
      algorithm,
      qopResponse
    )
  }

  private def hex(javaName: String, input: String): String =
    MessageDigest
      .getInstance(javaName)
      .digest(input.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString
}
