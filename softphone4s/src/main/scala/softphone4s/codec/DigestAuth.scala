package softphone4s.codec

import softphone4s.model.*

import java.security.MessageDigest

/** RFC 2617 HTTP Digest authentication — pure, no external deps. */
object DigestAuth {

  /** Answers `challenge` for a request with `method` and `uri` (the
    * digest-uri). When the challenge offers `auth`, the response includes
    * `qop`, `nc` and `cnonce`; otherwise it is the RFC 2069 form.
    */
  def respond(
      challenge: DigestChallenge,
      method: String,
      uri: String,
      credentials: Credentials,
      cnonce: Cnonce,
      nonceCount: NonceCount
  ): DigestCredentials = {
    val Credentials(username, password) = credentials
    val realm                           = challenge.realm
    val nonce                           = challenge.nonce.value
    val a1                              = md5(s"$username:$realm:$password")
    val ha1                             = challenge.algorithm match {
      case DigestAlgorithm.Md5     => a1
      case DigestAlgorithm.Md5Sess => md5(s"$a1:$nonce:${cnonce.value}")
    }
    val ha2 = md5(s"$method:$uri")

    val qop = Option.when(challenge.qop.contains(Qop.Auth))(
      DigestCredentials.QopResponse(nonceCount, cnonce)
    )
    val response = qop match {
      case Some(q) =>
        md5(
          s"$ha1:$nonce:${q.nonceCount.render}:${q.cnonce.value}:${Qop.Auth.token}:$ha2"
        )
      case None => md5(s"$ha1:$nonce:$ha2")
    }

    DigestCredentials(
      username,
      realm,
      challenge.nonce,
      uri,
      response,
      challenge.opaque,
      challenge.algorithm,
      qop
    )
  }

  private def md5(input: String): String =
    MessageDigest
      .getInstance("MD5")
      .digest(input.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString
}
