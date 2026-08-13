package softphone4s.codec

import java.security.MessageDigest

/** RFC 2617 HTTP Digest Authentication — pure, no external deps. */
object DigestAuth {

  /** Parse a WWW-Authenticate or Proxy-Authenticate challenge header and return
    * the Authorization / Proxy-Authorization header value to attach to the
    * retry request.
    */
  def compute(
      method: String,
      requestUri: String,
      username: String,
      password: String,
      challengeHeader: String,
      cnonce: String,
      nc: Int = 1
  ): String = {
    val challengeParams = parseChallenge(challengeHeader)
    val realm           = challengeParams.getOrElse("realm", "")
    val nonce           = challengeParams.getOrElse("nonce", "")
    val opaque          = challengeParams.get("opaque")
    val algorithm       = challengeParams.getOrElse("algorithm", "MD5")
    val qop             = challengeParams.get("qop").flatMap(parseQop)

    val ha1 =
      if algorithm.equalsIgnoreCase("MD5-sess") then
        md5(s"${md5(s"$username:$realm:$password")}:$nonce:$cnonce")
      else md5(s"$username:$realm:$password")

    val ha2 = md5(s"$method:$requestUri")

    val (response, extraParams) = qop match {
      case Some(q) =>
        val nonceCountHex  = "%08x".format(nc)
        val digestResponse = md5(s"$ha1:$nonce:$nonceCountHex:$cnonce:$q:$ha2")
        (digestResponse, s""", qop=$q, nc=$nonceCountHex, cnonce="$cnonce"""")
      case None =>
        (md5(s"$ha1:$nonce:$ha2"), "")
    }

    val opaquePart = opaque.map(o => s""", opaque="$o"""").getOrElse("")

    s"""Digest username="$username", realm="$realm", nonce="$nonce", uri="$requestUri", response="$response"$opaquePart$extraParams"""
  }

  def extractNonce(challengeHeader: String): Option[String] =
    parseChallenge(challengeHeader).get("nonce").filter(_.nonEmpty)

  // -------------------------------------------------------------------------

  private def parseQop(value: String): Option[String] =
    value
      .split(",")
      .map(_.trim)
      .find(token => token == "auth" || token == "auth-int")

  private def parseChallenge(header: String): Map[String, String] = {
    val withoutScheme = header.replaceFirst("(?i)^\\s*Digest\\s+", "")
    val parts         =
      withoutScheme.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)").map(_.trim)
    parts.flatMap { part =>
      part.split("=", 2) match {
        case Array(key, value) =>
          Some(key.trim -> value.trim.stripPrefix("\"").stripSuffix("\""))
        case _ => None
      }
    }.toMap
  }

  private def md5(input: String): String =
    MessageDigest
      .getInstance("MD5")
      .digest(input.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString
}
