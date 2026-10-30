package softphone4s.model

/** Digest hash algorithms we can answer (RFC 2617, RFC 7616, RFC 8760). A
  * `-sess` algorithm mixes the nonces into the key once (`session`).
  *
  * @param strength
  *   the higher the better; the strongest algorithm a server offers is used.
  */
enum DigestAlgorithm(
    val token: String,
    val javaName: String,
    val session: Boolean,
    val strength: Int
) {
  case Md5        extends DigestAlgorithm("MD5", "MD5", false, 1)
  case Md5Sess    extends DigestAlgorithm("MD5-sess", "MD5", true, 1)
  case Sha256     extends DigestAlgorithm("SHA-256", "SHA-256", false, 2)
  case Sha256Sess extends DigestAlgorithm("SHA-256-sess", "SHA-256", true, 2)
}

object DigestAlgorithm {
  def fromToken(s: String): Option[DigestAlgorithm] =
    values.find(_.token.equalsIgnoreCase(s.trim))
}
