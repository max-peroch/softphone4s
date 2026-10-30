package softphone4s.model

/** Digest hash algorithms we can answer. SHA-256 (RFC 8760) is planned. */
enum DigestAlgorithm(val token: String) {
  case Md5     extends DigestAlgorithm("MD5")
  case Md5Sess extends DigestAlgorithm("MD5-sess")
}

object DigestAlgorithm {
  def fromToken(s: String): Option[DigestAlgorithm] =
    values.find(_.token.equalsIgnoreCase(s.trim))
}
