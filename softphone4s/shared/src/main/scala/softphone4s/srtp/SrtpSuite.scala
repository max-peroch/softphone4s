package softphone4s.srtp

/** The cryptographic suites of RFC 3711 §5 that SDES (RFC 4568) names. Both
  * encrypt with AES in counter mode and authenticate with HMAC-SHA1; they
  * differ in how much of the tag an SRTP packet carries. (SRTCP always carries
  * 80 bits.)
  */
enum SrtpSuite(val sdpName: String, val tagLength: Int) {
  case AesCm128HmacSha1_80 extends SrtpSuite("AES_CM_128_HMAC_SHA1_80", 10)
  case AesCm128HmacSha1_32 extends SrtpSuite("AES_CM_128_HMAC_SHA1_32", 4)
}

object SrtpSuite {

  /** The suite SDP calls `name`. */
  def fromName(name: String): Option[SrtpSuite] =
    values.find(_.sdpName == name)
}
