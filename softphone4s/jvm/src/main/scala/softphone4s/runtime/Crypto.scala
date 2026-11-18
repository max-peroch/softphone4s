package softphone4s.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.security.{MessageDigest, SecureRandom}
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, Mac}

/** The primitives SIP and SRTP are made of, as the platform has them: here
  * `javax.crypto` and `java.security`. The same object, with the same members,
  * is in `js` and the other platforms.
  */
private[softphone4s] object Crypto {

  /** `blocks`, a whole number of 16-byte blocks, each encrypted with AES under
    * `key`, with no chaining and no padding (ECB, which is a block cipher used
    * by itself: the counter mode of RFC 3711 §4.1.1 is made on top of it).
    */
  def aesEcb(key: Array[Byte], blocks: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"))
    cipher.doFinal(blocks)
  }

  /** HMAC-SHA1 of `data` under `key` (RFC 2104), 20 bytes. */
  def hmacSha1(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val hmac = Mac.getInstance("HmacSHA1")
    hmac.init(new SecretKeySpec(key, "HmacSHA1"))
    hmac.doFinal(data)
  }

  /** The digest of `text` as UTF-8, by the algorithm named as Java names them:
    * `MD5` or `SHA-256`.
    */
  def digest(algorithm: String, text: String): Array[Byte] =
    MessageDigest.getInstance(algorithm).digest(text.getBytes(UTF_8))

  /** Whether the two are equal, in a time that does not depend on where they
    * differ, so that a check of a secret does not tell how much of it is right.
    */
  def equal(a: Array[Byte], b: Array[Byte]): Boolean =
    MessageDigest.isEqual(a, b)

  private val random = new SecureRandom

  /** `n` bytes nobody can predict. */
  def randomBytes(n: Int): Array[Byte] = {
    val bytes = new Array[Byte](n)
    random.nextBytes(bytes)
    bytes
  }
}
