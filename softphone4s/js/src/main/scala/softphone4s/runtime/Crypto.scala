package softphone4s.runtime

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, JSName}
import scala.scalajs.js.typedarray.*

/** The primitives SIP and SRTP are made of, as the platform has them: here the
  * `crypto` module of Node, which is synchronous, as the path of a packet
  * needs. The same object, with the same members, is in `jvm` and the other
  * platforms.
  */
private[softphone4s] object Crypto {

  @js.native
  @JSImport("node:crypto", JSImport.Namespace)
  private object NodeCrypto extends js.Object {
    def createCipheriv(
        algorithm: String,
        key: Int8Array,
        iv: Null
    ): NodeCipher                                               = js.native
    def createHmac(algorithm: String, key: Int8Array): NodeHmac = js.native
    def createHash(algorithm: String): NodeHash                 = js.native
    def timingSafeEqual(a: Int8Array, b: Int8Array): Boolean    = js.native
    def randomBytes(size: Int): Uint8Array                      = js.native
  }

  @js.native
  private trait NodeCipher extends js.Object {
    def setAutoPadding(on: Boolean): NodeCipher = js.native
    def update(data: Int8Array): Uint8Array     = js.native
    @JSName("final")
    def finish(): Uint8Array = js.native
  }

  @js.native
  private trait NodeHmac extends js.Object {
    def update(data: Int8Array): NodeHmac = js.native
    def digest(): Uint8Array              = js.native
  }

  @js.native
  private trait NodeHash extends js.Object {
    def update(data: String): NodeHash = js.native
    def digest(): Uint8Array           = js.native
  }

  private def bytes(array: Uint8Array): Array[Byte] =
    new Int8Array(array.buffer, array.byteOffset, array.length).toArray

  /** `blocks`, a whole number of 16-byte blocks, each encrypted with AES under
    * `key`, with no chaining and no padding (ECB, which is a block cipher used
    * by itself: the counter mode of RFC 3711 §4.1.1 is made on top of it). The
    * size of the key, 16, 24 or 32 bytes, picks AES-128, -192 or -256.
    */
  def aesEcb(key: Array[Byte], blocks: Array[Byte]): Array[Byte] = {
    val cipher =
      NodeCrypto.createCipheriv(
        s"aes-${key.length * 8}-ecb",
        key.toTypedArray,
        null
      )
    cipher.setAutoPadding(false)
    bytes(cipher.update(blocks.toTypedArray)) ++ bytes(cipher.finish())
  }

  /** HMAC-SHA1 of `data` under `key` (RFC 2104), 20 bytes. */
  def hmacSha1(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    bytes(
      NodeCrypto
        .createHmac("sha1", key.toTypedArray)
        .update(data.toTypedArray)
        .digest()
    )

  /** The digest of `text` as UTF-8, by the algorithm named as Java names them:
    * `MD5` or `SHA-256`.
    */
  def digest(algorithm: String, text: String): Array[Byte] =
    bytes(
      NodeCrypto
        .createHash(algorithm.filterNot(_ == '-').toLowerCase)
        .update(text)
        .digest()
    )

  /** Whether the two are equal, in a time that does not depend on where they
    * differ, so that a check of a secret does not tell how much of it is right.
    */
  def equal(a: Array[Byte], b: Array[Byte]): Boolean =
    a.length == b.length && NodeCrypto.timingSafeEqual(
      a.toTypedArray,
      b.toTypedArray
    )

  /** `n` bytes nobody can predict. */
  def randomBytes(n: Int): Array[Byte] = bytes(NodeCrypto.randomBytes(n))
}
