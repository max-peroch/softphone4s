package softphone4s.srtp

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

/** AES in counter mode, the cipher of RFC 3711 §4.1.1: the keystream is the
  * encryption of the IV, the IV plus 1, the IV plus 2, and so on, with the
  * block number counted in the low 16 bits of the IV, which are zero to begin
  * with.
  */
private[srtp] object AesCounter {

  /** The most blocks that 16 bits can count. */
  private val MaxBlocks = 1 << 16

  /** The first `length` bytes of the keystream of `key` from `iv`, a block of
    * 16 bytes whose last two are zero. At most 1 MiB, which is what a packet
    * and the key derivation need many times over.
    */
  def keystream(key: Array[Byte], iv: Array[Byte], length: Int): Array[Byte] = {
    require(iv.length == 16 && iv(14) == 0 && iv(15) == 0, "not an IV")
    val blocks = (length + 15) / 16
    require(blocks <= MaxBlocks, s"$length bytes of keystream is too long")
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"))
    val counter = iv.clone()
    val out     = new Array[Byte](length)
    Iterator.range(0, blocks).foreach { j =>
      counter(14) = (j >> 8).toByte
      counter(15) = j.toByte
      val block = cipher.doFinal(counter)
      System.arraycopy(block, 0, out, j * 16, math.min(16, length - j * 16))
    }
    out
  }
}
