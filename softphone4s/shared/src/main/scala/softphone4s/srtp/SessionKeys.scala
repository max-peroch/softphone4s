package softphone4s.srtp

import fs2.Chunk

/** The keys an SRTP or SRTCP stream uses, derived from the master key (RFC 3711
  * §4.3): one to encrypt, one to authenticate, and the salt of the IV. Prints
  * as nothing.
  */
final class SessionKeys private (
    private[srtp] val cipherKey: Chunk[Byte],
    private[srtp] val authKey: Chunk[Byte],
    private[srtp] val salt: Chunk[Byte]
) {
  override def toString: String = "SessionKeys(***)"
}

object SessionKeys {

  private val CipherKeyLength = 16
  private val AuthKeyLength   = 20

  /** The keys of the RTP stream: labels 0, 1 and 2. */
  def srtp(master: MasterKey): SessionKeys = derive(master, 0x00, 0x01, 0x02)

  /** The keys of the RTCP stream: labels 3, 4 and 5. */
  def srtcp(master: MasterKey): SessionKeys = derive(master, 0x03, 0x04, 0x05)

  private def derive(
      master: MasterKey,
      cipher: Int,
      auth: Int,
      salt: Int
  ): SessionKeys =
    new SessionKeys(
      key(master, cipher, CipherKeyLength),
      key(master, auth, AuthKeyLength),
      key(master, salt, MasterKey.SaltLength)
    )

  /** `PRF_n(master key, x)` with the key derivation rate zero, so that the
    * whole stream uses one set of keys: `x` is the label, then 48 zero bits,
    * set against the right end of the master salt, and the IV is `x * 2^16`.
    */
  private def key(master: MasterKey, label: Int, length: Int): Chunk[Byte] = {
    val x = master.salt.toArray
    x(MasterKey.SaltLength - 7) = (x(MasterKey.SaltLength - 7) ^ label).toByte
    Chunk.array(
      AesCounter.keystream(master.key.toArray, x ++ Array[Byte](0, 0), length)
    )
  }
}
