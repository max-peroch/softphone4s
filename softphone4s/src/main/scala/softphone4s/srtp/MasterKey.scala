package softphone4s.srtp

import fs2.Chunk

/** The master key and master salt of an SRTP stream (RFC 3711 §3.2.1), from
  * which its session keys are derived. Prints as nothing.
  */
final class MasterKey private (
    private[srtp] val key: Chunk[Byte],
    private[srtp] val salt: Chunk[Byte]
) {

  /** The key and salt joined, as SDES carries them (RFC 4568 §6.1). */
  def bytes: Chunk[Byte] = key ++ salt

  override def toString: String = "MasterKey(***)"
}

object MasterKey {

  val KeyLength: Int  = 16
  val SaltLength: Int = 14

  /** A key and a salt of random bytes from `random`, which must be a secure
    * one.
    */
  def random(random: java.security.SecureRandom): MasterKey = {
    val key  = new Array[Byte](KeyLength)
    val salt = new Array[Byte](SaltLength)
    random.nextBytes(key)
    random.nextBytes(salt)
    new MasterKey(Chunk.array(key), Chunk.array(salt))
  }

  /** A key of 16 bytes and a salt of 14. */
  def apply(
      key: Chunk[Byte],
      salt: Chunk[Byte]
  ): Either[SrtpError, MasterKey] =
    for {
      _ <- Either.cond(
        key.size == KeyLength,
        (),
        SrtpError.BadKeyLength(KeyLength, key.size)
      )
      _ <- Either.cond(
        salt.size == SaltLength,
        (),
        SrtpError.BadSaltLength(SaltLength, salt.size)
      )
    } yield new MasterKey(key, salt)

  /** A key and then a salt in one run of 30 bytes. */
  def fromBytes(bytes: Chunk[Byte]): Either[SrtpError, MasterKey] =
    apply(bytes.take(KeyLength), bytes.drop(KeyLength)).left.map {
      case SrtpError.BadKeyLength(expected, _) =>
        SrtpError.BadKeyLength(expected + SaltLength, bytes.size)
      case other => other
    }
}
