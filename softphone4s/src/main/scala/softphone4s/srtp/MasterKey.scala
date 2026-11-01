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

  /** The key and salt in the first 30 bytes of `bytes`, which are to come from
    * a strong source of randomness (see `Entropy.masterKey`). Shorter input is
    * filled out with zeros, so that the function is total; no caller gives
    * less.
    */
  def fromEntropy(bytes: Chunk[Byte]): MasterKey = {
    val all = bytes.toArray.padTo(KeyLength + SaltLength, 0.toByte)
    new MasterKey(
      Chunk.array(all.take(KeyLength)),
      Chunk.array(all.slice(KeyLength, KeyLength + SaltLength))
    )
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
