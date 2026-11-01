package softphone4s.sdp

import softphone4s.srtp.{MasterKey, SrtpPolicy, SrtpSuite}

/** What we offer, or answer with, for the encryption of a stream: the policy,
  * and a line of our own for each suite we speak, in order of preference, each
  * with a key made for the call. Being made for the call, the keys are not part
  * of what an endpoint is capable of, and the caller makes them.
  */
final case class LocalCrypto(policy: SrtpPolicy, lines: List[CryptoAttribute])

object LocalCrypto {

  /** Plain RTP. */
  val Off: LocalCrypto = LocalCrypto(SrtpPolicy.Off, Nil)

  /** How many keys a call needs: one for each suite. */
  def keysNeeded: Int = SrtpSuite.values.length

  /** The lines for one call under `policy`, from `keys` (see [[keysNeeded]]):
    * one for each suite, numbered from 1 in order of preference, each with a
    * key of its own. Under `Off` there is nothing to offer.
    */
  def numbered(policy: SrtpPolicy, keys: List[MasterKey]): LocalCrypto =
    if policy == SrtpPolicy.Off then Off
    else
      LocalCrypto(
        policy,
        SrtpSuite.values.toList
          .zip(keys)
          .zipWithIndex
          .map { case ((suite, key), i) => CryptoAttribute(i + 1, suite, key) }
      )
}
