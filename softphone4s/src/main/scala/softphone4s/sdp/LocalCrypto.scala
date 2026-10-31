package softphone4s.sdp

import cats.effect.Sync
import softphone4s.srtp.{MasterKey, SrtpPolicy, SrtpSuite}

import java.security.SecureRandom

/** What we offer, or answer with, for the encryption of a stream: the policy,
  * and a line of our own for each suite we speak, in order of preference, each
  * with a key made for the call. Being made for the call, the keys are not part
  * of what an endpoint is capable of, and the caller makes them.
  */
final case class LocalCrypto(policy: SrtpPolicy, lines: List[CryptoAttribute])

object LocalCrypto {

  /** Plain RTP. */
  val Off: LocalCrypto = LocalCrypto(SrtpPolicy.Off, Nil)

  /** The keys for one call under `policy`: a line for each suite, numbered from
    * 1 in order of preference, each with a key of its own from a secure random
    * source. Under `Off` there is nothing to make.
    */
  def generate[F[_]: Sync](policy: SrtpPolicy): F[LocalCrypto] =
    if policy == SrtpPolicy.Off then Sync[F].pure(Off)
    else
      Sync[F].delay {
        val random = new SecureRandom
        LocalCrypto(
          policy,
          SrtpSuite.values.toList.zipWithIndex.map((suite, i) =>
            CryptoAttribute(i + 1, suite, MasterKey.random(random))
          )
        )
      }
}
