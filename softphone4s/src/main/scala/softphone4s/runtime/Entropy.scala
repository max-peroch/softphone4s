package softphone4s.runtime

import cats.Functor
import cats.effect.Sync
import cats.effect.std.{Random, SecureRandom}
import cats.syntax.all.*
import com.comcast.ip4s.Host
import softphone4s.model.*

/** Where the identifiers of a SIP exchange come from, so the code that needs
  * them stays deterministic given an instance: tests pass a seeded one.
  */
trait Entropy[F[_]] {
  def tag: F[Tag]
  def branch: F[Branch]
  def callId(host: Host): F[CallId]
  def cnonce: F[Cnonce]

  /** A number in [0, 1), for jittering timers. */
  def fraction: F[Double]
}

object Entropy {

  def fromRandom[F[_]: Functor](random: Random[F]): Entropy[F] =
    new Entropy[F] {
      def tag: F[Tag]       = random.nextBytes(4).map(Tag.fromBytes)
      def branch: F[Branch] = random.nextBytes(8).map(Branch.fromBytes)
      def callId(host: Host): F[CallId] =
        random.nextBytes(10).map(CallId.fromBytes(_, host))
      def cnonce: F[Cnonce]   = random.nextBytes(8).map(Cnonce.fromBytes)
      def fraction: F[Double] = random.nextDouble
    }

  /** Backed by the JVM's cryptographically strong generator. */
  def secure[F[_]: Sync]: F[Entropy[F]] =
    SecureRandom.javaSecuritySecureRandom[F].map(fromRandom(_))
}
