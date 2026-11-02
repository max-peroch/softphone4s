package softphone4s.model

import cats.{Eq, Show}
import pureconfig.ConfigReader

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

/** A password or other secret. It cannot be printed by accident: `toString`,
  * `Show` and a log line of a configuration that holds one all say `***`, and
  * the text is had only by asking for it with [[reveal]], where it is needed.
  * Two are equal if their text is, compared in constant time.
  */
final class Secret private (value: String) {

  /** The text itself. */
  def reveal: String = value

  override def equals(other: Any): Boolean = other match {
    case that: Secret =>
      MessageDigest.isEqual(value.getBytes(UTF_8), that.reveal.getBytes(UTF_8))
    case _ => false
  }

  override def hashCode: Int = value.hashCode

  override def toString: String = "***"
}

object Secret {

  /** Any text, the empty one included: some accounts have no password. */
  def apply(value: String): Secret = new Secret(value)

  given Eq[Secret]   = Eq.fromUniversalEquals
  given Show[Secret] = Show.show(_ => "***")

  given ConfigReader[Secret] = ConfigReader[String].map(Secret(_))
}
