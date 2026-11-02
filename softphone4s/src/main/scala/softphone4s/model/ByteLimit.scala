package softphone4s.model

import cats.{Eq, Show}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** A limit on a size in bytes, at least one: the largest datagram sent, the
  * largest header or body of a message on a stream.
  */
opaque type ByteLimit = Int

object ByteLimit {

  def apply(bytes: Int): Either[String, ByteLimit] =
    Either.cond(bytes >= 1, bytes, s"a limit of $bytes bytes is below one")

  /** Whole kibibytes. */
  def kibibytes(n: Int): Either[String, ByteLimit] = apply(n * 1024)

  /** 1300 bytes, under which a request fits a datagram (RFC 3261 §18.1.1). */
  val Datagram: ByteLimit = 1300

  /** 16 KiB of headers on a stream. */
  val Headers: ByteLimit = 16 * 1024

  /** 1 MiB of body on a stream. */
  val Body: ByteLimit = 1024 * 1024

  extension (limit: ByteLimit) {
    def value: Int = limit

    /** Whether a size of `bytes` is over the limit. */
    def isExceededBy(bytes: Int): Boolean = bytes > limit
  }

  given Eq[ByteLimit]   = Eq.fromUniversalEquals
  given Show[ByteLimit] = Show.show(l => s"$l bytes")

  given ConfigReader[ByteLimit] = ConfigReader[Int].emap(n =>
    apply(n).left.map(CannotConvert(n.toString, "ByteLimit", _))
  )
}
