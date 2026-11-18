package softphone4s.model

import cats.{Order, Show}

import scala.concurrent.duration.*

/** A lifetime in whole seconds: the `Expires` and `Min-Expires` headers and the
  * `expires` parameter of a Contact (RFC 3261 §20.19). Zero means "remove".
  */
opaque type Expires = Int

object Expires {

  def apply(seconds: Int): Either[String, Expires] =
    Either.cond(seconds >= 0, seconds, s"invalid expiry: $seconds")

  /** Parses delta-seconds: digits only, and no larger than an `Int`. */
  def parse(text: String): Either[String, Expires] =
    text.trim.toIntOption
      .filter(_ => text.trim.forall(_.isDigit))
      .toRight(s"invalid expiry: '$text'")
      .flatMap(apply)

  /** Removes a registration. */
  val Zero: Expires = 0

  /** `duration` rounded down to whole seconds, at least zero. */
  def of(duration: FiniteDuration): Expires =
    duration.toSeconds.max(0L).min(Int.MaxValue.toLong).toInt

  /** The lifetime to ask a registrar for: whole seconds, rounded down, and
    * never less than one, since zero removes a registration.
    */
  def forRegistration(lifetime: PositiveDuration): Expires =
    of(lifetime.value.max(1.second))

  extension (e: Expires) {
    def value: Int               = e
    def duration: FiniteDuration = e.toLong.seconds
    def isZero: Boolean          = e == 0
  }

  given Order[Expires] = Order.by[Expires, Int](identity)
  given Show[Expires]  = Show.show(_.toString)
}
