package softphone4s.subscription

import softphone4s.model.Expires

import scala.concurrent.duration.*

/** The `Subscription-State` header of a NOTIFY (RFC 6665 §8.2.3): where the
  * subscription stands, as the notifier says.
  */
enum NotifyState {

  /** Authorised; `expires` is what is left of it. */
  case Active(expires: Option[Expires])

  /** Not yet authorised. */
  case Pending(expires: Option[Expires])

  /** Over. The subscriber may subscribe again, after `retryAfter` if it is
    * given, as `reason` allows.
    */
  case Terminated(reason: EndReason, retryAfter: Option[FiniteDuration])

  def isTerminated: Boolean = this match {
    case Terminated(_, _) => true
    case _                => false
  }
}

object NotifyState {

  /** The longest `retry-after` that is kept, 2^31-1 seconds (68 years), as long
    * as the project allows any lifetime in seconds (see `Expires`). RFC 6665
    * §4.1.3 gives `retry-after` as `delta-seconds`, `1*DIGIT`, with no limit; a
    * notifier that says to wait longer than this means not to come back. So a
    * larger value is this one, not an error and not "retry at once".
    */
  val MaxRetryAfter: FiniteDuration = Int.MaxValue.toLong.seconds

  /** `delta-seconds` (RFC 3261 §25.1: `1*DIGIT`, ASCII digits only) as a
    * duration, no longer than [[MaxRetryAfter]]; anything else is not one.
    * Turning a number into a `FiniteDuration` without that bound throws above
    * 292 years.
    */
  private def deltaSeconds(text: String): Option[FiniteDuration] =
    Option.when(text.nonEmpty && text.forall(c => c >= '0' && c <= '9'))(
      text.toLongOption
        .map(_.min(MaxRetryAfter.toSeconds).seconds)
        .getOrElse(MaxRetryAfter)
    )

  /** `active;expires=3600`, `pending`,
    * `terminated;reason=timeout;retry-after=30`.
    */
  def parse(value: String): Either[String, NotifyState] =
    value.split(';').toList.map(_.trim) match {
      case state :: params =>
        val named = params.flatMap { p =>
          p.split("=", 2) match {
            case Array(k, v) => List(k.trim.toLowerCase -> v.trim)
            case _           => Nil
          }
        }.toMap
        def expires: Either[String, Option[Expires]] =
          named
            .get("expires")
            .fold[Either[String, Option[Expires]]](Right(None))(e =>
              Expires.parse(e).map(Some(_))
            )
        state.toLowerCase match {
          case "active"     => expires.map(Active(_))
          case "pending"    => expires.map(Pending(_))
          case "terminated" =>
            val retry = named.get("retry-after").flatMap(deltaSeconds)
            Right(
              Terminated(
                named
                  .get("reason")
                  .fold(EndReason.Unspecified)(EndReason.parse),
                retry
              )
            )
          case other => Left(s"unknown subscription state: '$other'")
        }
      case Nil => Left("empty Subscription-State")
    }
}
