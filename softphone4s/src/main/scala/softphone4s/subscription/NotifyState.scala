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
            val retry = named
              .get("retry-after")
              .flatMap(_.toLongOption)
              .filter(_ >= 0)
              .map(_.seconds)
            Right(
              Terminated(
                named
                  .get("reason")
                  .fold(EndReason.Other("none"))(EndReason.parse),
                retry
              )
            )
          case other => Left(s"unknown subscription state: '$other'")
        }
      case Nil => Left("empty Subscription-State")
    }
}
