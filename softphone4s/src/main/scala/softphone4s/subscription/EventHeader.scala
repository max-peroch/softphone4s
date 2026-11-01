package softphone4s.subscription

import cats.syntax.all.*

/** The `Event` header (RFC 6665 §8.2.1): the event package a subscription is
  * to, and the `id` parameter that tells apart subscriptions to one package in
  * one dialog. The package name is a token (dots allowed, as in
  * `presence.winfo`), compared without regard to case and kept lower-cased.
  */
final case class EventHeader private (
    eventPackage: String,
    id: Option[String]
) {

  /** Whether `other` is the same subscription: same package, same `id`. */
  def matches(other: EventHeader): Boolean = this == other

  def render: String = s"$eventPackage${id.fold("")(i => s";id=$i")}"

  override def toString: String = render
}

object EventHeader {

  private val Token = """[A-Za-z0-9\-\.!%*_+`'~]+""".r

  /** The `presence` package (RFC 3856). */
  val Presence: EventHeader = new EventHeader("presence", None)

  /** The `message-summary` package (RFC 3842). */
  val MessageSummary: EventHeader = new EventHeader("message-summary", None)

  /** The `Event` header of a package, with an `id` if there is more than one
    * subscription to it in the dialog.
    */
  def apply(
      eventPackage: String,
      id: Option[String] = None
  ): Either[String, EventHeader] =
    for {
      name <- Either.cond(
        Token.matches(eventPackage),
        eventPackage.toLowerCase,
        s"not an event package name: '$eventPackage'"
      )
      kept <- id.traverse(i =>
        Either.cond(
          Token.matches(i),
          i,
          s"not an event id: '$i'"
        )
      )
    } yield new EventHeader(name, kept)

  /** The value of an `Event` header: `presence`, `message-summary;id=1`. Other
    * parameters (`;foo=bar`) are not part of the identity and are left out.
    */
  def parse(value: String): Either[String, EventHeader] =
    value.split(';').toList.map(_.trim) match {
      case name :: params if name.nonEmpty =>
        val id = params.collectFirst {
          case p if p.toLowerCase.startsWith("id=") => p.drop(3).trim
        }
        apply(name, id)
      case _ => Left(s"empty Event header: '$value'")
    }
}
