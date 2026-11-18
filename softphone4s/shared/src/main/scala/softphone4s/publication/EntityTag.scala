package softphone4s.publication

import softphone4s.model.{FieldName, SipResponse}

/** The tag a notifier gives a published state (RFC 3903 §4): `SIP-ETag` in the
  * 200 that accepts it, and `SIP-If-Match` in the PUBLISH that refreshes,
  * changes or removes it. It is a token, and means nothing to the publisher.
  */
final case class EntityTag private (value: String) {
  override def toString: String = value
}

object EntityTag {

  private val Token = """[A-Za-z0-9\-\.!%*_+`'~]+""".r

  def parse(text: String): Either[String, EntityTag] =
    Either.cond(
      Token.matches(text.trim),
      new EntityTag(text.trim),
      s"not an entity tag: '$text'"
    )

  /** The tag a response gives: `None` if it gives none, an error if the one it
    * gives is not a token.
    */
  def of(response: SipResponse): Option[Either[String, EntityTag]] =
    response.headers.others
      .find(_.name.matches(FieldName.SipETag.value))
      .map(h => parse(h.value))
}
