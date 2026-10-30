package softphone4s.model

import cats.{Eq, Show}

import scala.collection.immutable.ListMap

/** The value of a From, To, Contact, Route or Record-Route header: an optional
  * display name, a URI, and header parameters, with the `tag` parameter typed.
  * It always renders with angle brackets around the URI.
  */
final case class NameAddr(
    displayName: Option[String],
    uri: SipUri,
    tag: Option[Tag],
    params: HeaderParams
) {

  def withTag(t: Tag): NameAddr = copy(tag = Some(t))

  def render: String = {
    val name    = displayName.fold("")(n => s""""${NameAddr.escape(n)}" """)
    val tagText = tag.fold("")(t => s";tag=${t.value}")
    s"$name<${uri.render}>$tagText${HeaderSyntax.renderParams(params)}"
  }

  override def toString: String = render
}

object NameAddr {

  def apply(uri: SipUri): NameAddr =
    NameAddr(None, uri, None, ListMap.empty)

  def withTag(uri: SipUri, tag: Tag): NameAddr =
    NameAddr(None, uri, Some(tag), ListMap.empty)

  /** Parses `["Display"] <uri>;params` or a bare `uri;params`, where the
    * parameters of a bare URI belong to the header (RFC 3261 §20.10).
    */
  def parse(value: String): Either[String, NameAddr] = {
    val v                                                       = value.trim
    val parts: Either[String, (Option[String], String, String)] =
      HeaderSyntax.firstUnquoted(v, '<') match {
        case Some(open) =>
          val rest = v.drop(open + 1)
          rest.indexOf('>') match {
            case -1    => Left(s"unterminated '<' in: '$value'")
            case close =>
              Right(
                (
                  displayName(v.take(open)),
                  rest.take(close),
                  rest.drop(close + 1)
                )
              )
          }
        case None =>
          val idx = v.indexOf(';')
          if idx < 0 then Right((None, v, ""))
          else Right((None, v.take(idx), v.drop(idx)))
      }
    for {
      p   <- parts
      uri <- SipUri.parse(p._2)
      all <- HeaderSyntax.parseParams(p._3.stripPrefix(";"))
      tag <- all.get(ParamName.TagParam) match {
        case None          => Right(None)
        case Some(None)    => Left(s"tag without a value in: '$value'")
        case Some(Some(t)) => Tag(t.value).map(Some(_))
      }
    } yield NameAddr(p._1, uri, tag, all.removed(ParamName.TagParam))
  }

  private def displayName(text: String): Option[String] = {
    val t = text.trim
    if t.isEmpty then None
    else if t.startsWith("\"") && t.endsWith("\"") && t.length >= 2 then
      Some(unescape(t.slice(1, t.length - 1)))
    else Some(t)
  }

  private def escape(s: String): String =
    s.flatMap {
      case c @ ('"' | '\\') => s"\\$c"
      case c                => c.toString
    }

  private def unescape(s: String): String =
    s.replaceAll("""\\(.)""", "$1")

  given Eq[NameAddr]   = Eq.fromUniversalEquals
  given Show[NameAddr] = Show.show(_.render)
}
