package softphone4s.model

import cats.syntax.all.*
import cats.{Eq, Show}

import scala.collection.immutable.ListMap

/** A Content-Type: `type/subtype;param=value` (RFC 3261 §25.1). The type and
  * subtype are tokens, compared case-insensitively and stored lower-cased.
  * Construct via [[MediaType.from]] or [[MediaType.parse]].
  */
final case class MediaType private (
    mainType: String,
    subType: String,
    params: HeaderParams
) {
  def render: String =
    s"$mainType/$subType${HeaderSyntax.renderParams(params)}"

  /** Whether this is `application/sdp`, whatever its parameters. */
  def isSdp: Boolean = mainType == "application" && subType == "sdp"

  override def toString: String = render
}

object MediaType {

  private val syntax =
    UriSyntax.token(UriSyntax.TokenChars, Emptiness.Forbidden)

  val Sdp: MediaType = MediaType("application", "sdp", ListMap.empty)

  /** The text of an instant message (RFC 3428). */
  val Plain: MediaType = MediaType("text", "plain", ListMap.empty)

  /** A digit sent in SIP INFO, the way most phones do. */
  val DtmfRelay: MediaType =
    MediaType("application", "dtmf-relay", ListMap.empty)

  def from(
      mainType: String,
      subType: String,
      params: HeaderParams = ListMap.empty
  ): Either[String, MediaType] =
    (
      UriSyntax.check("media type", syntax, mainType),
      UriSyntax.check("media subtype", syntax, subType)
    ).mapN((t, s) => new MediaType(t.toLowerCase, s.toLowerCase, params))

  def parse(value: String): Either[String, MediaType] = {
    val parts = HeaderSyntax.split(value, ';')
    for {
      typeText <- parts.headOption.toRight("empty Content-Type")
      pair     <- typeText.trim.split("/", 2) match {
        case Array(t, s) => Right((t.trim, s.trim))
        case _           => Left(s"malformed Content-Type: '$value'")
      }
      params <- HeaderSyntax.parseParams(parts.tail.mkString(";"))
      mt     <- from(pair._1, pair._2, params)
    } yield mt
  }

  given Eq[MediaType]   = Eq.fromUniversalEquals
  given Show[MediaType] = Show.show(_.render)
}
