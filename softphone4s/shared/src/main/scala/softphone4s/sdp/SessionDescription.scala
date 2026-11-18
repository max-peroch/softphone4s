package softphone4s.sdp

import cats.syntax.all.*
import com.comcast.ip4s.{Host, IpAddress, Port, ip}
import softphone4s.model.MediaDirection

import scala.annotation.tailrec

/** A Session Description (RFC 4566): the session-level lines and the media
  * sections. Only what a softphone acts on is kept; lines for bandwidth,
  * encryption keys, repeat times and the like are read and dropped, and the
  * timing is always `t=0 0`. A description is read even if it lacks `o=`, `s=`
  * or `t=`, which it should not; one without `v=` is refused.
  */
final case class SessionDescription(
    origin: Origin,
    name: String,
    connection: Option[IpAddress],
    attributes: List[Attribute],
    media: List[MediaDescription]
) {

  /** Where media for `section` is to be sent: its own address, else the
    * session's.
    */
  def addressOf(section: MediaDescription): Option[IpAddress] =
    section.connection.orElse(connection)

  /** The direction `section` is in: its own, else the session's, else
    * `sendrecv`. A connection address of 0.0.0.0 is the old way to say the
    * sender will not receive (RFC 2543), which is `inactive`.
    */
  def directionOf(section: MediaDescription): MediaDirection =
    if addressOf(section).exists(SessionDescription.isUnspecified) then
      MediaDirection.Inactive
    else
      section.direction
        .orElse(MediaDescription.directionIn(attributes))
        .getOrElse(MediaDirection.SendRecv)

  /** The text of the description, with the line endings SDP uses. */
  def render: String = {
    val lines =
      "v=0" ::
        s"o=${origin.username} ${origin.sessionId} ${origin.sessionVersion} IN ${SessionDescription
            .addressTypeOf(origin.address)} ${origin.address}" ::
        s"s=${if name.isEmpty then " " else name}" ::
        connection.toList.map(c =>
          s"c=IN ${SessionDescription.addressType(c)} $c"
        ) :::
        "t=0 0" ::
        attributes.map(_.render) :::
        media.flatMap(_.render(SessionDescription.addressType))
    lines.mkString("", "\r\n", "\r\n")
  }
}

object SessionDescription {

  private[sdp] def addressType(address: IpAddress): String =
    address.fold(_ => "IP4", _ => "IP6")

  private[sdp] def addressTypeOf(host: Host): String =
    host match {
      case address: IpAddress => addressType(address)
      case _                  => "IP4"
    }

  private[sdp] def isUnspecified(address: IpAddress): Boolean =
    address == ip"0.0.0.0" || address == ip"::"

  /** One `x=value` line, and where it was. */
  private final case class Field(kind: Char, value: String, line: Int)

  /** Reads a description. Lines may end in CRLF or LF. */
  def parse(text: String): Either[SdpError, SessionDescription] =
    for {
      fields <- text.linesIterator.zipWithIndex
        .map((l, i) => (l.trim, i + 1))
        .filter(_._1.nonEmpty)
        .toList
        .traverse(field)
      (head, rest) = fields.span(_.kind != 'm')
      version <- head
        .find(_.kind == 'v')
        .toRight(SdpError.Missing('v'))
      _ <- Either.cond(
        version.value == "0",
        (),
        SdpError.Malformed(version.line, "version is not 0")
      )
      origin <- head
        .find(_.kind == 'o')
        .fold(Right(Origin.Unknown))(originOf)
      connection <- head
        .find(_.kind == 'c')
        .traverse(f => connectionOf(f))
      sections <- sectionsOf(rest).traverse(mediaOf)
    } yield SessionDescription(
      origin,
      head.find(_.kind == 's').fold("")(_.value.trim),
      connection,
      head.filter(_.kind == 'a').map(f => Attribute.parse(f.value)),
      sections
    )

  private def field(line: (String, Int)): Either[SdpError, Field] =
    line match {
      case (text, number)
          if text.length >= 2 && text(1) == '=' && text(0).isLetter =>
        Right(Field(text(0), text.drop(2), number))
      case (_, number) =>
        Left(SdpError.Malformed(number, "not a <type>=<value> line"))
    }

  private def originOf(f: Field): Either[SdpError, Origin] =
    f.value.split("\\s+").toList match {
      case user :: id :: version :: "IN" :: _ :: address :: Nil =>
        for {
          sessionId      <- number(f, id)
          sessionVersion <- number(f, version)
          host           <- Host
            .fromString(address)
            .toRight(SdpError.Malformed(f.line, s"bad address $address"))
        } yield Origin(user, sessionId, sessionVersion, host)
      case _ => Left(SdpError.Malformed(f.line, "bad o= line"))
    }

  private def number(f: Field, text: String): Either[SdpError, BigInt] =
    Either
      .catchOnly[NumberFormatException](BigInt(text))
      .leftMap(_ => SdpError.Malformed(f.line, s"$text is not a number"))

  /** `IN IP4 192.0.2.1`, with a multicast `/ttl` or `/count` ignored. */
  private def connectionOf(f: Field): Either[SdpError, IpAddress] =
    f.value.split("\\s+").toList match {
      case "IN" :: _ :: address :: Nil =>
        val text = address.takeWhile(_ != '/')
        IpAddress
          .fromString(text)
          .toRight(
            if Host.fromString(text).isDefined then SdpError.NameAddress(text)
            else SdpError.Malformed(f.line, s"bad address $text")
          )
      case _ => Left(SdpError.Malformed(f.line, "bad c= line"))
    }

  /** The lines from each `m=` up to the next. */
  private def sectionsOf(fields: List[Field]): List[List[Field]] = {
    @tailrec
    def go(rest: List[Field], acc: List[List[Field]]): List[List[Field]] =
      rest match {
        case Nil           => acc.reverse
        case first :: tail =>
          val (own, others) = tail.span(_.kind != 'm')
          go(others, (first :: own) :: acc)
      }
    go(fields, Nil)
  }

  private def mediaOf(fields: List[Field]): Either[SdpError, MediaDescription] =
    fields match {
      case first :: lines =>
        first.value.split("\\s+").toList match {
          case kind :: portText :: protocol :: formats if formats.nonEmpty =>
            for {
              port <- portText
                .takeWhile(_ != '/')
                .toIntOption
                .flatMap(Port.fromInt)
                .toRight(SdpError.Malformed(first.line, s"bad port $portText"))
              connection <- lines.find(_.kind == 'c').traverse(connectionOf)
            } yield MediaDescription(
              MediaKind.fromToken(kind),
              port,
              MediaProtocol.fromToken(protocol),
              formats,
              connection,
              lines.filter(_.kind == 'a').map(f => Attribute.parse(f.value))
            )
          case _ => Left(SdpError.Malformed(first.line, "bad m= line"))
        }
      case Nil => Left(SdpError.Malformed(0, "empty media section"))
    }
}
