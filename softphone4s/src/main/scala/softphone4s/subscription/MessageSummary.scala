package softphone4s.subscription

import cats.syntax.all.*
import softphone4s.model.{MediaType, SipUri}

/** What a mailbox holds (RFC 3842): whether there are messages waiting, the
  * account they are for, and counts by class where the server gives them.
  */
final case class MessageSummary(
    waiting: Boolean,
    account: Option[SipUri],
    counts: Map[MessageClass, MessageCounts]
)

object MessageSummary {

  val ContentType: MediaType = MediaType.SimpleMessageSummary

  /** The `message-summary` package. */
  val Package: EventPackage[MessageSummary] = new EventPackage {
    val event: EventHeader      = EventHeader.MessageSummary
    val accept: List[MediaType] = List(ContentType)
    def decode(n: Notification): Either[PackageError, MessageSummary] =
      EventPackage.bodyOf(n, ContentType).flatMap(parse)
  }

  private val Counts =
    """(\d+)\s*/\s*(\d+)(?:\s*\(\s*(\d+)\s*/\s*(\d+)\s*\))?""".r

  /** Reads the body: `Field: value` lines, whose names are not case-sensitive.
    * A field that is not known is skipped, as the RFC asks.
    */
  def parse(body: String): Either[PackageError, MessageSummary] = {
    val fields = body.linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
      .traverse { line =>
        line.split(":", 2) match {
          case Array(name, value) => Right((name.trim.toLowerCase, value.trim))
          case _ => Left(PackageError.Malformed(s"not a field: '$line'"))
        }
      }
    fields.flatMap { all =>
      val malformed = (reason: String) => PackageError.Malformed(reason)
      for {
        waiting <- all
          .collectFirst { case ("messages-waiting", v) => v.toLowerCase }
          .toRight(malformed("no Messages-Waiting"))
          .flatMap {
            case "yes" => Right(true)
            case "no"  => Right(false)
            case other => Left(malformed(s"Messages-Waiting: '$other'"))
          }
        account <- all
          .collectFirst { case ("message-account", v) => v }
          .traverse(v => SipUri.parse(v).leftMap(malformed))
        counts <- all.flatTraverse { (name, value) =>
          MessageClass
            .named(name)
            .toList
            .traverse(c => count(value).map(c -> _))
        }
      } yield MessageSummary(waiting, account, counts.toMap)
    }
  }

  private def count(value: String): Either[PackageError, MessageCounts] = {
    val read: Either[String, MessageCounts] = value match {
      case Counts(n, o, un, uo) =>
        // Digits only, but possibly too many for an Int.
        (
          n.toIntOption,
          o.toIntOption,
          Option(un).fold(Some(0))(_.toIntOption),
          Option(uo).fold(Some(0))(_.toIntOption)
        ).tupled
          .toRight(s"not a count: '$value'")
          .flatMap(MessageCounts.of.tupled)
      case _ => Left(s"not a count: '$value'")
    }
    read.leftMap(PackageError.Malformed(_))
  }
}
