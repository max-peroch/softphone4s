package softphone4s.presence

import cats.syntax.all.*
import fs2.data.xml.*
import fs2.data.xml.XmlEvent.*
import fs2.{Fallible, Stream}
import softphone4s.model.MediaType
import softphone4s.subscription.{
  EventHeader,
  EventPackage,
  Notification,
  PackageError
}

/** Reads PIDF (RFC 3863), the body of a presence NOTIFY.
  *
  * The XML is read by `fs2-data-xml`, in Scala and over a stream that cannot
  * suspend an effect, which fetches nothing: a document with a DOCTYPE is
  * refused, and so is one that uses an entity other than the five XML defines.
  * Elements are matched by name under the namespace of the document, so
  * extensions in other namespaces are skipped.
  */
private[presence] object Pidf {

  val Namespace: String = "urn:ietf:params:xml:ns:pidf"

  val Package: EventPackage[Presence] = new EventPackage[Presence] {
    val event: EventHeader      = EventHeader.Presence
    val accept: List[MediaType] = List(MediaType.PidfXml)
    def decode(n: Notification): Either[PackageError, Presence] =
      EventPackage.bodyOf(n, MediaType.PidfXml).flatMap(parse)
  }

  def parse(body: String): Either[PackageError, Presence] =
    for {
      events <- read(body).leftMap(e => malformed(e.getMessage))
      root   <- tree(events).leftMap(malformed)
      found  <- presence(root).leftMap(malformed)
    } yield found

  private def malformed(reason: String): PackageError =
    PackageError.Malformed(reason)

  // ---- XML to a tree ---------------------------------------------------------------

  private final case class Element(
      name: QName,
      attributes: List[(QName, String)],
      children: List[Element],
      text: String
  ) {
    def attribute(local: String): Option[String] =
      attributes.collectFirst { case (QName(None, `local`), value) =>
        value
      }
  }

  /** An element being read: what it has so far, last first. */
  private final case class Open(
      name: QName,
      attributes: List[(QName, String)],
      children: List[Element],
      text: List[String]
  ) {
    def close: Element =
      Element(name, attributes, children.reverse, text.reverse.mkString.trim)
  }

  private def read(body: String): Either[Throwable, List[XmlEvent]] =
    Stream
      .emit(body)
      .covary[Fallible]
      .through(events[Fallible, String]())
      .through(normalize[Fallible])
      .compile
      .toList

  private val Entities: Map[String, String] =
    Map("lt" -> "<", "gt" -> ">", "amp" -> "&", "apos" -> "'", "quot" -> "\"")

  /** The text of a reference: one of the five entities XML defines, or a
    * character by its number. Any other entity would have to be declared in a
    * DOCTYPE, which is refused.
    */
  private def resolved(text: XmlTexty): Either[String, String] =
    text match {
      case XmlString(s, _)    => Right(s)
      case XmlEntityRef(name) =>
        Entities
          .get(name)
          .toRight(s"an entity that is not defined: &$name;")
      case XmlCharRef(value) =>
        Either.cond(
          Character.isValidCodePoint(value),
          new String(Character.toChars(value)),
          s"not a character: $value"
        )
    }

  private type Reading = (List[Open], Option[Element])

  private def finished(
      done: Element,
      reading: Reading
  ): Either[String, Reading] =
    reading match {
      case (parent :: above, root) =>
        Right((parent.copy(children = done :: parent.children) :: above, root))
      case (Nil, None)    => Right((Nil, Some(done)))
      case (Nil, Some(_)) => Left("more than one document element")
    }

  private def step(reading: Reading, event: XmlEvent): Either[String, Reading] =
    (reading, event) match {
      // An element with nothing in it is given an end tag as well.
      case (_, StartTag(name, attributes, _)) =>
        attributes
          .traverse(a => a.value.traverse(resolved).map(a.name -> _.mkString))
          .map(attrs => (Open(name, attrs, Nil, Nil) :: reading._1, reading._2))
      case ((current :: above, root), EndTag(_)) =>
        finished(current.close, (above, root))
      case ((Nil, _), EndTag(_)) => Left("an end tag with nothing open")
      case ((current :: above, root), text: XmlTexty) =>
        resolved(text).map(s =>
          (current.copy(text = s :: current.text) :: above, root)
        )
      case (_, _: XmlTexty)         => Right(reading)
      case (_, XmlDoctype(_, _, _)) => Left("a DOCTYPE is not allowed")
      case _                        => Right(reading)
    }

  /** The document element, from the events. Nothing but elements and their text
    * is kept.
    */
  private def tree(events: List[XmlEvent]): Either[String, Element] =
    events
      .foldLeft[Either[String, Reading]](Right((Nil, None)))((reading, event) =>
        reading.flatMap(step(_, event))
      )
      .flatMap {
        case (Nil, Some(root)) => Right(root)
        case (Nil, None)       => Left("no document element")
        case _                 => Left("an element is not closed")
      }

  // ---- The tree to presence ----------------------------------------------------------

  /** The namespace a prefix, or no prefix, is declared to be on `root`. */
  private def declared(root: Element): Option[String] =
    root.attributes.collectFirst {
      case (QName(None, "xmlns"), uri) if root.name.prefix.isEmpty        => uri
      case (QName(Some("xmlns"), p), uri) if root.name.prefix.contains(p) => uri
    }

  private def presence(root: Element): Either[String, Presence] =
    for {
      _ <- Either.cond(
        root.name.local == "presence",
        (),
        s"not a presence document: <${root.name.render}>"
      )
      _ <- Either.cond(
        declared(root).contains(Namespace),
        (),
        s"not in the namespace $Namespace"
      )
      entity <- root.attribute("entity").toRight("no entity")
      prefix = root.name.prefix
      named  = (e: Element, local: String) =>
        e.name.prefix == prefix && e.name.local == local
      tuples <- root.children.filter(named(_, "tuple")).traverse(tuple(named))
    } yield Presence(
      entity,
      tuples,
      root.children.filter(named(_, "note")).map(_.text)
    )

  private def tuple(
      named: (Element, String) => Boolean
  )(element: Element): Either[String, PresenceTuple] =
    for {
      id     <- element.attribute("id").toRight("a tuple with no id")
      status <- element.children
        .find(named(_, "status"))
        .toRight(s"tuple $id has no status")
      basic <- status.children
        .find(named(_, "basic"))
        .traverse(b =>
          Availability.parse(b.text).toRight(s"basic: '${b.text}'")
        )
    } yield PresenceTuple(
      id,
      basic,
      element.children.find(named(_, "contact")).map(_.text),
      element.children.filter(named(_, "note")).map(_.text)
    )
}
