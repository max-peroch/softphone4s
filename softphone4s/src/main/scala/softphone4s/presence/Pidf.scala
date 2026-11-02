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

  // ---- Presence to XML -----------------------------------------------------------------

  /** Whether `s` is made of characters XML 1.0 allows (§2.2). */
  private def validText(s: String): Boolean =
    s.codePoints.allMatch(c =>
      c == 0x9 || c == 0xa || c == 0xd || (c >= 0x20 && c <= 0xd7ff) ||
        (c >= 0xe000 && c <= 0xfffd) || (c >= 0x10000 && c <= 0x10ffff)
    )

  /** A name the `id` of a tuple can be: XML's `NCName`, which is what the
    * schema of PIDF asks of an `xs:ID`.
    */
  private def validId(s: String): Boolean =
    s.nonEmpty && isNCNameStart(s.head) && s.forall(isNCNameChar)

  private val Escapes: Map[Char, String] =
    Map('&' -> "amp", '<' -> "lt", '>' -> "gt", '"' -> "quot", '\'' -> "apos")

  /** The text as events, the markup characters as references. The renderer
    * writes what it is given as it is, so this is where it is made safe.
    */
  private def escaped(s: String): List[XmlTexty] =
    s.foldRight(List.empty[XmlTexty]) { (c, rest) =>
      Escapes
        .get(c)
        .fold {
          rest match {
            case XmlString(t, false) :: tail =>
              XmlString(c.toString + t, false) :: tail
            case _ => XmlString(c.toString, false) :: rest
          }
        }(name => XmlEntityRef(name) :: rest)
    }

  private def element(name: String, attributes: List[(String, String)])(
      inside: List[XmlEvent]
  ): List[XmlEvent] =
    StartTag(
      QName(name),
      attributes.map((n, v) => Attr(QName(n), escaped(v))),
      false
    ) :: inside ::: List(EndTag(QName(name)))

  private def leaf(name: String, value: String): List[XmlEvent] =
    element(name, Nil)(escaped(value))

  private def tupleEvents(t: PresenceTuple): List[XmlEvent] =
    element("tuple", List("id" -> t.id))(
      element("status", Nil)(
        t.basic.toList.flatMap(b => leaf("basic", b.toString.toLowerCase))
      ) ++
        t.contact.toList.flatMap(leaf("contact", _)) ++
        t.notes.flatMap(leaf("note", _))
    )

  def render(presence: Presence): Either[String, String] = {
    val texts = presence.entity :: presence.notes ::: presence.tuples.flatMap(
      t => t.id :: t.contact.toList ::: t.notes
    )
    for {
      _ <- Either.cond(presence.entity.nonEmpty, (), "an empty entity")
      _ <- presence.tuples
        .find(t => !validId(t.id))
        .fold(Right(()))(t => Left(s"not a tuple id: '${t.id}'"))
      _ <- Either.cond(texts.forall(validText), (), "a character XML lacks")
      events = XmlDecl("1.0", Some("UTF-8"), None) ::
        element(
          "presence",
          List("xmlns" -> Namespace, "entity" -> presence.entity)
        )(
          presence.tuples.flatMap(tupleEvents) ++ presence.notes.flatMap(
            leaf("note", _)
          )
        )
      xml <- Stream
        .emits(events)
        .covary[Fallible]
        .through(fs2.data.xml.render.raw[Fallible]())
        .compile
        .string
        .leftMap(_.getMessage)
    } yield xml
  }

  private def malformed(reason: String): PackageError =
    PackageError.Malformed(reason)

  // ---- XML to a tree ---------------------------------------------------------------

  /** The namespaces in force: by prefix, and with no prefix for the default. */
  private type Scope = Map[Option[String], String]

  /** The scope inside an element: its parent's, changed by what the element
    * declares (`xmlns="..."` and `xmlns:p="..."`; an empty one takes it away).
    */
  private def scopeOf(
      parent: Scope,
      attributes: List[(QName, String)]
  ): Scope =
    attributes.foldLeft(parent) {
      case (scope, (QName(None, "xmlns"), uri))    => declare(scope, None, uri)
      case (scope, (QName(Some("xmlns"), p), uri)) =>
        declare(scope, Some(p), uri)
      case (scope, _) => scope
    }

  private def declare(scope: Scope, prefix: Option[String], uri: String) =
    if uri.isEmpty then scope - prefix else scope + (prefix -> uri)

  private final case class Element(
      name: QName,
      namespace: Option[String],
      attributes: List[(QName, String)],
      children: List[Element],
      text: String
  ) {

    /** Whether this is the element `local` of PIDF, in whatever way the
      * document spells the namespace.
      */
    def is(local: String): Boolean =
      namespace.contains(Namespace) && name.local == local

    def attribute(local: String): Option[String] =
      attributes.collectFirst { case (QName(None, `local`), value) =>
        value
      }
  }

  /** An element being read: what it has so far, last first. */
  private final case class Open(
      name: QName,
      scope: Scope,
      attributes: List[(QName, String)],
      children: List[Element],
      text: List[String]
  ) {
    def close: Element =
      Element(
        name,
        scope.get(name.prefix),
        attributes,
        children.reverse,
        text.reverse.mkString.trim
      )
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
          .map { attrs =>
            val outer = reading._1.headOption.fold(Map.empty: Scope)(_.scope)
            val open  = Open(name, scopeOf(outer, attrs), attrs, Nil, Nil)
            (open :: reading._1, reading._2)
          }
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

  private def presence(root: Element): Either[String, Presence] =
    for {
      _ <- Either.cond(
        root.name.local == "presence",
        (),
        s"not a presence document: <${root.name.render}>"
      )
      _ <- Either.cond(
        root.is("presence"),
        (),
        s"not in the namespace $Namespace"
      )
      entity <- root.attribute("entity").toRight("no entity")
      tuples <- root.children.filter(_.is("tuple")).traverse(tuple)
    } yield Presence(
      entity,
      tuples,
      root.children.filter(_.is("note")).map(_.text)
    )

  private def tuple(element: Element): Either[String, PresenceTuple] =
    for {
      id     <- element.attribute("id").toRight("a tuple with no id")
      status <- element.children
        .find(_.is("status"))
        .toRight(s"tuple $id has no status")
      basic <- status.children
        .find(_.is("basic"))
        .traverse(b =>
          Availability.parse(b.text).toRight(s"basic: '${b.text}'")
        )
    } yield PresenceTuple(
      id,
      basic,
      element.children.find(_.is("contact")).map(_.text),
      element.children.filter(_.is("note")).map(_.text)
    )
}
