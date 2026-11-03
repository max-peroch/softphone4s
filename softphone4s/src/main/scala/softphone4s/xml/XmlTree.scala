package softphone4s.xml

import cats.syntax.all.*
import fs2.data.xml.*
import fs2.data.xml.XmlEvent.*
import fs2.{Fallible, Stream}

/** What the event packages that carry XML (PIDF, dialog-info) share: reading a
  * document as a tree of elements, and writing events as text.
  *
  * The XML is read by `fs2-data-xml`, in Scala and over a stream that cannot
  * suspend an effect, which fetches nothing: a document with a DOCTYPE is
  * refused, and so is one that uses an entity other than the five XML defines.
  * Elements keep the namespace they were written in, so extensions in other
  * namespaces can be skipped.
  */
private[softphone4s] object XmlTree {

  /** Whether `s` is made of characters XML 1.0 allows (§2.2). */
  def validText(s: String): Boolean =
    s.codePoints.allMatch(c =>
      c == 0x9 || c == 0xa || c == 0xd || (c >= 0x20 && c <= 0xd7ff) ||
        (c >= 0xe000 && c <= 0xfffd) || (c >= 0x10000 && c <= 0x10ffff)
    )

  private val Escapes: Map[Char, String] =
    Map('&' -> "amp", '<' -> "lt", '>' -> "gt", '"' -> "quot", '\'' -> "apos")

  /** The text as events, the markup characters as references. The renderer
    * writes what it is given as it is, so this is where it is made safe.
    */
  def escaped(s: String): List[XmlTexty] =
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

  def element(name: String, attributes: List[(String, String)])(
      inside: List[XmlEvent]
  ): List[XmlEvent] =
    StartTag(
      QName(name),
      attributes.map((n, v) => Attr(QName(n), escaped(v))),
      false
    ) :: inside ::: List(EndTag(QName(name)))

  def leaf(name: String, value: String): List[XmlEvent] =
    element(name, Nil)(escaped(value))

  // ---- XML to a tree ---------------------------------------------------------------

  /** The namespaces in force: by prefix, and with no prefix for the default. */
  type Scope = Map[Option[String], String]

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

  final case class Element(
      name: QName,
      namespace: Option[String],
      attributes: List[(QName, String)],
      children: List[Element],
      text: String
  ) {

    /** Whether this is the element `local` of `ns`, in whatever way the
      * document spells the namespace.
      */
    def is(ns: String, local: String): Boolean =
      namespace.contains(ns) && name.local == local

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

  /** The events as text. The renderer writes what it is given as it is, so text
    * must have been made safe with [[escaped]].
    */
  def render(events: List[XmlEvent]): Either[String, String] =
    Stream
      .emits(events)
      .covary[Fallible]
      .through(fs2.data.xml.render.raw[Fallible]())
      .compile
      .string
      .leftMap(_.getMessage)

  /** The document element of `body`, or why it cannot be read. */
  def parse(body: String): Either[String, Element] =
    read(body).leftMap(_.getMessage).flatMap(tree)
}
