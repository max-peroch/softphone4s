package softphone4s.presence

import cats.syntax.all.*
import fs2.data.xml.*
import fs2.data.xml.XmlEvent.*
import softphone4s.model.MediaType
import softphone4s.subscription.{
  EventHeader,
  EventPackage,
  Notification,
  PackageError
}
import softphone4s.xml.XmlTree
import softphone4s.xml.XmlTree.{Element, element, leaf, validText}

/** Reads and writes PIDF (RFC 3863), the body of a presence NOTIFY and of a
  * PUBLISH. The XML is read as [[softphone4s.xml.XmlTree]] does, which refuses
  * a DOCTYPE and any entity but the five XML defines. Elements are matched by
  * name under the namespace of the document, so extensions in other namespaces
  * are skipped.
  */
private[presence] object Pidf {

  val Namespace: String = "urn:ietf:params:xml:ns:pidf"

  val Package: EventPackage[Presence] = new EventPackage[Presence] {
    val event: EventHeader      = EventHeader.Presence
    val accept: List[MediaType] = List(MediaType.PidfXml)
    def decode(n: Notification): Either[PackageError, Presence] =
      EventPackage.bodyOf(n, MediaType.PidfXml).flatMap(parse)
  }

  def parse(document: String): Either[PackageError, Presence] =
    for {
      body  <- EventPackage.withinLimit(document)
      root  <- XmlTree.parse(body).leftMap(malformed)
      found <- presence(root).leftMap(malformed)
    } yield found

  // ---- Presence to XML -----------------------------------------------------------------

  /** A name the `id` of a tuple can be: XML's `NCName`, which is what the
    * schema of PIDF asks of an `xs:ID`.
    */
  private def validId(s: String): Boolean =
    s.nonEmpty && isNCNameStart(s.head) && s.forall(isNCNameChar)

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
      xml <- XmlTree.render(events)
    } yield xml
  }

  private def malformed(reason: String): PackageError =
    PackageError.Malformed(reason)

  extension (element: Element)

    /** Whether this is the element `local` of PIDF. */
    private def is(local: String): Boolean = element.is(Namespace, local)

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
