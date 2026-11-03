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

  /** The data model of RFC 4479, which holds the `person` that has activities.
    */
  val DataModel: String = "urn:ietf:params:xml:ns:pidf:data-model"

  /** Rich presence, RFC 4480. */
  val Rpid: String = "urn:ietf:params:xml:ns:pidf:rpid"

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

  /** The `person` that says what the entity is doing, with an `id` that is none
    * of the tuples' (an `xs:ID` is unique in its document); nothing if it is
    * doing nothing in particular.
    */
  private def personEvents(presence: Presence): List[XmlEvent] =
    if presence.activities.isEmpty then Nil
    else {
      val taken = presence.tuples.map(_.id).toSet
      val id    =
        (1 to taken.size + 1).map("p" + _).find(!taken(_)).getOrElse("p")
      element("dm:person", List("id" -> id))(
        element("rpid:activities", Nil)(presence.activities.flatMap {
          case Activity.Other(text) => leaf("rpid:other", text)
          case known                =>
            Activity
              .element(known)
              .toList
              .flatMap(name => element("rpid:" + name, Nil)(Nil))
        })
      )
    }

  def render(presence: Presence): Either[String, String] = {
    val others = presence.activities.collect { case Activity.Other(t) => t }
    val texts  = presence.entity :: presence.notes ::: others :::
      presence.tuples.flatMap(t => t.id :: t.contact.toList ::: t.notes)
    val namespaces =
      if presence.activities.isEmpty then Nil
      else List("xmlns:dm" -> DataModel, "xmlns:rpid" -> Rpid)
    for {
      _ <- Either.cond(presence.entity.nonEmpty, (), "an empty entity")
      _ <- Either.cond(
        others.forall(_.trim.nonEmpty),
        (),
        "an activity with no text"
      )
      _ <- presence.tuples
        .find(t => !validId(t.id))
        .fold(Right(()))(t => Left(s"not a tuple id: '${t.id}'"))
      _ <- Either.cond(texts.forall(validText), (), "a character XML lacks")
      events = XmlDecl("1.0", Some("UTF-8"), None) ::
        element(
          "presence",
          List("xmlns" -> Namespace, "entity" -> presence.entity) ++ namespaces
        )(
          presence.tuples.flatMap(tupleEvents) ++ presence.notes.flatMap(
            leaf("note", _)
          ) ++ personEvents(presence)
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
      root.children.filter(_.is("note")).map(_.text),
      activities(root)
    )

  /** What the persons of the document are doing: the children of their
    * `activities` that RPID names, in order. Elements of other namespaces, and
    * the notes of `activities`, are skipped.
    */
  private def activities(root: Element): List[Activity] =
    for {
      person <- root.children.filter(_.is(DataModel, "person"))
      listed <- person.children.filter(_.is(Rpid, "activities"))
      child  <- listed.children
      if child.namespace.contains(Rpid) && child.name.local != "note"
      activity <- Activity.fromElement(child.name.local, child.text).toList
    } yield activity

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
