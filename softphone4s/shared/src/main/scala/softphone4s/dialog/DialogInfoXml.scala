package softphone4s.dialog

import cats.syntax.all.*
import softphone4s.subscription.{EventPackage, PackageError}
import softphone4s.xml.XmlTree
import softphone4s.xml.XmlTree.Element

import scala.concurrent.duration.{FiniteDuration, SECONDS}

/** Reads dialog-info (RFC 4235), the body of a `dialog` NOTIFY. The XML is read
  * as [[softphone4s.xml.XmlTree]] does, which refuses a DOCTYPE and any entity
  * but the five XML defines. Elements are matched by name under the namespace
  * of the document, so extensions in other namespaces are skipped, and so are
  * elements of this one that this does not read.
  */
private[dialog] object DialogInfoXml {

  val Namespace: String = "urn:ietf:params:xml:ns:dialog-info"

  /** The most seconds a duration can be and still be a `FiniteDuration`. */
  private val MaxSeconds: Long = Long.MaxValue / 1_000_000_000L

  def parse(document: String): Either[PackageError, DialogInfo] =
    for {
      body  <- EventPackage.withinLimit(document)
      root  <- XmlTree.parse(body).leftMap(malformed)
      found <- dialogInfo(root).leftMap(malformed)
    } yield found

  private def malformed(reason: String): PackageError =
    PackageError.Malformed(reason)

  extension (element: Element)
    private def is(local: String): Boolean =
      element.is(Namespace, local)

  private def dialogInfo(root: Element): Either[String, DialogInfo] =
    for {
      _ <- Either.cond(
        root.name.local == "dialog-info",
        (),
        s"not a dialog-info document: <${root.name.render}>"
      )
      _ <- Either.cond(
        root.is("dialog-info"),
        (),
        s"not in the namespace $Namespace"
      )
      entity <- root
        .attribute("entity")
        .filter(_.nonEmpty)
        .toRight("no entity")
      version <- root
        .attribute("version")
        .toRight("no version")
        .flatMap(v =>
          v.trim.toLongOption
            .filter(_ >= 0)
            .toRight(s"version: '$v'")
        )
      coverage <- root
        .attribute("state")
        .toRight("no state")
        .flatMap(s => Coverage.parse(s).toRight(s"state: '$s'"))
      dialogs <- root.children.filter(_.is("dialog")).traverse(entry)
    } yield DialogInfo(entity, version, coverage, dialogs)

  private def entry(element: Element): Either[String, DialogEntry] =
    for {
      id <- element
        .attribute("id")
        .filter(_.nonEmpty)
        .toRight("a dialog with no id")
      direction <- element
        .attribute("direction")
        .traverse(d => DialogDirection.parse(d).toRight(s"direction: '$d'"))
      stateElement <- element.children
        .find(_.is("state"))
        .toRight(s"dialog $id has no state")
      state <- DialogState
        .parse(stateElement.text)
        .toRight(s"state: '${stateElement.text}'")
      code <- stateElement
        .attribute("code")
        .traverse(c =>
          c.trim.toIntOption
            .filter(n => n >= 100 && n <= 699)
            .toRight(s"code: '$c'")
        )
      duration <- element.children
        .find(_.is("duration"))
        .traverse(d => seconds(d.text))
      replaces <- element.children.find(_.is("replaces")).traverse(replaced)
    } yield DialogEntry(
      id,
      element.attribute("call-id"),
      element.attribute("local-tag"),
      element.attribute("remote-tag"),
      direction,
      state,
      stateElement.attribute("event").flatMap(EndEvent.parse),
      code,
      duration,
      replaces,
      element.children.find(_.is("referred-by")).map(_.text).filter(_.nonEmpty),
      element.children.find(_.is("local")).map(party),
      element.children.find(_.is("remote")).map(party)
    )

  private def seconds(text: String): Either[String, FiniteDuration] =
    text.trim.toLongOption
      .filter(s => s >= 0 && s <= MaxSeconds)
      .map(FiniteDuration(_, SECONDS))
      .toRight(s"duration: '$text'")

  private def replaced(element: Element): Either[String, Replaces] =
    element
      .attribute("call-id")
      .filter(_.nonEmpty)
      .map(
        Replaces(
          _,
          element.attribute("local-tag"),
          element.attribute("remote-tag")
        )
      )
      .toRight("replaces with no call-id")

  private def party(element: Element): Party = {
    val identity = element.children.find(_.is("identity"))
    Party(
      identity.map(_.text).filter(_.nonEmpty),
      identity.flatMap(_.attribute("display")),
      element.children.find(_.is("target")).flatMap(_.attribute("uri"))
    )
  }
}
