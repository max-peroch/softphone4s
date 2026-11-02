package softphone4s.presence

import softphone4s.model.{AccountId, MediaType}
import softphone4s.publication.PublicationBody
import softphone4s.subscription.{EventPackage, PackageError}

/** The presence of an entity (RFC 3863): the tuples it is reachable by and
  * notes about it as a whole. No tuples means nothing is known.
  *
  * @param entity
  *   whose presence it is, as the document gives it (`pres:alice@example.com`).
  */
final case class Presence(
    entity: String,
    tuples: List[PresenceTuple],
    notes: List[String]
) {

  /** Whether any tuple is open. */
  def isOpen: Boolean = tuples.exists(_.basic.contains(Availability.Open))
}

object Presence {

  /** The presence of `account`, whose entity is its own address as a `pres:`
    * URI (`sip:alice@example.com` is `pres:alice@example.com`), which is what a
    * notifier expects of a PUBLISH to that address.
    */
  def forAccount(
      account: AccountId,
      tuples: List[PresenceTuple] = Nil,
      notes: List[String] = Nil
  ): Presence =
    Presence(
      "pres:" + account.addressOfRecord.render
        .stripPrefix("sips:")
        .stripPrefix("sip:"),
      tuples,
      notes
    )

  /** The `presence` package: subscribe with it, and read PIDF. */
  val Package: EventPackage[Presence] = Pidf.Package

  /** Our own presence as a PIDF document, for a PUBLISH. Refused if the entity
    * is empty, a tuple's id is not a name XML allows, or a text has a character
    * XML does not.
    */
  def render(presence: Presence): Either[String, String] =
    Pidf.render(presence)

  /** Our own presence as the body of a PUBLISH. */
  def document(presence: Presence): Either[String, PublicationBody] =
    render(presence).map(PublicationBody(MediaType.PidfXml, _))

  /** Reads a PIDF document (RFC 3863), whether it came in a NOTIFY or not. */
  def parse(document: String): Either[PackageError, Presence] =
    Pidf.parse(document)
}
