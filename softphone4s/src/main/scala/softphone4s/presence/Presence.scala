package softphone4s.presence

import softphone4s.subscription.EventPackage

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

  /** The `presence` package: subscribe with it, and read PIDF. */
  val Package: EventPackage[Presence] = Pidf.Package
}
