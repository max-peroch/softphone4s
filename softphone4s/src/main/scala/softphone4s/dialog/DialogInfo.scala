package softphone4s.dialog

import softphone4s.model.MediaType
import softphone4s.subscription.{
  EventHeader,
  EventPackage,
  Notification,
  PackageError
}

/** The state of the dialogs of an entity (RFC 4235): what a `dialog` NOTIFY
  * carries, and what busy-lamp fields and call-pickup are built on.
  *
  * @param entity
  *   whose dialogs they are, as the notifier names it.
  * @param version
  *   which notification this is, going up by one each time; it orders them.
  * @param coverage
  *   whether `dialogs` are all there are, or only those that changed.
  */
final case class DialogInfo(
    entity: String,
    version: Long,
    coverage: Coverage,
    dialogs: List[DialogEntry]
) {

  def dialog(id: String): Option[DialogEntry] = dialogs.find(_.id == id)

  /** The dialogs that have not ended. */
  def ongoing: List[DialogEntry] = dialogs.filterNot(_.isTerminated)

  /** Whether the entity is in a dialog that has not ended. */
  def isBusy: Boolean = ongoing.nonEmpty
}

object DialogInfo {

  /** The `dialog` package: subscribe with it, and read dialog-info. */
  val Package: EventPackage[DialogInfo] = new EventPackage[DialogInfo] {
    val event: EventHeader      = EventHeader.Dialog
    val accept: List[MediaType] = List(MediaType.DialogInfoXml)
    def decode(n: Notification): Either[PackageError, DialogInfo] =
      EventPackage.bodyOf(n, MediaType.DialogInfoXml).flatMap(parse)
  }

  /** Reads a dialog-info document (RFC 4235), outside a NOTIFY as well. */
  def parse(document: String): Either[PackageError, DialogInfo] =
    DialogInfoXml.parse(document)

  /** What is known once `update` has come, given what was known before: the
    * dialogs that have not ended, as of the version of `update`.
    *
    *   - A full update is the new state, whatever came before.
    *   - A partial one amends the state: its dialogs replace those of the same
    *     `id` (or are added), and one that has ended is dropped. It must be the
    *     next version; if not, or if nothing full was held to amend, a
    *     notification was missed and the result is a [[MissedUpdate]]: the
    *     state is not known until a full one comes.
    *   - One that is not newer than what is held is a repeat or came late, and
    *     changes nothing.
    *
    * Versions count within a subscription: when a subscription begins again, so
    * does the tracking, from `None`.
    */
  def track(
      known: Option[DialogInfo],
      update: DialogInfo
  ): Either[MissedUpdate, DialogInfo] =
    (known, update.coverage) match {
      case (Some(held), _) if update.version <= held.version => Right(held)
      case (_, Coverage.Full) => Right(complete(update))
      case (Some(held), Coverage.Partial)
          if update.version == held.version + 1 =>
        Right(amended(held, update))
      case (_, Coverage.Partial) =>
        Left(MissedUpdate(known.map(_.version), update.version))
    }

  private def complete(update: DialogInfo): DialogInfo =
    update.copy(dialogs = update.ongoing)

  /** Linear in the dialogs of both: a document of 128 KiB holds a few thousand,
    * and a table of them is not to be searched for each.
    */
  private def amended(held: DialogInfo, update: DialogInfo): DialogInfo = {
    val updates = update.dialogs.map(d => d.id -> d).toMap
    val heldIds = held.dialogs.iterator.map(_.id).toSet
    val kept    = held.dialogs.flatMap(old =>
      updates.getOrElse(old.id, old) match {
        case ended if ended.isTerminated => None
        case current                     => Some(current)
      }
    )
    val added = update.dialogs
      .filterNot(d => heldIds.contains(d.id))
      .map(d => updates(d.id))
      .distinctBy(_.id)
      .filterNot(_.isTerminated)
    DialogInfo(update.entity, update.version, Coverage.Full, kept ++ added)
  }
}
