package softphone4s.dialog

/** One end of a dialog, as the notifier names it: who, as a URI and a display
  * name, and where its requests go.
  */
final case class Party(
    identity: Option[String],
    display: Option[String],
    target: Option[String]
)
