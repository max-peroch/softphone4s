package softphone4s.dialog

/** The dialog this one replaces (RFC 3891), by its identifiers. */
final case class Replaces(
    callId: String,
    localTag: Option[String],
    remoteTag: Option[String]
)
