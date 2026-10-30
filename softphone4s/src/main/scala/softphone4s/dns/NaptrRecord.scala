package softphone4s.dns

/** A naming authority pointer (RFC 3403), as RFC 3263 §4.1 uses it to say which
  * transports a domain's SIP service is offered over.
  *
  * @param flags
  *   `S` when `replacement` is the name of an SRV record.
  * @param service
  *   such as `SIP+D2U`, `SIP+D2T` or `SIPS+D2T`.
  * @param replacement
  *   the SRV name to look up next.
  */
final case class NaptrRecord(
    order: Int,
    preference: Int,
    flags: String,
    service: String,
    replacement: String
)
