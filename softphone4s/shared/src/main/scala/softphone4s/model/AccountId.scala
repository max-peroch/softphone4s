package softphone4s.model

/** Names one of the accounts a `Softphone` holds by its address-of-record, such
  * as `sip:alice@example.com`.
  */
final case class AccountId(addressOfRecord: SipUri)

object AccountId {
  extension (id: AccountId) {
    def render: String = id.addressOfRecord.render
  }
}
