package softphone4s.logging

import softphone4s.metrics.CallDirection

/** The names of the fields that the stack's log lines carry, so that logs can
  * be searched and joined on them: every line of a call has its `call-id`, of a
  * registration its `account`, and so on. They are the keys of the context
  * (`StructuredLogger`) of each line.
  *
  *   - a call: `call-id`, `direction` (`outgoing` or `incoming`), `account`;
  *   - a registration: `account`;
  *   - a subscription: `account`, `event`, `target`;
  *   - a publication: `account`, `event`, `presentity`;
  *   - a transaction, where a line is about one: `branch`, `method`;
  *   - and as the line needs: `error`, `reason`, `status`, `cseq`, `address`,
  *     `transport`, `host`, and `side` (`client` or `server`) for a
  *     transaction.
  */
object LogFields {

  val CallId: String     = "call-id"
  val Direction: String  = "direction"
  val Account: String    = "account"
  val Branch: String     = "branch"
  val Side: String       = "side"
  val Method: String     = "method"
  val Status: String     = "status"
  val Cseq: String       = "cseq"
  val Error: String      = "error"
  val Reason: String     = "reason"
  val Address: String    = "address"
  val Transport: String  = "transport"
  val Host: String       = "host"
  val Event: String      = "event"
  val Target: String     = "target"
  val Presentity: String = "presentity"

  def direction(d: CallDirection): String =
    d match {
      case CallDirection.Outgoing => "outgoing"
      case CallDirection.Incoming => "incoming"
    }
}
