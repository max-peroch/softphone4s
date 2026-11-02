package softphone4s.transaction

import softphone4s.model.{Branch, CallId, SipMethod}

/** Which transaction a span is of. */
final case class TransactionInfo(
    side: TransactionSide,
    method: SipMethod,
    branch: Branch,
    callId: CallId
)
