package softphone4s.messaging

import softphone4s.model.{AccountId, NameAddr}

/** An instant message that arrived, and the account it was for. */
final case class IncomingMessage(
    account: AccountId,
    from: NameAddr,
    content: MessageContent
)
