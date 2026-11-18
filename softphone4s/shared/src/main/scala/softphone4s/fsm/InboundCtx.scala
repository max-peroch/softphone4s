package softphone4s.fsm

import softphone4s.codec.SdpCodec.AudioOffer
import softphone4s.model.*
import softphone4s.transaction.TransactionConfig

/** What an incoming call knows from its INVITE. */
final case class InboundCtx(
    invite: SipRequest,
    localTag: Tag,
    offer: AudioOffer,
    dialog: SipDialog,
    config: TransactionConfig
)
